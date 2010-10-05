#
#  BeanCounter.pm --- A stock portfolio performance monitoring toolkit
#
#  Copyright (C) 1998 - 2001  Dirk Eddelbuettel <edd@debian.org>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

#  $Id: BeanCounter.pm,v 1.20 2001/08/17 01:46:22 edd Exp $

package Finance::BeanCounter;

require strict;
require Exporter;

#use Carp;			# die with info on caller
use Data::Dumper;		# debugging aid
use Date::Manip;		# for date parsing
use DBI;			# for the Perl interface to the database
use English;			# friendlier variable names
use HTTP::Request::Common;	# grab data from Yahoo's web interface
use LWP::UserAgent;		# for data queries from http://quote.yahoo.com
use POSIX qw(strftime);		# for date formatting
use Statistics::Descriptive;	# simple statistical functions
use Text::ParseWords;		# parse .csv data more reliably

@ISA = qw(Exporter);		# make these symbols known
@EXPORT = qw(BeanCounterVersion
	     ConnectToDb
	     CloseDB
	     DatabaseDailyData
	     DatabaseHistoricalData
	     DatabaseInfoData
	     GetTodaysAndPreviousDates
	     GetCashData
	     GetConfig
	     GetDate
	     GetDateEU
	     GetDailyData
	     GetFXData
	     GetHistoricalData 
	     GetPriceData
	     GetRiskData
	     ParseDailyData 
	     ParseNumeric 
	     PrintHistoricalData
	     ReportDailyData
	     Sign
	     UpdateDatabase
	     UpdateFXDatabase
	    );
@EXPORT_OK = qw( );
%EXPORT_TAGS = (all => [@EXPORT_OK]);

my $VERSION = sprintf("%d.%d", q$Revision: 1.20 $ =~ /(\d+)\.(\d+)/); 

my %Config;			# local copy of configuration hash

sub BeanCounterVersion {
  return $VERSION;
}

sub CloseDB {
  my $dbh = shift;
  $dbh->disconnect;
}


sub ConnectToDb {		# log us into the database (PostgreSQL)
  my $dbh = undef;

  if ($Config{odbc}) {
    $dbh = DBI->connect("dbi:ODBC:$Config{dsn}",
			$Config{user}, $Config{passwd}, 
			{ PrintError => 0, Warn => 1, AutoCommit => 0 });
  } else {
    $dbh = DBI->connect("dbi:Pg:dbname=beancounter;host=$Config{host}", 
			$Config{user}, $Config{passwd},
			{ PrintError => 0, Warn => 1, AutoCommit => 0 });
  }
  die "No luck with database connection" unless ($dbh);

  return $dbh;
}


sub GetTodaysAndPreviousDates {
  my ($date, $prev_date);
  my $today = DateCalc(ParseDate("today"), "- 8 hours");

  # Depending on whether today is a working day, use today 
  # or the most recent preceding working day
  if (Date_IsWorkDay($today)) {
    $date = UnixDate($today, "%Y%m%d");
    $prev_date = UnixDate(DateCalc($today, "- 1 business days"), "%Y%m%d");
  } else {
    $date = UnixDate(DateCalc($today, "- 1 business days"), "%Y%m%d");
    $prev_date = UnixDate(DateCalc($today, "- 2 business days"), "%Y%m%d");
  }
  # override with optional dates, if supplied
  $date      = UnixDate(ParseDate($main::datearg),    "%Y%m%d") 
    if ($main::datearg); 
  $prev_date = UnixDate(ParseDate($main::prevdatearg),"%Y%m%d") 
    if ($main::prevdatearg); 

  # and create 'prettier' non-ISO 8601 form
  my $pretty_date = UnixDate(ParseDate($date), "%d %b %Y");
  my $pretty_prev_date = UnixDate(ParseDate($prev_date), "%d %b %Y");

  return ($date, $prev_date, $pretty_date, $pretty_prev_date);
}


sub GetConfig {
  my ($file, $debug, $verbose, $fx, $extrafx, $updatedate, $command) = @_;

  %Config = ();			# reset hash

  $Config{debug} = $debug;	# no debugging as default
  $Config{verbose} = $verbose;	# silent == non-verbose as default

  $Config{odbc} = 0;		# if 1, use DBI-ODBC, else use DBI-Pg

  $Config{currency} = "USD";	# default to US dollars as domestic currency

  $Config{user} = $ENV{USER};	# default user is current user
  $Config{passwd} = undef;	# default password is no password

  $Config{today} = strftime("%Y%m%d", localtime);
  ($Config{lastbizday}, $Config{prevbizday}) = GetTodaysAndPreviousDates;

  # DSN name for ODBC
  $Config{dsn} = "beancounter";	# default ODBC data source name

  # host is needed only for the DBI-Pg interface
  $Config{host} = "localhost";	# default to local machine

  unless ( -f $file ) {
    warn "Config file $file not found, ignored.";
    return %Config;
  }

  open (FILE, "<$file") or die "Cannot open $file: $!\n";
  while (<FILE>) {
    next if (m/(\#|%)/);	# ignore comments, if any
    next if (m/^\s*$/);		# ignore empty lines, if any
    if (m/^\s*(\w+)\s*=\s*(.+)\s*$/) {
      $Config{$1} = "$2";
    }
  }
  close(FILE);

  $Config{currency} = $fx if defined($fx);

  if (defined($extrafx)) {
    unless ($command =~ /^(update|dailyjob)$/) {
      warn "Warning: --extrafx ignored as not updating db\n";
    } else {
      $Config{extrafx} = $extrafx if defined($extrafx);
    }
  }

  if (defined($updatedate)) {	# test the updatedate argument 
    unless ($command =~ /^(update|dailyjob)$/) {
      warn "Warning: --updatedate ignored as not updating db\n";
    } else {
      die "Error: Invalid date $updatedate for --forceupdate"
	unless (ParseDate($updatedate));
      $Config{updatedate} =  UnixDate(ParseDate($updatedate),"%Y%m%d");
    }
  }

  print Dumper(\%Config) if $Config{debug};
  return %Config;
}

sub GetCashData {
  my ($dbh, $date, $res) = @_;

  my ($stmt, $sth, $rv, $ary_ref, $sym_ref, %cash);
  my ($name, $value, $fx, $cost);
  # get the symbols
  $stmt  = "select name, value, currency, cost from cash ";
  $stmt .= "where $res " if (defined($res));
  $stmt .= "order by name";
  $sth = $dbh->prepare($stmt);
  $rv = $sth->execute(); 	# run query for report end date
  while (($name, $value, $fx, $cost) = $sth->fetchrow_array) {
    $cash{$name}{value} += $value; # adds if there are several
    $cash{$name}{fx} = $fx;
    $cash{$name}{cost} = $cost;
    $dbh->commit();		# the ODBC driver needs that for a weird reason
  }
  return(\%cash);
}

sub GetDailyData {		# use Finance::YahooQuote::getquote
  my @Args = @_;
  # This uses the 'return an entire array' approach of Finance::YahooQuote.
  my (@NA,@NAX,@EU,@UK,@SG,@Res);	# arrays of symbols by server, results
  my $na = "N/A";
  foreach $ARG (@Args) {		# sort stock symbol
    if (IsAsiaAustraliaNZ($ARG)) {	# if it's Asian, Australian or NZ
      push @SG, $ARG;	 
    } elsif (IsBritish($ARG)) {	# or if it is from London
      push @UK, $ARG;	
    } elsif (IsUSCanadaOption($ARG)) {
      push @NAX, $ARG;		# or if it is Europe
    } elsif (IsNonUSCanada($ARG)) {
      push @EU, $ARG;		# or if it is Europe
    } else {
      push @NA, $ARG;		# else use the default: North America
    }
  }

  # North America (i.e. NYSE, Nasdaq, AMEX, TSE, CDNX, non-cross FX rates)
  # in: name,symbol,price,last date (m/d/y),time,change,percent,volume,avg vol,
  #     bid, ask, previous,open,day range,52 week range,eps,p/e,div,divyld, cap
  if ($#NA > -1) {		# if there are stocks for Yahoo! North America
    my $url = "http://quote.yahoo.com/d" .
      "?f=snl1d1t1c1p2va2bapomwerr1dyj1x&s=";
    my $array = GetQuote($url,@NA); # get all North American quotes
    push @Res, (@$array);	# and store the entire array of arrays 
  }

  # North American option 
  # in: name,symbol,price,last date (m/d/y),time,change,percent,volume,avg vol,
  #     bid, ask, previous,open,day range,52 week range,eps,p/e,div,divyld, cap
  if ($#NAX > -1) {		# if there are stocks for Yahoo! North America
    my $url = "http://finance.yahoo.com/d" .
      "?f=snl1d1t1c1p2vbapom&s="; 
    my $array = GetQuote($url,@NAX); # get all North American quotes
    foreach my $r (@$array) {	# loop over all returned symbols
      my @arr = [ @$r[0..7],	# symb,name,last,date,time,chg,pcchg,v
		  $na, @$r[8..12], # avgvol, bid, ask, previous, open, dayrange
		  "$na - $na",	# 52wkrange, eps, pe, div, divyld, cap
		  $na, $na, $na, 
		  $na, $na, $na, 
		  "Option"];
      push @Res, @arr;		# and store the entire array of arrays 
    }
  }


  # UK quotes
  if ($#UK > -1) { 		# if there are stocks for Yahoo! UK
    my $url = "http://uk.finance.yahoo.com/d/quotes.csv?" .
      "&f=snl1d1t1c1p2vpoghx&s=" ;
    my $array = GetQuote($url,@UK);
    foreach my $r (@$array) {	# loop over all returned symbols
      ($r->[12]) = ($r->[12] =~ m/<small>(.*)<\/small>/); # get exchange
      my @arr = [ @$r[0..7], $na, $na, $na, @$r[8..9],
		 "$r->[10] - $r->[11]", "$na - $na", $na, $na, $na, 
		 $na, $na, $na, "$r->[12]"];
      push @Res, @arr;		# and return arranges just as NA
    } 
  }

  # (Continental) European quotes
  if ($#EU > -1) { 		# if there are stocks for Yahoo! UK
    ## my $url = "http://finanzen.de.yahoo.com/d/quotes.csv" . 
    my $url = "http://de.finance.yahoo.com/d/quotes.csv" . 
      "?f=snl1d1t1c1p2vpoghx&s=" ; 
    my $array = GetQuote($url, @EU);
    foreach my $r (@$array) {
      # we retrieve: symbol, name, close, date, time, change, percent change
      #              volume, avg vol, previous, open, low, high, exchange
      my @arr = [ @$r[0..7], $na, $na, $na, @$r[8..9],
		 "$r->[10] - $r->[11]", "$na - $na", $na, $na, $na, 
		 $na, $na, $na, "$r->[12]"];
      push @Res, @arr;
    } 
  }

  # Asia: Singapore, HongKong, Australia, New Zealand, ...
  if ($#SG > -1) { 		# if there are stocks for Yahoo! SG
    my $URL = "http://sg.finance.yahoo.com/d/quotes.csv" .
     "?f=snl1d1t1c1p2vbapomwerr1dyx&s=" ;
    my $array = GetQuote($URL,@SG); # Singapore quotes
    for my $r (@$array) {
      # symbol, name, price, date, time, change, %change      
      # vol, avg vol, bid, ask, previous, open, day range,year range,
      # eps,p/e,div date,div,yld, exchange -- avg vol and market cap missing
      my @arr = [ @$r[0..7], $na, @$r[8..18], $na, $r->[19]  ];
      push @Res, @arr;
    }
  } 

  print Dumper(\@Res) if $Config{debug};
  return @Res;
}


# map between ISO country codes and Yahoo symbols for the Philly exchange
sub GetFXMaps {
  my %iso2yahoo = (
		   "AUD" => "^XAD",
		   "CAD" => "^XCD",
		   "CHF" => "^XSF",
		   "EUR" => "^XEU",
		   "GBP" => "^XBP",
		   "JPY" => "^XJY",
		   "USD" => "----",
		   "DEM" => "^XDM"
	      );
  my %yahoo2iso = (
		   "^XAD" => "AUD",
		   "^XCD" => "CAD",
		   "^XSF" => "CHF",
		   "^XEU" => "EUR",
		   "^XBP" => "GBP",
		   "^XJY" => "JPY",
		   "----" => "USD",
		   "^XDM" => "DEM"
	      );
  return (\%iso2yahoo, \%yahoo2iso);
}


sub GetHistoricalData {		# get a batch of historical quotes from Yahoo!
  my ($symbol,$from,$to) = @_;
  my $ua = RequestAgent->new;
  $ua->env_proxy;		# proxy settings from *_proxy env. variables.
  $ua->proxy('http', $Config{proxy}) if $Config{proxy};  # or config vars
  my ($a,$b,$c,$d,$e,$f);	# we need the date as yy, mm and dd
  ($c,$a,$b) = ($from =~ m/\d\d(\d\d)(\d\d)(\d\d)/);
  ($f,$d,$e) = ($to =~ m/\d\d(\d\d)(\d\d)(\d\d)/);
  # Create a request for symbol from 19880101 to 19990114
  my $req = new HTTP::Request GET => "http://chart.yahoo.com/table.csv?" .
    "s=$symbol&a=$a&b=$b&c=$c&d=$d&e=$e&f=$f&g=d&q=q&y=0&z=$symbol&x=.csv";
  my $res = $ua->request($req);  # Pass request to user agent and get response
  if ($res->is_success) {	# Check the outcome of the response
    return split(/\n/, $res->content);
  } else {
    die "No luck with symbol $symbol\n";
  }
}

sub GetPriceData {
  my ($dbh, $date, $res) = @_;
  my ($stmt, $sth, $rv, $ary_ref, @symbols, %dates);
  my ($ra, $symbol, $name, $shares, $currency, $price, $prevprice,
      %prices, %prev_prices, %shares, %fx, %name, %purchdate, %cost,
      $cost,$pdate,%pricedate);

  # get the symbols
  $stmt  = "select distinct symbol from portfolio ";
  $stmt .= "where $res " if (defined($res));
  $stmt .= "order by symbol";
  $dbh->commit();		# sometime get DB error here if not reset
  @symbols = @{ $dbh->selectcol_arrayref($stmt) };
  # for each symbol, get most recent date subject to supplied date
  $stmt  = qq{select max(date) 
	      from stockprices 
	      where symbol = ? 
	      and day_close > 0
	      and date <= ?
	     };
  $sth = $dbh->prepare($stmt);
  foreach $ra (@symbols) {	
    $rv = $sth->execute($ra, $date); # run query for report end date
    my $res = $sth->fetchrow_array;
    $dates{$ra} = $res;
    $dbh->commit();		# the ODBC driver needs that for a weird reason
  }

  # now get closing price etc at date
  $stmt =    qq{select i.symbol, i.name, p.shares, p.currency, 
		       d.day_close, p.cost, p.date, d.previous_close
		from stockinfo i, portfolio p, stockprices d 
		where d.symbol = p.symbol 
		and i.symbol = d.symbol  
		and d.date = ?
		and d.symbol = ?
	       };
  $stmt .= qq{and d.symbol in
	      (select distinct symbol from portfolio where $res)
	     }   if (defined($res));
  $sth = $dbh->prepare($stmt);
  my $i = 0;
  foreach $ra (@symbols) {		
    $rv = $sth->execute($dates{$ra}, $ra); 
    while (($symbol, $name, $shares, $currency, $price, 
	    $cost, $pdate, $prevprice) = $sth->fetchrow_array) {
      print join " ", ($symbol, $name, $shares, $currency, $price, 
		       $cost, $pdate, $prevprice), "\n" if $Config{debug};
      $fx{$name} = $currency;	
      $prices{$name} = $price;
      $pricedate{$name} = $dates{$symbol};
      $cost{$name} = $cost;
      $purchdate{$name} = $pdate;
      $prev_prices{$name} = $prevprice;
      $name .= ":$i";
      $i++;
      $shares{$name} = $shares;
    }
  }

  print Dumper(\%prices) if $Config{debug};
  print Dumper(\%prev_prices)  if $Config{debug};
  return (\%fx, \%prices, \%prev_prices, \%shares, \%pricedate, 
	  \%cost, \%purchdate);
}

sub GetFXData {
  my ($dbh, $date, $fx) = @_;
  my $stmt = qq{ select day_close, previous_close
		 from fxprices 
		 where date = ?
		 and currency = ?
	       };
  my $sth = $dbh->prepare($stmt);
  my (%fx_prices,%prev_fx_prices);
  foreach my $fxval (sort values %$fx) {
    if ($fxval eq "USD") {	
      $fx_prices{$fxval} = 1.0;
      $prev_fx_prices{$fxval} = 1.0;
    } else {
      $sth->execute($date,$fxval);	# run query for FX cross
      my ($val,$prevval) = $sth->fetchrow_array
	or die "Could not fetch $fxval for $date";
      $fx_prices{$fxval} = $val;
      $prev_fx_prices{$fxval} = $prevval;
      my $ary_ref = $sth->fetchall_arrayref;
    }
  }

  return (\%fx_prices, \%prev_fx_prices);
}


sub GetQuote {			# taken from Dj's Finance::YahooQuote
  my ($URL,@symbols) = @_;	# and modified to allow for different URL
  my($x,@q,@qr,$ua,$url);	# and the simple filtering below as well
  $x = $";			# the firewall code below
  $" = "+";
  $url = $URL."@symbols";
  $" = $x;
  $ua = RequestAgent->new;
  # Load proxy settings from *_proxy environment variables.
  $ua->env_proxy;
  # or use the proxy specified as an option
  $ua->proxy('http', $Config{proxy}) if $Config{proxy};
  ## $ua->timeout($timeout);	# timeout after this many secs
  undef @qr;
  foreach (split('\n',$ua->request(GET $url)->content)) {
    next if m/^\"SYMBOL\",\"PRICE\"/; # Yahoo! UK sends headers
    next if m/index.html/;	# try csv mode at Yahoo! UK to see this bug
    $ARG =~ s/\r$//;		# kill DOS-ish end-of-line character
    if (tr/;// >= 2) {		# with at least 2 ';', suspect European quote
      $ARG =~ s/,/\./g;		# so harmonize it -- . as decimal sep.
      $ARG =~ s/;/,/g;		# and ; as field sep
    }
    @q = quotewords(',', 0, $ARG);
    push(@qr,[@q]);
  }
  return \@qr;
}				


sub GetRiskData {
  my ($dbh,$date,$prevdate,$res,$fx_prices,$crit) = @_;

  # get the symbols
  my $stmt  = qq{select distinct p.symbol, i.name
		 from portfolio p, stockinfo i
		 where p.symbol = i.symbol };
  $stmt .= qq{and p.symbol in
	      (select distinct symbol from portfolio where $res)
	     }   if (defined($res));
  $stmt .= "order by symbol";
  my $sth = $dbh->prepare($stmt);
  my $rv = $sth->execute(); 	# run query for report end date
  my $sref = $sth->fetchall_arrayref;

  # compute volatility
  $stmt  = qq{select day_close 
	      from stockprices 
	      where symbol = ? 
	      and date <= ? 
	      and date >= ?
	      and day_close > 0
	      order by date
	     };
  $sth = $dbh->prepare($stmt);
  my (%vol, %quintile);
  foreach my $ra (@$sref) {
    $rv = $sth->execute($ra->[0], $date, $prevdate);
    my $dref = $sth->fetchall_arrayref;	# get data
    my $x = Statistics::Descriptive::Full->new();
    for (my $i=1; $i<scalar(@{$dref}); $i++) { # add returns
      $x->add_data($dref->[$i][0]/$dref->[$i-1][0] - 1);
    }
    printf("%16s: stdev %6.2f min %6.2f max %6.2f\n",
	   $ra->[1], $x->standard_deviation, $x->min, $x->max)
      if $Config{debug};
    $vol{$ra->[1]} = $x->standard_deviation;
    if ($x->count() < 100) {
      print "$ra->[1]: Only ", $x->count(), " data points, ",
      	"need at least 100 for percentile calculation\n" if $Config{debug};
      $quintile{$ra->[1]} = undef;
    } else {
      $quintile{$ra->[1]} = $x->percentile(1);
    }
  }

  # compute correlations via OLS regression
  $stmt  = qq{select a.day_close, b.day_close 
	      from stockprices a, stockprices b
	      where a.symbol = ? and b.symbol = ? 
	      and a.date <= ? and a.date >= ?
	      and a.date = b.date
	      and a.day_close != 0 
	      and b.day_close != 0 
	      order by a.date
	     };
  $sth = $dbh->prepare($stmt);
  my %cor;
  foreach my $ra (@$sref) {		
    foreach my $rb (@$sref) {
      my $res = $ra->[0] cmp $rb->[0];
      if ($res < 0) {
	$rv = $sth->execute($ra->[0], $rb->[0], $date, $prevdate);
	my $dref = $sth->fetchall_arrayref;	# get data
	my $x = Statistics::Descriptive::Full->new();
	my $y = Statistics::Descriptive::Full->new();
	for (my $i=1; $i<scalar(@{$dref}); $i++) { # add returns
	  $x->add_data($dref->[$i][0]/$dref->[$i-1][0] - 1);
	  $y->add_data($dref->[$i][1]/$dref->[$i-1][1] - 1);
	}
	my @arr = $x->least_squares_fit($y->get_data());
	my $rho = $arr[2];
	unless (defined($rho)) {
	  warn "No computable correlation between $ra->[1] and $rb->[1];"
	    . " set to 0\n";
	  $rho = 0;
	}
	$cor{$ra->[1]}{$rb->[1]} = $rho;
	printf("%6s %6s correlation %6.4f\n", 
	       $ra->[1], $rb->[1], $arr[2]) if $Config{debug}; 
      } elsif ($res > 0) {
	$cor{$ra->[1]}{$rb->[1]} = $cor{$rb->[1]}{$ra->[1]};
      } else {
	$cor{$ra->[1]}{$rb->[1]} = 1;
      }
    }
  }

  # for each symbol, get most recent date subject to supplied date
  my %maxdate;
  $stmt  = qq{select max(date) 
	      from stockprices 
	      where symbol = ? 
	      and date <= ?
	     };
  $sth = $dbh->prepare($stmt);
  foreach my $ra (@$sref) {		
    $rv = $sth->execute($ra->[0], $date); # run query for report end date
    my $res = $sth->fetchrow_array;
    $maxdate{$ra->[1]} = $res;
  }

  # get position values
  my (%pos, $possum);
  $stmt =    qq{select p.shares, d.day_close, p.currency
 		from portfolio p, stockprices d, stockinfo i
 		where d.symbol = p.symbol 
 		and d.symbol = i.symbol 
 		and d.date = ?
 		and d.symbol = ?
 	       };
  $stmt .= qq{and d.symbol in
	      (select distinct symbol from portfolio where $res)
	     }   if (defined($res));
  $sth = $dbh->prepare($stmt);
  foreach my $ra (@$sref) {		
    $rv = $sth->execute($maxdate{$ra->[1]}, $ra->[0]); 
    while (my ($shares, $price, $fx) = $sth->fetchrow_array) {
      print "$ra->[1] $shares $price\n" if $Config{debug};
      my $amount = $shares * $price *
	$fx_prices->{$fx} / $fx_prices->{$Config{currency}};
      $pos{$ra->[1]} += $amount;
    }
  }

  # aggregate risk: 
  # VaR is z_crit * sqrt(horizon) * sqrt (X.transpose * Sigma * X)
  # where X is position value vector and Sigma the covariance matrix
  # given that Perl is not exactly a language for matrix calculus (as
  # eg GNU Octave), we flatten the computation into a double loop
  my $sum = 0;
  foreach my $pkey (keys %pos) {
    foreach my $vkey (keys %vol) { 
      $sum += $pos{$pkey} * $pos{$vkey} * $vol{$vkey} * $vol{$pkey} 
	* $cor{$vkey}{$pkey};
    }
  }
  my $var = $crit * sqrt($sum);


  ## marginal var
  my %margvar;
  foreach my $outer (keys %pos) {
    my $saved = $pos{$outer};
    my $sum = 0;
    $pos{$outer} = 0;
    foreach my $pkey (keys %pos) {
      foreach my $vkey (keys %vol) { 
	$sum += $pos{$pkey} * $pos{$vkey} * $vol{$vkey} * $vol{$pkey} 
	  * $cor{$vkey}{$pkey};
      }
    }
    $margvar{$outer} = $crit * sqrt($sum) - $var;
    $pos{$outer} = $saved;
  }

  return ($var, \%pos, \%vol, \%quintile, \%margvar);
}

sub DatabaseDailyData {		# a row to the dailydata table
  my ($dbh, %hash) = @_;
  foreach my $key (keys %hash) { # now split these into reference to the arrays
    print "$hash{$key}{symbol} " if $Config{verbose};

    my $cmd = "insert into stockprices values (" . 
              "'$hash{$key}{symbol}'," . 
	      "'$hash{$key}{date}'," .
	      "$hash{$key}{previous_close}," .
	      "$hash{$key}{open}," .
	      "$hash{$key}{day_low}," .
	      "$hash{$key}{day_high}," .
	      "$hash{$key}{close}," .
	      "$hash{$key}{change}," .
	      "$hash{$key}{bid}," .
	      "$hash{$key}{ask}," .
	      "$hash{$key}{volume})";
    $cmd =~ s|'?N/A'?|null|g;	# convert (textual) "N/A" into (database) null 
    print "$cmd\n" if $Config{debug};
    print "$hash{$key}{symbol} " if $Config{verbose};
    $dbh->do($cmd) or warn "\nFailed for $hash{$key}{symbol} with $cmd\n";
    $dbh->commit();
  }
}


sub DatabaseFXDailyData {
  my ($dbh, %hash) = @_;
  my $stmt = qq{insert into fxprices values (?, ?, ?, ?, ?, ?, ?, ?);};
  my $sth = $dbh->prepare($stmt);
  my ($iso2yahoo,$yahoo2iso) = GetFXMaps;
  foreach my $key (keys %hash) { # now split these into reference to the arrays
    my $fx = $yahoo2iso->{$hash{$key}{symbol}};
    print "$fx ($hash{$key}{symbol})  " if $Config{debug};
    $sth->execute($fx, 
		  $hash{$key}{date}, 
		  $hash{$key}{previous_close},
		  $hash{$key}{open},
		  $hash{$key}{day_low},
		  $hash{$key}{day_high},
		  $hash{$key}{close},
		  $hash{$key}{change}
		 )
      or warn "\nFailed for $fx at $hash{$key}{date}";
    $dbh->commit();
  }
}


sub DatabaseHistoricalData {
  my ($dbh, $symbol, @res) = @_;
  my $checked = 0;
  foreach $ARG (@res) {
    # we better make sure that the first line of data is correct 
    # so that we don't insert garbage
    if ($checked==0 and m/Date(,Open,High,Low)?,Close(,Volume)?/) {
      $checked = tr/,//;
      print "Checked now $checked\n" if $Config{verbose};
    } elsif ($checked) {
      my ($date, $open, $high, $low, $close, $volume, $cmd);
      if ($checked eq 5) {	# indices have no volume
	($date, $open, $high, $low, $close, $volume) = split(/\,/, $ARG);
	$cmd = "insert into stockprices " . 
         "(symbol, date, day_open, day_high, day_low, day_close, volume) " .
	 "values ('$symbol', '$date', $open, $high, $low, $close, $volume);";
      } elsif ($checked eq 1) {	# only close for mutual funds
	($date, $close) = split(/\,/, $ARG);
	$cmd = "insert into stockprices " . 
	  "(symbol, date, day_open, day_high, day_low, day_close, volume) " .
	    "values ('$symbol', '$date', null, null, null, $close, null);";
      } else {			# no volume for indices
	($date, $open, $high, $low, $close) = split(/\,/, $ARG);
	$cmd = "insert into stockprices " . 
	  "(symbol, date, day_open, day_high, day_low, day_close, volume) " .
	    "values ('$symbol', '$date', $open, $high, $low, $close, null);";
      }
      print "$cmd\n" if $Config{debug};
      $dbh->do($cmd) or die $dbh->errstr;
      $dbh->commit();
    } else {
      ;				# do nothing with bad data
    }

  }
  print "Done with $symbol\n" if $Config{verbose};
}


sub DatabaseInfoData {		# update a row in the info table
  my ($dbh, %hash) = @_;
  foreach my $key (keys %hash) { # now split these into reference to the arrays
    my $cmd = "insert into stockinfo values (" . 
              "'$hash{$key}{symbol}'," .
	      "'$hash{$key}{name}', " .
	      "'$hash{$key}{exchange}', " .
              "$hash{$key}{market_capitalisation}," .
              "$hash{$key}{'52_week_low'}," .
  	      "$hash{$key}{'52_week_high'}," .
	      "$hash{$key}{earnings_per_share}," .
	      "$hash{$key}{dividend_per_share}," .
	      "$hash{$key}{price_earnings_ratio}," .
	      "$hash{$key}{average_volume})";
    $cmd =~ s|'?N/A'?|null|g;	# convert (textual) "N/A" into (database) null 
    print "$cmd\n" if $Config{debug};
    print "$hash{$key}{symbol} " if $Config{verbose};
    $dbh->do($cmd) or die $dbh->errstr;
    $dbh->commit();
  }
}


sub GetDate {			# date can be "4:01PM" (same day) or "Jan 15"
  my ($value) = @_;		# Date::Manip knows how to deal with them...
  return UnixDate(ParseDate($value), "%Y%m%d");
}


sub GetDateEU {			# date in day/month/year format 
  my ($v) = @_;		
  if ($v =~ m/\d?\d:\d\d/) {
    return GetDate($v);
  } else {
    my ($d,$m,$y);
    ($d,$m,$y) = split(/\//, $v); # split on /
    return GetDate("$m/$d/$y");	# and analyse reordered
  }
}


sub IsAsiaAustraliaNZ {		# test if stock is Asia/Australia/NZ
  my $arg = shift;
  if ($arg =~ m/\.(\w+)$/ and ($1 =~ /^(SI|KL|JK|HK|TW|NS|KS|AX|NZ)$/)) {
    return 1;			# true if there is an exchange symbol 
  } else {			# and it is not Asia, Australia or New Zealand
    return 0;
  }
}


sub IsBritish {			# test if stock is from London-US or Canadian
  my $arg = shift;
  if ($arg =~ m/\.(\w+)$/ and ($1 =~ m/^L$/)) {
    return 1;			# true if there is an exchange symbol 
  } else {			# and it is London (.L)
    return 0;
  }
}

sub IsUSCanadaOption {		# test if option on US or Canadian stock
  my $arg = shift;		
  if ($arg =~ m/\.(\w+)$/ and ($1 =~ m/^X$/)) {
    return 1;			# true if there is an exchange symbol 
  } else {			# and it has option extension .X
    return 0;
  }
}

sub IsNonUSCanada {		# test if stock is non-US or Canadian
  my $arg = shift;		# or OTC Bulletin Board ot Options
  if ($arg =~ m/\.(\w+)$/ and ($1 !~ m/^(TO|V|M|OBX)$/)) {
    return 1;			# true if there is an exchange symbol 
  } else {			# and it is not Toronto/Vancouver/Montreal
    return 0;
  }
}


sub ParseDailyData {		# stuff the output into the hash
  my @rra = @_;			# we receive an array with references to arrays
  my %hash;			# we return a hash of hashes

  foreach my $ra (@rra) {	# now split these into reference to the arrays
    my $key = $ra->[0];
    $hash{$key}{symbol}         = $ra->[0];
    $hash{$key}{name}           = RemoveTrailingSpace($ra->[1]);
    $hash{$key}{close}          = ParseNumeric($ra->[2]);
    $hash{$key}{date}           = GetDate($ra->[3]);
    warn "Unparseable date for $key\n" unless $hash{$key}{date};
    $hash{$key}{time}           = $ra->[4];
    $hash{$key}{change}	  	= ParseNumeric($ra->[5]);
    $hash{$key}{percent_change} = $ra->[6];
    $hash{$key}{volume}         = $ra->[7];
    $hash{$key}{average_volume} = $ra->[8];
    $hash{$key}{bid}            = ParseNumeric($ra->[9]);
    $hash{$key}{ask}            = ParseNumeric($ra->[10]);
    $hash{$key}{previous_close} = ParseNumeric($ra->[11]);
    $hash{$key}{open}           = ParseNumeric($ra->[12]);
    my (@tmp) = split / - /, $ra->[13];
    $hash{$key}{day_low}        = ParseNumeric($tmp[0]);
    $hash{$key}{day_high}       = ParseNumeric($tmp[1]);
    (@tmp) = split / - /, $ra->[14];
    $hash{$key}{'52_week_low'}  = ParseNumeric($tmp[0]);
    $hash{$key}{'52_week_high'} = ParseNumeric($tmp[1]);
    $hash{$key}{earnings_per_share} = $ra->[15];
    $hash{$key}{price_earnings_ratio} = $ra->[16];
    $hash{$key}{dividend_date}  = $ra->[17]; 
    $hash{$key}{dividend_per_share} = $ra->[18];
    $hash{$key}{yield} = $ra->[19];
    if ($ra->[20] =~ m/(\S*)B$/) {
      $hash{$key}{market_capitalisation} = $1*(10e3); # keep it in millions
    } elsif ($ra->[20] =~ m/(\S*)M/) {
      $hash{$key}{market_capitalisation} = $1*(10e0); # keep it in millions
    } else {
      $hash{$key}{market_capitalisation} = $ra->[20];
    }
    $hash{$key}{exchange}  	= RemoveTrailingSpace($ra->[21]);
  }
  return %hash
}


sub ParseNumeric {		# parse numeric fields which could be fractions
  my $v = shift;		# expect one argument
  $v =~ s/\s*$//;		# kill trailing whitespace
  $v =~ s/\+//;			# kill leading plus sign
  if ($v =~ m|(.*) (.*)/(.*)|) {# if it is a fraction
    return $1 + $2/$3;		#   return the decimal value
  } else {			# else
    return $v;			#   return the value itself
  }
}


sub PrintHistoricalData {	# simple display routine for hist. data
  my (@res) = @_;
  my $i=1;
  foreach $ARG (@res) {
    print $i++, ": $ARG\n";
  }
}


sub RemoveTrailingSpace {
  my $txt = shift;
  $txt =~ s/\s*$//;
  return $txt;
}


sub ReportDailyData {		# detailed display / debugging routine
  my (%hash) = @_;
  foreach my $key (keys %hash) { # now split these into reference to the arrays
    printf "Name               %25s\n", $hash{$key}{name};
    printf "Symbol             %25s\n", $hash{$key}{symbol};
    printf "Exchange           %25s\n", $hash{$key}{exchange};
    printf "Date               %25s\n", $hash{$key}{date};
    printf "Time               %25s\n", $hash{$key}{time};
    printf "Previous Close     %25s\n", $hash{$key}{previous_close};
    printf "Open               %25s\n", $hash{$key}{open};
    printf "Day low            %25s\n", $hash{$key}{day_low};
    printf "Day high           %25s\n", $hash{$key}{day_high};
    printf "Close              %25s\n", $hash{$key}{close};
    printf "Change             %25s\n", $hash{$key}{change};
    printf "Percent Change     %25s\n", $hash{$key}{percent_change};
    printf "Bid                %25s\n", $hash{$key}{bid};
    printf "Ask                %25s\n", $hash{$key}{ask};
    printf "52-week low        %25s\n", $hash{$key}{'52_week_low'};
    printf "52-week high       %25s\n", $hash{$key}{'52_week_high'};
    printf "Volume             %25s\n", $hash{$key}{volume};
    printf "Average Volume     %25s\n", $hash{$key}{average_volume};
    printf "Dividend date      %25s\n", $hash{$key}{dividend_date};
    printf "Dividend / share   %25s\n", $hash{$key}{dividend_per_share};
    printf "Dividend yield     %25s\n", $hash{$key}{yield};
    printf "Earnings_per_share %25s\n", $hash{$key}{earnings_per_share};
    printf "P/E ratio          %25s\n", $hash{$key}{price_earnings_ratio};
    printf "Market Capital     %25s\n", $hash{$key}{market_capitalisation};
  }
}

sub ScrubDailyData {          # stuff the output into the hash
  my %hash = @_;              # we receive

  ## Check the date supplied from Yahoo!
  ##
  ## The first approach was to count all dates for a given market
  ## This works well when you have, say, 3 Amex and 5 NYSE stock, and
  ## Yahoo just gets one date wrong -- we can then compare the one "off-date"
  ## against, say, four "good" dates and override
  ## Unfortunately, this doesn't work so well for currencies where you
  ## typically only get one, or maybe two, and have nothing to compare against
  ##
  ## my %date;                   # date comparison hash
  ## foreach my $key (keys %hash) {# store all dates for market
  ##   $date{$hash{$key}{exchange}}{$hash{$key}{date}}++; # and count'em
  ## }
  ## -- and later 
  ##    if ($date{$hash{$key}{exchange}}{$hash{$key}{date}} # and outnumbered
  ##	  < $date{$hash{$key}{exchange}}{$Config{today}}) {
  ##	warn("Override: $hash{$key}{name}: $hash{$key}{date} has only " .
  ##	     "$date{$hash{$key}{exchange}}{$hash{$key}{date}} votes,\n\tbut " .
  ##	     "$hash{$key}{exchange} has " .
  ##	     "$date{$hash{$key}{exchange}}{$Config{today}} " .
  ##	     "votes for $Config{today}");
  ##	$hash{$key}{date} = $Config{today};
  ##      } else {
  ##	warn("$hash{$key}{name} has date $hash{$key}{date}, " .
  ##	     "not $Config{today} but no voting certainty");
  ##      }
  ##
  ##    $date{$hash{$key}{exchange}}{$Config{today}} = 0 
  ##	  unless defined($date{$hash{$key}{exchange}}{$Config{today}});
  ##
  ## So now we simply override if (and only if) the --forceupdate
  ## argument is used. This is still suboptimal if eg you are running this
  ## on public holidays. We will have to find a way to filter this
  ##
  foreach my $key (keys %hash) {# now check the date
    if ($hash{$key}{date} ne $Config{today}) {   # if date is not today
      if (defined($Config{updatedate})) {        # and if we have an override
	$hash{$key}{date} = $Config{updatedate}; # use it
	warn "Overriding date for $hash{$key}{name} to $Config{updatedate}";
      } else {
	warn "$hash{$key}{name} has $hash{$key}{date}";
      }
    }

    if (($hash{$key}{close} == $hash{$key}{previous_close}) 
	and ($hash{$key}{change} != 0)) {
      $hash{$key}{previous_close} = $hash{$key}{close} - $hash{$key}{change};
      warn "Adjusting previous close for $key from close and change\n";
    }
  }
  return %hash;
}

sub Sign {
  my $x = shift;
  if ($x > 0) {
    return 1;
  } elsif ($x < 0){
    return -1;
  } else {
    return 0;
  }
}

sub UpdateDatabase {		# update content in the db at end of day
  my ($dbh, $res) = @_;
  my ($stmt, $sth, $rv, $ra, @symbols);

  $stmt = qq{  select distinct symbol
	       from portfolio p
	       where symbol != '' };
  $stmt .= "   and $res " if (defined($res));
  $stmt .= " order by symbol;";

  @symbols = @{ $dbh->selectcol_arrayref($stmt) };
  print join " ", @symbols, "\n" if $Config{verbose};

  my @arr = GetDailyData(@symbols);# retrieve _all_ the data
  my %data = ParseDailyData(@arr); # put it into a hash
  %data = ScrubDailyData(%data);   # and "clean" it      
  ReportDailyData(%data) if $Config{verbose};
  UpdateInfoData($dbh, %data);
  DatabaseDailyData($dbh, %data);
}


sub UpdateFXDatabase {
  my ($dbh, $res) = @_;

  my ($iso2yahoo,$yahoo2iso) = GetFXMaps;

  # get all non-USD symbols (no USD as we don't need a USD/USD rate)
  my $stmt = qq{  select distinct currency
		  from portfolio 
		  where symbol != '' 
		  and currency != 'USD'
	    };
  $stmt .= "   and $res " if (defined($res));
  my @symbols = map { $iso2yahoo->{$ARG} } @{ $dbh->selectcol_arrayref($stmt)};
  if ($Config{extrafx}) {
    foreach my $arg (split /,/, $Config{extrafx}) {
      push @symbols, $iso2yahoo->{$arg};	
    }
  }
  if (scalar(@symbols) > 0) {	# if there are FX symbols
    my @arr = GetDailyData(@symbols); # retrieve _all_ the data
    my %data = ParseDailyData(@arr);
    %data = ScrubDailyData(%data);   # and "clean" it 
    ReportDailyData(%data) if $Config{verbose};
    DatabaseFXDailyData($dbh, %data);
  }
}


sub UpdateInfoData {		# update a row in the info table
  my ($dbh, %hash) = @_;
  foreach my $key (keys %hash) { # now split these into reference to the arrays
    my $cmd = "update stockinfo " .
              "set capitalisation = $hash{$key}{market_capitalisation}, " .
              "low_52weeks = $hash{$key}{'52_week_low'}, " .
  	      "high_52weeks = $hash{$key}{'52_week_high'}, " .
	      "earnings = $hash{$key}{earnings_per_share}, " .
	      "dividend = $hash{$key}{dividend_per_share}, " .
	      "p_e_ratio = $hash{$key}{price_earnings_ratio}, " .
	      "avg_volume = $hash{$key}{average_volume} " .
	      "where symbol = '$hash{$key}{symbol}';";
    $cmd =~ s|'?N/A'?|null|g;	# convert (textual) "N/A" into (database) null 
    print "$cmd\n" if $Config{debug};
    print "$hash{$key}{symbol} " if $Config{verbose};
    $dbh->do($cmd) or warn "\nFailed for $hash{$key}{symbol} with $cmd";
  }
}


BEGIN {				# Local variant of LWP::UserAgent that 
  use LWP;			# checks for user/password if document 
  package RequestAgent;		# this code taken from lwp-request, see
  no strict 'vars';		# the various LWP manual pages
  @ISA = qw(LWP::UserAgent);

  sub new { 
    my $self = LWP::UserAgent::new(@_);
    $self->agent("beancounter/$VERSION");
    $self;
  }

  sub get_basic_credentials {
    my $self = @_;
    if (defined($Config{firewall}) and $Config{firewall} ne "" 
	and $Config{firewall} =~ m/.*:.*/) {
      return split(':', $Config{firewall}, 2);
    } else {
      return (undef, undef)
    }
  }
}


1;				# required for a package file

__END__

=head1 NAME

Finance::Beancounter - Module for stock portfolio performance functions.

=head1 SYNOPSIS

 use Finance::Beancounter;

=head1 DESCRIPTION       

B<Finance::BeanCounter> provides functions to I<download>, I<store> and
I<analyse> stock market data. 

I<Downloads> are available of current (or rather: 15 or 20
minute-delayed) price and company data as well as of historical price
data.  Both forms can be stored in an SQL database (for which we
currently default to B<PostgreSQL>).

I<Analysis> currently consists of performance and risk
analysis. Performance reports comprise a profit-and-loss (or 'p/l' in
the lingo) report which can be run over arbitrary time intervals such
as C<--prevdate 'friday six months ago' --date 'yesterday'> -- in
essence, whatever the wonderful B<Date::Manip> module understands --
as well as dayendreport which defaults to changes in the last trading
day. A risk report show parametric and non-parametric value-at-risk
(VaR) estimates.

Most available functionality is also provided in the reference
implementation B<beancounter>, a convenient command-line script.

The API might change and evolve over time. The low version number
really means to say that the code is not in its final form yet, but it
has been in use for well over two years. 

More documentation is in the Perl source code.

=head1 DATABASE LAYOUT

The easiest way to see the table design is to look at the content of
the B<setup_beancounter> script. It creates five tables: I<stockinfo>,
I<stockprices>, I<fxprices>, I<portfolio> and I<indices>.

=head2 THE STOCKINFO TABLE

The I<stockinfo> table contains general (non-price) information and is
index by I<symbol>:


	    symbol   		varchar(12) not null,
	    name     		varchar(64) not null,
	    exchange 		varchar(16) not null,
	    capitalisation  	float4,
	    low_52weeks		float4,
	    high_52weeks	float4,
	    earnings		float4,
	    dividend		float4,
	    p_e_ratio		float4,
	    avg_volume		int4

This table is updated by overwriting the previous content.

=head2 THE STOCKPRICES TABLE

The I<stockprices> table contains (daily) price and volume
information. It is indexed by both I<date> and I<symbol>:

	    symbol   		varchar(12) not null,
	    date		date,
	    previous_close	float4,
	    day_open		float4,
	    day_low		float4,
	    day_high		float4,
	    day_close		float4,
	    change		float4,
	    bid			float4,
	    ask			float4,
	    volume		int4

During updates, information is appended to this table.

=head2 THE FXPRICES TABLE

The I<fxprices> table contains (daily) foreign exchange rates. It can be used to calculate home market values of foreign stocks:

	    currency   		varchar(12) not null,
	    date		date,
	    previous_close	float4,
	    day_open		float4,
	    day_low		float4,
	    day_high		float4,
	    day_close		float4,
	    change		float4

Similar to the I<stockprices> table, it is index on I<date> and I<symbol>.

=head2 THE STOCKPORTFOLIO TABLE

The I<portfolio> table contains contains the holdings information:

	    symbol   		varchar(16) not null,
	    shares		float4,
	    currency		varchar(12),
	    type		varchar(16),
	    owner		varchar(16),
	    cost		float(4),
	    date		date

It is indexed on I<symbol,owner,date>.

=head2 THE INDICES TABLE

The I<indices> table links a stock I<symbol> with one or several
market indices:

	    symbol   		varchar(12) not null,
	    index		varchar(12) not null

=head1 BUGS

B<Finance::BeanCounter> and B<beancounter> are so fresh that there are
only missing features :)

On a more serious note, this code (or its earlier predecessors) have
been in use since the fall of 1998.

Known bugs or limitations are documented in TODO file in the source
package.

=head1 SEE ALSO

F<beancounter.1>, F<smtm.1>, F<Finance::YahooQuote.3pm>,
F<LWP.3pm>, F<Date::Manip.3pm>

=head1 COPYRIGHT

Finance::BeanCounter.pm is (c) 2000, 2001 by Dirk Eddelbuettel <edd@debian.org>

Updates to this program might appear at 
F<http://eddelbuettel.com/dirk/code/beancounter.html>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.  There is NO warranty whatsoever.

The information that you obtain with this program may be copyrighted
by Yahoo! Inc., and is governed by their usage license.  See
F<http://www.yahoo.com/docs/info/gen_disclaimer.html> for more
information.

=head1 ACKNOWLEDGEMENTS

The Finance::YahooQuote module by Dj Padzensky (on the web at
F<http://www.padz.net/~djpadz/YahooQuote/>) served as the backbone for
data retrieval, and a guideline for the extension to the non-North
American quotes which was already very useful for the real-time ticker 
F<http://eddelbuettel.com/dirk/code/smtm.html>.

=cut
