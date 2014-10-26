#!perl -w
#
# High level interface above ARS module
#
# Andrew V Makarow, 2010-03-02, K)
#
#
# 2010-03-24 detached
# 2010-03-02 started inside a script
# 
package ARSObject;
use vars qw($VERSION @ISA $AUTOLOAD $CGI::Carp::CUSTOM_MSG);
use UNIVERSAL;
use strict;
use POSIX qw(:fcntl_h);

$VERSION = '0.55';

my $fretry =5;

1;

sub new {	# New ARS object
		# (-param=>value,...) -> ARS object
 my $c=shift;
 my $s ={'' => ''
	,-ctrl => undef		# ARS control struct from ars_Login()
	,-srv  => undef		# Server name
	,-usr  => undef		# User name
	,-pswd => undef		# Password string
	,-lang => ''		# Language
	,-schema => undef	# Schemas to use: [form,...]
	,-vfbase =>		# Var files base
			(do{	my $v =$^O eq 'MSWin32' ? scalar(Win32::GetFullPathName($0)) : $0;
				$v =~/^(.+?)\.[^\\\/]*$/ ? "$1-" : "$v-"
			})
	#,-storable =>undef	# Use Storable module for cache files?
	,-schgen => 1		# 1 - use vfname('meta') for '-meta', generate it from ARS if not exists.
				# 2 - renewable 'meta'
	,-schfdo => 0		# Include display only fields into schema (AR_FIELD_OPTION_DISPLAY)
	,-meta => {}		# Forms metadata from ARS:
				#	{formName}->{-fields}->{fieldName}=>{}
				#	{formName}->{-fldids}->{fieldId}=>{}
				#	Additional parameters may be:
				#	,'fieldLbl'=>label,'fieldLblc'=>label cmt
				#	,'strOut'|'strIn'=>sub(self,form,{field},$_=val){}
	#,-meta-min		# Used in 'arsmetamin' operation
	,-metax => 		# Exclude field schema parameters from '-meta'
			['displayInstanceList','permissions']
	,-metaid => {}		# Commonly used fields with common names and value translation
	,-metadn => {}		# {fieldId | fieldName => 
				#	{fieldName=>'name',FieldId=>id
				#	,strIn=>sub(self,form,{field},$_=val){}
				#	,strOut=>sub(self,form,{field},$_=val){}
				#	},...}
	,-maxRetrieve => 0	# ARS::ars_GetListEntry(maxRetrieve)
	,-entryNo => undef	# Logical number of entry inserted
	,-strFields => 1	# Translate fields data using 'strIn'/'strOut'/'-meta'?
	,-cmd =>''		# Command running, for err messages, script local $s->{-cmd}
	,-die =>undef		# Error die/warn,  'Carp' or 'CGI::Carp...'
	# ,-diemsg => undef	#
	,-warn=>undef		# , see set() and connect() below
	# ,-warnmsg => undef	#
	,-cpcon=>undef		# Translation to console codepage sub{}(self, args) -> translated
	,-echo=>0		# Echo printout switch
	,-dbi=>undef		# DBI object, by dbiconnect()
	,-dbiconnect =>undef	#
	,-cgi=>undef		# CGI object, by cgi()
	,-smtp=>undef
	,-smtphost=>undef
	#,-fpl=>[]		# CGI Form Presenter fields list
	#,-fphc=>{}		# 	CGI fields cache
	#,-fphd=>{}		#	DB fields cache
	#,-fpbv=>[]		#	buffer values
	#,-fpbn=>''		#	buffer name == record common name
	};
 bless $s,$c;
 set($s, @_);
 $s->{-storable} =eval('use Storable; 1') if !exists($s->{-storable});
 $s
}


sub AUTOLOAD {	# Use self->arsXXX() syntax for ars_XXX(ctrl,...) calls
 my $m =substr($AUTOLOAD, rindex($AUTOLOAD, '::')+2);
 $m =~s/^ars/ARS::ars_/;
 return(&{$_[0]->{-die}}($_[0]->efmt("Called name without 'ars_'", $_[0]->{-cmd}, undef, 'AUTOLOAD',$m)))
	if $m !~/^ars_/;
 no strict;
 &$m($_[0]->{-ctrl}, @_[1..$#_])
}


sub DESTROY {
	my $s =shift;
	$s->{-die} =undef;
	$s->{-warn}=undef;
	$s->{-ctrl} && eval{ars_Logoff($s->{-ctrl})};
	$s->{-ctrl}=undef;
	$s->{-dbi} && eval{$s->{-dbi}->disconnect()};
	$s->{-dbi} =undef;
	$s->{-cgi} =undef;
	$s->{-diemsg}  =undef;
	$s->{-warnmsg} =undef;
}


sub set {	# Set/Get parameters
		# () -> (parameters)
		# (-param) -> value
		# (-param => value,...) -> self
 return(keys(%{$_[0]})) if scalar(@_) ==1;
 return($_[0]->{$_[1]}) if scalar(@_) ==2;
 my ($s,%a) =@_;
 foreach my $k (keys %a) {
	$s->{$k} =$a{$k}
 }
 if ($a{-die}) {
	if ($a{-die} =~/^Carp/) {
		eval('use ' .$a{-die} .';');
		$s->{-die} =\&Carp::confess;
		$s->{-warn}=\&Carp::carp;
	}
	elsif ($a{-die} =~/^CGI::Carp/) {
		eval('use ' .$a{-die} .';');
		$s->{-die} =\&CGI::Carp::confess;
		$s->{-warn}=\&CGI::Carp::carp;
		if ($s->{-diemsg}) {
			my $dm =$s->{-diemsg};
			CGI::Carp::set_message(sub{&$dm(@_); $s->disconnect() if $s;})
		}
	}
	elsif ($a{-die} =~/^CGI::Die/) {
		eval('use Carp;');
		$s->{-die} =\&Carp::confess;
		$s->{-warn}=\&Carp::carp;
		my $sigdie =$SIG{__DIE__};
		$SIG{__DIE__} =sub{
			return if ineval();
			if ($s && $s->{-diemsg}) {
				&{$s->{-diemsg}}(@_)
			}
			else {
				print   $s->{-cgi}->header(-content=>'text/html'
					,($ENV{SERVER_SOFTWARE}||'') =~/IIS/ ? (-nph=>1) : ()
					)
					, "<h1>Error:</h1>"
					, $s->{-cgi}->escapeHTML($_[0])
					, "<br />\n"
					if $s && $s->{-cgi}
			}
			$s->DESTROY() if $s;
			$s =undef;
			# $SIG{__DIE__} =$sigdie;
			# &$sigdie(@_) if ref($sigdie) eq 'CODE';
			# CORE::die($_[0]);
		};
		$SIG{__WARN__} =sub{
			return if !$^W ||ineval();
			if ($s && $s->{-warnmsg}) {
				&{$s->{-warnmsg}}(@_)
			}
			else {
				print   '<div style="font-weight: bolder">Warnig: '
					, $s->{-cgi}->escapeHTML($_[0])
					, "<div>\n"
					if $s && $s->{-cgi}
			}
			# CORE::warn($_[0]);
		} if $^W;
	}
 }
 elsif ($a{-vfbase}) {
	if ($a{-vfbase} !~/[\\\/]/) {
		my $v =$^O eq 'MSWin32' ? scalar(Win32::GetFullPathName($0)) : $0;
		$s->{-vfbase} =$v =~/^(.+?[\\\/])[^\\\/]+$/ ? $1 .$a{-vfbase} : $a{-vfbase};
	}
 }
 $s
}


sub ineval {	# is inside eval{}?
		# for PerlEx and mod_perl
		# see CGI::Carp::ineval comments and errors
 return $^S	if !($ENV{GATEWAY_INTERFACE}
			&& ($ENV{GATEWAY_INTERFACE} =~/PerlEx/))
		&& !$ENV{MOD_PERL};
 my ($i, @a) =(1);
 while (@a =caller($i)) {
	return(0) if $a[0] =~/^(?:PerlEx::|Apache::Perl|Apache::Registry|Apache::ROOT)/i;
	return(1) if $a[3] eq '(eval)';
	$i +=1;
 }
 $^S
}

		# error message form ??? use ???
		# (err/var, command, operation, function, args)
sub efmt {
	efmt1(@_)
}

sub efmt0 {
 my ($s, $e, $c, $o, $f, @a) =@_;
 cpcon($s
	,join(': '
		,($c ? $c : ())
		,($f ? $f .'(' .join(',', map {$s->dsquot($_)} @a) .')' : ())
		,($o ? $o : ())
		)
	.($e && ($e eq '$!') && $^E ? (' -> ' .$! .' / ' .$^E) : ( ' -> ' .($e || 'unknown error')))
	)
}

sub efmt1 {
 my ($s, $e, $c, $o, $f, @a) =@_;
 cpcon($s
	,join(' # '
		,($e && ($e eq '$!') && $^E ? ($! .' / ' .$^E) : ($e || 'unknown error'))
		,($o ? $o : ())
		,($f ? $f .'(' .join(',', map {$s->dsquot($_)} @a) .')' : ())
		,($c ? $c : ())
		)
	)
}


sub strquot {	# Quote and Escape string enclosing in ''
 my $v =$_[1];		# (string) -> escaped
 return('undef') if !defined($v);
 $v =~s/([\\'])/\\$1/g;
 $v =~s/([\x00-\x1f])/sprintf("\\x%02x",ord($1))/eg;
 $v =~/^\d+$/ ? $v : ('\'' .$v .'\'');
}


sub strquot2 {	# Quote and Escape string enclosing in ""
 my $v =$_[1];		# (string) -> escaped
 return('undef') if !defined($v);
 $v =~s/([\\"])/\\$1/g;
 $v =~s/([\x00-\x1f])/sprintf("\\x%02x",ord($1))/eg;
 $v =~/^\d+$/ ? $v : ('"' .$v .'"');
}


sub arsquot {	# Quote string for ARS
 return('NULL') if !defined($_[1]);
 my $v =$_[1];
 $v =~s/"/""/g;
 $v =~/^\d+$/ ? $v : ('"' .$v .'"');
}


sub dsquot {	# Quote data structure
   $#_ <2		# (self, ?'=>', data struct)
 ? dsquot($_[0],'=> ',$_[1])
 : !ref($_[2])	# (, hash delim, value) -> stringified
 ? strquot($_[0],$_[2])
 : ref($_[2]) eq 'ARRAY'
 ? '[' .join(', ', map {dsquot(@_[0..1],$_)
			} @{$_[2]}) .']'
 : ref($_[2]) eq 'HASH'
 ? '{' .join(', ', map {$_ .$_[1] .dsquot(@_[0..1],$_[2]->{$_})
			} sort keys %{$_[2]}) .'}'
 : strquot($_[0],$_[2])
}


sub dsquot1 {	# Quote data structure, defined elements only
   $#_ <2		# (self, ?'=>', data struct)
 ? dsquot1($_[0],'=> ',$_[1])
 : !ref($_[2])	# (, hash delim, value) -> stringified
 ? strquot($_[0],$_[2])
 : ref($_[2]) eq 'ARRAY'
 ? '[' .join(', ', map {defined($_) ? dsquot1(@_[0..1],$_) : ()
			} @{$_[2]}) .']'
 : ref($_[2]) eq 'HASH'
 ? '{' .join(', ', map {defined($_[2]->{$_}) ? $_ .$_[1] .dsquot1(@_[0..1],$_[2]->{$_}) : ()
			} sort keys %{$_[2]}) .'}'
 : strquot($_[0],$_[2])
}


sub dsdump {     # Data structure dump to string
 my ($s, $d) =@_;	# (data structure) -> dump string
 eval('use Data::Dumper');
 my $o =Data::Dumper->new([$d]); 
 $o->Indent(1);
 $o->Deepcopy(1);
 $o->Dump();
}


sub dsparse {  # Data structure dump string to perl structure
 my ($s, $d) =@_;	# (string) -> data structure
 eval('use Safe; 1')
 && Safe->new()->reval($d)
}


sub dscmp {	# Compare data structures
 my($s, $ds1, $ds2) =@_;
 return(1) if (defined($ds1) && !defined($ds2)) ||(defined($ds2) && !defined($ds1));
 return(0) if !defined($ds1) && !defined($ds2);
 return(1) if (ref($ds1) ||'') ne (ref($ds2) ||'');
 return($ds1 cmp $ds2) if !ref($ds1);
 return(dsquot($s,$ds1) cmp dsquot($s,$ds2)) if ref($ds1) eq 'ARRAY';
 return(dsquot($s,$ds1) cmp dsquot($s,$ds2)) if ref($ds1) eq 'HASH';
 $ds1 cmp $ds2
}


sub dsunique {	# Unique list
 my %h =(map {defined($_) ? ($_ => 1) : ()} @_[1..$#_]);
 use locale;
 sort keys %h
}



sub dsmerge {	# Merge arrays or hashes
 my $r;
 if (ref($_[1]) eq 'ARRAY') {
	$r =[];
	for (my $i=1; $i <=$#_; $i++) {
		for (my $j=0; $j <=$#{$_[$i]}; $j++) {
			$r->[$j] =$_[$i]->[$j]
		}
	}
 }
 elsif (ref($_[1]) eq 'HASH') {
	$r ={};
	for (my $i=1; $i <=$#_; $i++) {
		foreach my $k (keys %{$_[$i]}) {
			$r->{$k} =$_[$i]->{$k}
		}
	}
 }
 $r
}


sub strtime {	# Stringify Time
 my $s =shift;
 if (scalar(@_) && !defined($_[0])) {
	&{$s->{-warn}}('Not defined time in strtime()') if $^W;
	return(undef)
 }
 my $msk =(scalar(@_) ==0) || ($_[0] =~/^\d+$/i) ? 'yyyy-mm-dd hh:mm:ss' : shift;
 my @tme =(scalar(@_) ==0) ? localtime(time) : scalar(@_) ==1 ? localtime($_[0]) : @_;
 $msk =~s/yyyy/%Y/;
 $msk =~s/yy/%y/;
 $msk =~s/mm/%m/;
 $msk =~s/mm/%M/i;
 $msk =~s/dd/%d/;
 $msk =~s/hh/%H/;
 $msk =~s/hh/%h/i;
 $msk =~s/ss/%S/;
#eval('use POSIX');
 my $r =POSIX::strftime($msk, @tme);
# &{$s->{-warn}}("Not defined strtime('$msk'," .join(',', map {defined($_) ? $_ : 'undef'} @tme) .")")
#	if !defined($r);
 $r
}


sub timestr {	# Time from String
 my $s   =shift;
 if (scalar(@_) && !defined($_[0])) {
	&{$s->{-warn}}('Not defined time in timestr()') if $^W;
	return(undef)
 }
 my $msk =(scalar(@_) <2) || !$_[1] ? 'yyyy-mm-dd hh:mm:ss' : shift;
 my $ts  =$_[0];
 my %th;
 while ($msk =~/(yyyy|yy|mm|dd|hh|MM|ss)/) {
    my $m=$1; $msk =$';
    last if !($ts =~/(\d+)/);
    my $d =$1; $ts   =$';
    $d   -=1900   if $m eq 'yyyy' ||$m eq '%Y';
    $m    =chop($m);
    $m    ='M'    if $m eq 'm' && $th{$m};
    $m    =lc($m) if $m ne 'M';
    $th{$m}=$d;
 }
#eval('use POSIX');
 my $r =POSIX::mktime($th{'s'}||0,$th{'M'}||0,$th{'h'}||0,$th{'d'}||0,($th{'m'}||1)-1,$th{'y'}||0,0,0,(localtime(time))[8]);
# &{$s->{-warn}}("Not defined timestr('$_[0]')")
#	if !defined($r);
 $r
}


sub timeadd {	# Adjust time to years, months, days,...
 my $s =$_[0];
 if (!defined($_[1])) {
	&{$s->{-warn}}('Not defined time in timeadd()') if $^W;
	return(undef)
 }
 my @t =localtime($_[1]);
 my $i =5;
 foreach my $a (@_[2..$#_]) {$t[$i] += ($a||0); $i--}
#eval('use POSIX');
 POSIX::mktime(@t[0..5],0,0,$t[8])
}


sub charset {
 $_[0]->{-charset} && ($_[0]->{-charset} =~/^\d/)
	? 'windows-' .$_[0]->{-charset}
	: ($_[0]->{-charset} || ($_[0]->{-cgi} && $_[0]->{-cgi}->charset())
		|| eval('!${^ENCODING}') && eval('use POSIX; POSIX::setlocale(POSIX::LC_CTYPE)=~/\\.([^.]+)$/ ? "cp$1" : "cp1252"'))
}


sub cptran {	# Translate strings between codepages
 my ($s,$f,$t,@s) =@_;	# (from, to, string,...) -> string,...
 if (($] >=5.008) && eval("use Encode; 1")) {
	map {$_=  /oem|866/i	? 'cp866'
		: /ansi|1251/i	? 'cp1251'
		: /koi/i	? 'koi8-r'
		: /8859-5/i	? 'iso-8859-5'
		: $_
		} $f, $t;
	map {Encode::is_utf8($_)
		? ($_ =Encode::encode($t, $_, 0))
		: Encode::from_to($_, $f, $t, 0)
		if defined($_) && ($_ ne '')
		} @s;
 }
 else {
	foreach my $v ($f, $t) {	# See also utf8enc, utf8dec
		if    ($v =~/oem|866/i)   {$v ='���������������������������������������񦧨�����������������������'}
		elsif ($v =~/ansi|1251/i) {$v ='�����Ũ����������������������������������������������������������'}
		elsif ($v =~/koi/i)       {$v ='�������������������������������������ţ��������������������������'}
		elsif ($v =~/8859-5/i)    {$v ='������������������������������������������������������������������'}
	}
	map {eval("~tr/$f/$t/") if defined($_)} @s;
 }
 @s >1 ? @s : $s[0];
}


sub cpcon {		# Translate to console codepage
   $_[0] && $_[0]->{-cpcon}
 ? &{$_[0]->{-cpcon}}(@_)
 : $#_ <2
 ? $_[1]
 : (@_[1..$#_])
}


sub sfpath {		# self file path
			# () -> script's dir
			# (subpath) -> dir/subpath
 my $p =$0 =~/[\\\/]/ ? $0 : $^O eq 'MSWin32' ? Win32::GetFullPathName($0) : '';
 $_[1]
 ? (($p =~/^(.+?[\\\/])[^\\\/]+$/ ? $1 : '') .$_[1])
 : ($p =~/^(.+?)[\\\/][^\\\/]+$/ ? $1 : '')
}



sub fopen {		# Open file
 my $s =shift;		# ('-b',filename) -> success
 my $o =$_[0] =~/^-(?:\w[\w\d+-]*)*$/ ? shift : '-';
 my $f =$_[0]; $f ='<' .$f if $f !~/^[<>]/;
 eval('use IO::File');
 my $h =IO::File->new($f) || return(&{$s->{-die}}($s->efmt('$!',undef,'cannot open file','fopen',$f)));
 $h->binmode() if $h && ($o =~/b/);
 $h
}


sub fdirls {		# Directory listing
 my $s =shift;		# ('-',pathname, ?filter sub{}(self, path, $_=entry), ? []) -> (list) || [list]
 my $o =$_[0] =~/^-(?:\w[\w\d+-]*)*$/ ? shift : '-';
 my ($f, $cf, $cs) =@_;
 local *FILE; opendir(FILE, $f) || return(&{$s->{-die}}($s->efmt('$!',undef,'cannot open dir','fdirls',$f)));
 local $_;
 my ($r, @r);
 if ($cs) {
	while (defined($r =readdir(FILE))) {
		push @$cs, $r if !$cf ||&$cf($s,$f,$_ =$r)
	}
	closedir(FILE);
	return $cs;
 }
 else {
	while (defined($r =readdir(FILE))) {
		push @r, $r if !$cf ||&$cf($s,$f,$_ =$r)
	}
	closedir(FILE);
	return @r;
 }
}


sub fstore {		# Store file
 my $s =shift;		# ('-b',filename, strings) -> success
 my $o =$_[0] =~/^-(?:\w[\w\d+-]*)*$/ ? shift : '-';
 my $f =$_[0]; $f ='>' .$f if $f !~/^[<>]/;
 print "fstore('$f')\n" if $s->{-echo};
 # local $SIG{'TERM'} ='IGNORE';
 # local $SIG{'INT'}  ='IGNORE';
 # local $SIG{'BREAK'}='IGNORE';
 my $r;
 local *FILE;
 for (my $i =0; $i <$fretry; $i++) {
	$r =open(FILE, $f);
	last if $r;
 }
 return(&{$s->{-die}}($s->efmt('$!',undef,'cannot open file','fstore',$f)))
	if !$r;
 if ($o =~/b/) {
	binmode(FILE);
	$r =defined(syswrite(FILE,$_[1]))
 }
 else {
	$r =print FILE join("\n",@_[1..$#_])
 }
 close(FILE);
 $r || &{$s->{-die}}($s->efmt('$!',undef,'Cannot write file','fstore',$f))
}


sub fload {		# Load file
 my $s =shift;		# ('-b',filename) -> content
 my $o =$_[0] =~/^-(?:\w[\w\d+-]*)*$/ ? shift : '-';
 my($f,$f0) =($_[0],$_[0]); 
	if ($f =~/^[<>]+/)	{$f0 =$'}
	else			{$f  ='<' .$f}
 print "fload('$f')\n" if $s->{-echo};
 local *FILE;
 my $r;
 for (my $i =0; $i <$fretry; $i++) {
	$r =open(FILE, $f);
	last if $r;
 }
 return(&{$s->{-die}}($s->efmt('$!',undef,'Cannot open file','fload',$f)))
	if !$r;
 my $b =undef;
 binmode(FILE) if $o =~/b/;
 $r =read(FILE,$b,-s $f0);
 close(FILE);
 defined($r) ? $b : &{$s->{-die}}($s->efmt('$!',undef,'Cannot read file','fload',$f))
}


sub vfname {		# Name of variables file
			# (varname|-slot) -> pathname
 return($_[0]->{-vfbase}) if !$_[1];
 my $v =$_[1];	$v =~s/[\s.,:;|\/\\?*+()<>\]\["']/_/g;
 $_[0]->{-vfbase} .($v =~/^-(.+)/ ? ($1 .($_[2] ||'.var')) : ($v .($_[2] ||'.var')))
}


sub vfstore {		# Store variables file
			# (varname, {data}) -> success
			# (-slot) -> success
 my($s,$n,$d)=@_;
 $d =$s->{$n} if !$d && ($n =~/^-/);
 my $f =$s->vfname($n, '.new');
 my $r;
 if (($n =~/^-/) && exists($s->{"${n}-storable"}) ? $s->{"${n}-storable"} : $s->{-storable}) {
	for (my $i =0; ($i <$fretry) && eval("use Storable; 1"); $i++) {
		$r =Storable::store($d, $f);
		last if $r;
	}
	return(&{$s->{-die}}($s->efmt('$!',$s->{-cmd},undef,'Storable::store',$f)))
		if !$r;
 }
 else {
	$r =$s->fstore('-', $f, $s->dsdump($d));
 }
 if ($r) {
	my $rr =0;
	for (my $i =0; $i <$fretry; $i++) {
		$rr =rename($f, $s->vfname($n));
		last if $rr
	}
	return(&{$s->{-die}}($s->efmt('$!',$s->{-cmd},undef,'rename',$f,'*.var')))
		if !$rr
 }
 $r
}


sub vfload {		# Load variables file
			# (varname|-slot, ?{use default} | load default, ?renew | renew seconds) -> {data}
 my($s,$f,$d,$nn) =@_;	# -slot-calc, -slot-store
 my $k =($f =~/^-/ ? $f : undef);
 $f =$s->vfname($f);
 if ($nn && $nn >1) {
	my @st =stat($f);
	$nn =0 if $st[9] && (time() -$st[9] <$nn);
 }
 if ($d && ($nn || !-f $f)) {
	if (ref($d)) {
		$s->vfstore($k, $d =ref($d) eq 'CODE' ? &$d($s,$k) : $d);
		$s->{$k} =$d if $k;
	}
	elsif (!$k) {
	}
	elsif (ref($s->{"$k-calc"}) eq 'CODE') {
		my $cc =$s->{"$k-calc"};
		local $s->{"$k-calc"} =undef;
		$s->{$k} =$d =&$cc($s,$k);
	}
	elsif (ref($s->{"$k-store"}) eq 'CODE') {
		$s->vfstore($k, $s->{$k} =$d =&{$s->{"$k-store"}}($s,$k))
	}
	elsif (ref($s->{$k}) eq 'CODE') {
		$s->vfstore($k, $s->{$k} =$d =&{$s->{$k}}($s,$k))
	}
	return($d)
 }
 elsif (ref($s->{"$k-calc"}) eq 'CODE') {
	my $cc =$s->{"$k-calc"};
	local $s->{"$k-calc"} =undef;
	$s->{$k} =$d =&$cc($s,$k);
	return($d);
 }
 my $r;
 if (0) {
	$r =($k && exists($s->{"${k}-storable"}) ? $s->{"${k}-storable"} : $s->{-storable})
	? eval("use Storable; 1")
		&& Storable::retrieve($f)
		|| return(&{$s->{-die}}($s->efmt('$!',$s->{-cmd},undef,'Storable::retrieve',$f)))
	: ((eval{do($f)}) || return(&{$s->{-die}}($s->efmt($@,$s->{-cmd},undef,'do',$f))));
 }
 else {
	local *FILE;
	for (my $i =0; $i <$fretry; $i++) {
		$r =open(FILE, "<$f");
		last if $r;
	}
	return(&{$s->{-die}}($s->efmt('$!',undef,'Cannot open file','vfload',$f)))
		if !$r;
	binmode(FILE);
	my $v;
	sysread(FILE,$v,64,0)
		||return(&{$s->{-die}}($s->efmt('$!',$s->{-cmd},undef,'sysread',$f)));
	$r =($v 
		? $v !~/^\$VAR1\s*=/
		: ($k && exists($s->{"${k}-storable"}) ? $s->{"${k}-storable"} : $s->{-storable}))
	? ((seek(FILE,0,0) ||1)
		&& eval("use Storable; 1")
		&& Storable::fd_retrieve(\*FILE)
		|| return(&{$s->{-die}}($s->efmt('$!',$s->{-cmd},undef,'Storable::retrieve',$f))))
	: ((eval{close(FILE); 1}) &&
		do($f) || return(&{$s->{-die}}($s->efmt($@,$s->{-cmd},undef,'do',$f)))
		);
	eval{close(FILE)};
 }
 $s->{$k} =$r if $k;
 $r
}



sub vfrenew {		# Renew variables file
 my($s,$f,$nn) =@_;	# (-slot, ?period seconds) -> vfload
 return(1) if $f !~/^-/;
 vfload($s,$f,1,$nn ||1);
}



sub vfclear {	# Clear vfdata() and vfhash()
 my($s,$f) =@_;	# (-slot, ?period seconds) -> vfload
 return(1) if $f !~/^-/;
 delete($s->{$f});
 foreach my $k (keys %$s) {
	next if $k !~/^\Q$f\E[\/].+/;
	delete $s->{$k};
 }
 1;
}


sub vfdata {	# Access to array data from variables file
		# automatically load using vfload().
		# (-slot) -> [array]
		# (-slot, filter sub{}(self, -slot, index, $_=value)) -> [array]
 vfload($_[0], $_[1], 1) if !$_[0]->{$_[1]} || (ref($_[0]->{$_[1]}) eq 'CODE');
 if ($_[2]) {
	if (ref($_[2]) eq 'CODE') {
		local $_;
		local $_[0]->{-cmd} =($_[0]->{-cmd} ? $_[0]->{-cmd} .': ' : '')
			."vfdata('$_[1]', sub{})";
		my ($rr, $v);
		if (ref($_[0]->{$_[1]}) eq 'ARRAY') {
			$rr =[];
			for(my $i=0; $i<=$#{$_[0]->{$_[1]}}; $i++) {
				if (!defined(eval{$v =&{$_[2]}($_[0], $_[1], $i, $_ =$_[0]->{$_[1]}->[$i])}) && $@) {
					last if $@ =~/^last[\r\n]*$/;
					next if $@ =~/^next[\r\n]*$/;
					return(&{$_[0]->{-die}}($_[0]->efmt($@,$_[0]->{-cmd})));
				}
				elsif ($v) {
					push @$rr, $_[0]->{$_[1]}->[$i]
				}
			}
		}
		elsif (ref($_[0]->{$_[1]}) eq 'HASH') {
			$rr ={};
			foreach my $i (keys %{$_[0]->{$_[1]}}) {
				if (!defined(eval{$v =&{$_[2]}($_[0], $_[1], $i, $_ =$_[0]->{$_[1]}->{$i})}) && $@) {
					last if $@ =~/^last[\r\n]*$/;
					next if $@ =~/^next[\r\n]*$/;
					return(&{$_[0]->{-die}}($_[0]->efmt($@,$_[0]->{-cmd})));
				}
				elsif ($v) {
					$rr->{$i} =$_[0]->{$_[1]}->{$i}
				}
			}
		}
		return($rr)
	}
	else {
		return($_[0]->{$_[1]}->[$_[2]])
	}
 }
 $_[0]->{$_[1]}
}


sub vfhash {	# Access to hash of array data from variables file
		# automatically formed in memory using vfdata().
		# (-slot, key name) -> {hash from vfdata()}
		# (-slot, key name => key value) -> {key=>value,...}
		# (-slot, key name => key value => elem name ) -> elem value
		# (-slot, key name => filter sub{}(self, -slot, key, $_ = value)) -> {key=>value,...}
 my($s, $f, $k, $v, $e) =@_;
 return(&{$s->{-die}}($s->efmt('Key name needed',undef,undef,'vfhash',$f))) if !$k;
 $s->vfload($f, 1) if !$s->{$f} ||(ref($s->{$f}) eq 'CODE');
 my $kk ="$f/$k";
 if (!$s->{$kk}) {
	$s->{$kk} ={};
	if (ref($s->{$f}) eq 'ARRAY') {
		for(my $i=0; $i<=$#{$s->{$f}}; $i++) {
			$s->{$kk}->{$s->{$f}->[$i]->{$k}} =$s->{$f}->[$i]
				if defined($s->{$f}->[$i]->{$k})
		}
	}
	else {
		foreach my $kh (keys %{$s->{$f}}) {
			$s->{$kk}->{$s->{$f}->{$kh}->{$k}} =$s->{$f}->{$kh}
				if defined($s->{$f}->{$kh}->{$k})
		}
	}
 }
 if (ref($v) eq 'CODE') {
	my ($rh, $t) =({});
	local $_;
	local $_[0]->{-cmd} =($_[0]->{-cmd} ? $_[0]->{-cmd} .': ' : '')
		."vfhash('$f', '$k', sub{})";
	foreach my $ke (keys %{$s->{$kk}}) {
		if (!defined(eval{$t =&$v($s, $f, $k, $_ =$s->{$kk}->{$ke})}) && $@) {
			last if $@ =~/^last[\r\n]*$/;
			next if $@ =~/^next[\r\n]*$/;
			return(&{$s->{-die}}($s->efmt($@,$s->{-cmd})));
		}
		elsif ($t) {
			$rh->{$ke} =$s->{$kk}->{$ke};
		}
	}
	return($rh)
 }
 !defined($v) 
 ? $s->{$kk} 
 : !defined($s->{$kk})
 ? $s->{$kk}
 : !ref($s->{$kk}->{$v})
 ? $s->{$kk}->{$v}
 : defined($e)
 ? $s->{$kk}->{$v}->{$e}
 : $s->{$kk}->{$v}
}



sub vfdistinct {# Distinct values from vfdata() field.
		# (-slot, key name) -> [keys %{vfhash(...)}]
		# (-slot, key name => filter sub{}(self, -slot, key, $_ = value)) -> [keys %{vfhash(...)}]
 my($s, $f, $k, $v) =@_;
 my(%rh, $t);
 local $_;
 local $_[0]->{-cmd} =($_[0]->{-cmd} ? $_[0]->{-cmd} .': ' : '')
	."vfdistinct('$f', '$k', sub{})";
 $s->vfload($f, 1) if !$s->{$f} ||(ref($s->{$f}) eq 'CODE');
 if (ref($s->{$f}) eq 'ARRAY') {
	for(my $i=0; $i<=$#{$s->{$f}}; $i++) {
		if (!defined($s->{$f}->[$i]->{$k})) {
		}
		elsif ($v && !defined(eval{$t =&$v($s, $f, $k, $_ =$s->{$f}->[$i])}) && $@) {
			last if $@ =~/^last[\r\n]*$/;
			next if $@ =~/^next[\r\n]*$/;
			return(&{$s->{-die}}($s->efmt($@,$s->{-cmd})));
		}
		elsif (!$v ||$t) {
			$rh{$s->{$f}->[$i]->{$k}} =1
		}
	}
 }
 else {
	foreach my $kh (keys %{$s->{$f}}) {
		if (!defined($s->{$f}->{$kh}->{$k})) {
		}
		elsif ($v && !defined(eval{$t =&$v($s, $f, $k, $_ =$s->{$k}->{$kh})}) && $@) {
			last if $@ =~/^last[\r\n]*$/;
			next if $@ =~/^next[\r\n]*$/;
			return(&{$s->{-die}}($s->efmt($@,$s->{-cmd})));
		}
		elsif (!$v ||$t) {
			$rh{$s->{$f}->{$kh}->{$k}} =1
		}
	}
 }
 use locale;
 return([sort {$a cmp $b} keys %rh])
}



sub connect {		# Connect to ARS server
 eval('use ARS');	# (-param=>value,...) -> self
 my $s =shift;		# -srv, -usr, -pswd, -lang
 $s->set(@_);
 $s->set(-die=>'Carp') if !$s->{-die};
 local $s->{-cmd} ="connect()";
 return($s) if $s->{-ctrl};
 print $s->cpcon("connect()\n") if $s->{-echo};
 return($s) if $s->{-ctrl} && ARS::ars_VerifyUser($s->{-ctrl});
 $s->{-ctrl} =ARS::ars_Login(
		$s->{-srv}, $s->{-usr}, $s->{-pswd}, $s->{-lang}
		, '' # , join('-', ($ENV{COMPUTERNAME} ||$ENV{HOSTNAME} ||eval('use Sys::Hostname;hostname') ||'localhost'), getlogin() || $> || '', $$, $^T, time())
		, 0, 0)
	|| return(&{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd},undef,'ars_Login', map {$_=>$s->{$_}} qw(-srv -usr -lang))));
 $s->{-ctrl} && ARS::ars_SetSessionConfiguration($s->{-ctrl}, &ARS::AR_SESS_OVERRIDE_PREV_IP, 1);
 $s->arsmeta();
 $s
}


sub disconnect {	# Disconnect data servers
 my $s =shift;
 $s->{-ctrl} && eval{ars_Logoff($s->{-ctrl})};
 $s->{-ctrl}=undef;
 $s->{-dbi} && eval{$s->{-dbi}->disconnect()};
 $s->{-dbi} =undef;
}


sub arsmeta {		# Load/refresh ARS metadata
 my $s =shift;		# -srv, -usr, -pswd, -lang
 $s->set(@_);
 local $s->{-cmd} =($s->{-cmd} ? $s->{-cmd} .': ' : '')
	.($s->{-schgen} ? "dumper('" .$s->vfname('meta') ."')" : 'arsmeta()');
 if (ref($s->{-schgen})
 || ($s->{-schgen} && ($s->{-schgen} >1))
 || (!-e $s->vfname('-meta'))
	) {
	#
	# Data types:
	# 'integer','real','char','enum','time','decimal'
	# 'diary','attach','currency'
	# 'trim','control','table','column','page','page_holder'
	#
	my ($vfs, $vfu);
	local $s->{-schgen} =$s->{-schgen};
	if (ref($s->{-schgen}) && (-e $s->vfname('-meta'))) {
		$s->vfload('-meta');
	}
	elsif (($s->{-schgen} >1) && (-e $s->vfname('-meta'))) {
		$s->vfload('-meta');
		$vfs =[stat $s->vfname('-meta')]->[9];
	}
	else {
		$s->{-meta} ={};
	}
	foreach my $f (ref($s->{-schgen}) ? @{$s->{-schgen}} : @{$s->{-schema}}){
		if ($vfs && $s->{-meta}->{$f}) {
			my $fa =ARS::ars_GetSchema($s->{-ctrl}, $f);
			!$fa && return(&{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd},undef,'ars_GetSchema',$f)));
			# print $s->strtime($fa->{timestamp}),'/',$s->strtime($vfs), "\n", $s->cpcon($s->dsdump($fa)), "\n"; exit(0);
			next	if $s->{-meta}->{$f} && $s->{-meta}->{$f}->{timestamp}
				? ($s->{-meta}->{$f}->{timestamp}||0) >=($fa->{timestamp}||0)
				: $vfs >=($fa->{timestamp}||0 +60*60);
			$s->{-meta}->{$f} ={};
			$s->{-meta}->{$f}->{-fields} ={};
			$s->{-meta}->{$f}->{timestamp} =$fa->{timestamp};
			$vfu =1
		}
		else {
			$s->{-meta}->{$f} ={};
			$s->{-meta}->{$f}->{-fields} ={};
			$vfu =1
		}
		my %ff =ARS::ars_GetFieldTable($s->{-ctrl}, $f);
		!%ff && return(&{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd},undef,'ars_GetFieldTable',$f)));
		foreach my $ff (sort keys %ff) {
			my $fm =ARS::ars_GetField($s->{-ctrl},$f,$ff{$ff})
				|| return(&{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd},undef,'ars_GetField',$f,$ff)));
			# 'fieldId', 'fieldName', 'dataType'
			next	if !$fm->{dataType}
				|| ($fm->{dataType} =~/^(trim|control|table|column|page)/);
			next	if !$s->{-schfdo} && $fm->{option} && ($fm->{option} == 4); # AR_FIELD_OPTION_DISPLAY
			$s->{-meta}->{$f}->{-fields}->{$ff} =$fm;
			if ($fm->{displayInstanceList}->{dInstanceList}
			&& !exists($fm->{fieldLbl}) && !exists($fm->{fieldLblc})) {
				for (my $i =0; $i <=$#{$fm->{displayInstanceList}->{dInstanceList}}; $i++) {
					next if !$fm->{displayInstanceList}->{dInstanceList}->[$i]->{props};
					for(my $j =0; $j <=$#{$fm->{displayInstanceList}->{dInstanceList}->[$i]->{props}}; $j++) {
						if (20 ==$fm->{displayInstanceList}->{dInstanceList}->[$i]->{props}->[$j]->{prop}) {
							$fm->{fieldLbl} =$fm->{displayInstanceList}->{dInstanceList}->[$i]->{props}->[$j]->{value}
								if $i;
							$fm->{fieldLblc} =($fm->{fieldLblc} ? $fm->{fieldLblc} .'; ' : '')
								."[$i,$j] " .$fm->{displayInstanceList}->{dInstanceList}->[$i]->{props}->[$j]->{value}
						}
					}
				}
			}
			if ($s->{-metax}) {
				foreach my $e (@{$s->{-metax}}) {
					delete $fm->{$e};
				}
			}
		}
	}
	if (!$s->{-schgen}) {
	}
	else {
		$vfu && $s->vfstore('-meta')
	}
	# print $s->cpcon($s->dsdump($s->{-meta})), "\n"; exit(0);
 }
 elsif (-e $s->vfname('meta')) {
	$s->vfload('-meta');
	# print $s->cpcon($s->dsdump($s->{-meta})), "\n"; exit(0);
 }
 else {
	$s->{-meta} ={};
	return(&{$s->{-die}}($s->efmt('No metadata',$s->{-cmd})))
 }
 $s->arsmetaix() if $s->{-meta};
}


sub arsmetaix {	# Index ARS metadata
 my $s =shift;
 if ($s->{-meta}) {
	foreach my $f (keys %{$s->{-meta}}){
		next if $f =~/^-/;
		$s->{-meta}->{$f}->{-fldids} ={}
			if !$s->{-meta}->{$f}->{-fldids};
		foreach my $ff (keys %{$s->{-meta}->{$f}->{-fields}}) {
			$s->{-meta}->{$f}->{-fldids}->{$s->{-meta}->{$f}->{-fields}->{$ff}->{fieldId}}
				=$s->{-meta}->{$f}->{-fields}->{$ff}
		}
	}
	if (ref($s->{-metadn})) {
		foreach my $dn (keys %{$s->{-metadn}}) {
			$s->{-metadn}->{$dn} ={fieldName=>$dn, fieldId=>$s->{-metadn}->{$dn}}
				if !ref($s->{-metadn}->{$dn});
			$s->{-metadn}->{$dn}->{fieldName} =$dn
				if !$s->{-metadn}->{$dn}->{fieldName};
			$s->{-metaid}->{$s->{-metadn}->{$dn}->{fieldId}} =$s->{-metadn}->{$dn}
				if $s->{-metadn}->{$dn}->{fieldId}
				&& !$s->{-metaid}->{$s->{-metadn}->{$dn}->{fieldId}};
		}
	}
	if (ref($s->{-metaid})) {
		foreach my $id (keys %{$s->{-metaid}}) {
			$s->{-metaid}->{$id} ={fieldId=>$id, fieldName=>$s->{-metaid}->{$id}}
				if !ref($s->{-metaid}->{$id});
			$s->{-metaid}->{$id}->{fieldId} =$id;
			$s->{-metadn}->{$s->{-metaid}->{$id}->{fieldName}} =$s->{-metaid}->{$id}
				if $s->{-metaid}->{$id}->{fieldName}
				&& !$s->{-metadn}->{$s->{-metaid}->{$id}->{fieldName}};
		}
	}
	# print $s->cpcon($s->dsdump($s->{-metaid})), "\n"; exit(0);
 }
}


sub arsmetamin {	# Minimal ARS metadata ('-meta-min' varfile)
 my $s =shift;		# 	refresh after 'arsmeta'/'connect'
 $s->set(@_);		# 	load instead of 'arsmeta'/'connect'
 local $s->{-cmd} =($s->{-cmd} ? $s->{-cmd} .': ' : '')
	.($s->{-schgen} ? "dumper('" .$s->vfname('meta-min') ."')" : 'arsmetamin()');
 if (ref($s->{-schgen})
 || ($s->{-schgen} && ($s->{-schgen} >1))
 || (!-e $s->vfname('-meta-min'))
	) {
	$s->arsmeta() if !$s->{-meta} ||!scalar(%{$s->{-meta}});
	my $fvs =[stat $s->vfname('-meta-min')]->[9] ||0;
	$fvs =0 if $fvs && ($fvs <([stat $s->vfname('-meta')]->[9]||0));
	$fvs =0 if $fvs && ($fvs <([stat ($^O eq 'MSWin32' ? scalar(Win32::GetFullPathName($0)) : $0)]->[9]||0));
	if (!$fvs) {
		$s->{'-meta-min'} ={};
		foreach my $f (keys %{$s->{-meta}}) {
			foreach my $ff (keys %{$s->{-meta}->{$f}->{-fields}}) {
				my $e =$s->{-meta}->{$f}->{-fields}->{$ff};
				next	if !$e->{'limit'}
					|| !$e->{'limit'}->{'enumLimits'}
					|| !($e->{'limit'}->{'enumLimits'}->{'regularList'} ||$e->{'limit'}->{'enumLimits'}->{'customList'});
				$s->{'-meta-min'}->{$f} ={} if !$s->{'-meta-min'}->{$f};
				$s->{'-meta-min'}->{$f}->{-fields} ={} if !$s->{'-meta-min'}->{$f}->{-fields};
				$e ={%$e};
				delete @$e{'owner','lastChanged', 'timestamp'};
				$s->{'-meta-min'}->{$f}->{-fields}->{$ff} ={%$e};				
			}
		}
		$s->vfstore('-meta-min');
	};
 };
# print do($s->vfname('-meta-min'))||0,' ', $@||'', $s->vfname('-meta-min'),' ', "\n";
 $s->vfload('-meta-min') if !$s->{'-meta-min'};
 if (!$s->{-meta} ||!scalar(%{$s->{-meta}})) {
	$s->{-meta} =$s->{'-meta-min'};
	$s->arsmetaix();
 }
 else {
	foreach my $f (keys %{$s->{'-meta-min'}}) {
		my $fs =$s->{'-meta-min'}->{$f};
		$s->{-meta}->{$f} ={}
			if !$s->{-meta}->{$f};
		foreach my $ff (keys %{$fs->{-fields}}) {
			$s->{-meta}->{$f}->{-fields}->{$ff} ={}
				if !$s->{-meta}->{$f}->{-fields}->{$ff};
			@{$s->{-meta}->{$f}->{-fields}->{$ff}}{keys %{$fs->{-fields}->{$ff}}}
				=values %{$fs->{-fields}->{$ff}};
		}
	}
	$s->arsmetaix()
 }
 delete $s->{'-meta-min'};
 $s;
}


sub ars_errstr {# Last ARS error
	$ARS::ars_errstr
}



sub schema {	# Schema by form name
		# (schema) -> {schema descr}
		# () -> {schemaName=>{descr}}
 $_[1]
 ? $_[0]->{-meta}->{ref($_[1]) ? $_[1]->{schemaName} : $_[1]}
 : $_[0]->{-meta};
}


sub schfld {	# Schema of field
		# (schema, field) -> {field descr}
		# ({schemaName=>name, fieldName=>name}) -> {field descr}
		# (schema) -> {field=>descr}
 ref($_[1])
 ? $_[0]->{-meta}->{$_[1]->{schemaName}}->{-fields}->{$_[1]->{fieldName}}
 : $_[2]
 ? $_[0]->{-meta}->{$_[1]}->{-fields}->{$_[2]}
 : $_[0]->{-meta}->{$_[1]}->{-fields}
}


sub schid {	# Schema info by field id
		# (schema, fieldId) -> {fieldName=>'name', FieldId=>id}
		# () -> rearranged self
 $_[0]->{-metaid}->{$_[2]}
 || $_[0]->{-meta}->{$_[1]}->{-fldids}->{$_[2]}
 || &{$_[0]->{-die}}($_[0]->efmt('Field not found',$_[0]->{-cmd},undef,'schid',$_[1],$_[2]))
}


sub schdn {	# Schema info by field distiguished name
		# (schema, fieldName) -> {fieldName=>'name', FieldId=>id}
 (($_[2] =~/^\d+$/)
	&& ($_[0]->{-metaid}->{$_[2]} 
		|| $_[0]->{-meta}->{$_[1]}->{-fldids}->{$_[2]}))
 || $_[0]->{-metadn}->{$_[2]}
 || $_[0]->{-meta}->{$_[1]}->{-fields}->{$_[2]}
 || &{$_[0]->{-die}}($_[0]->efmt('Field not found',$_[0]->{-cmd},undef,'schdn',$_[1],$_[2]))
}


sub schdi {	# Schema info by field Id
		# (schema, fieldId) -> {fieldName=>'name', FieldId=>id} || undef
 $_[0]->{-metaid}->{$_[2]}
 || $_[0]->{-meta}->{$_[1]}->{-fldids}->{$_[2]}
}


sub schlbls {	# Enum field {values => labels}
		# (schema, fieldId) -> {value=>label,...}
 my($s,$f,$ff) =@_;
 $ff =ref($ff) ? $ff
	: !$s->{-meta} || !$s->{-meta}->{$f} ? return(undef)
	: $ff =~/^\d+$/ ? $s->{-meta}->{$f}->{-fldids}->{$ff}
	: $s->{-meta}->{$f}->{-fields}->{$ff}; 
 if ($ff && !$ff->{-hashOut} && ($ff->{dataType} eq 'enum')) {
	my $et =ref($ff->{'limit'}->{'enumLimits'}) eq 'ARRAY'
		? $ff->{'limit'}->{'enumLimits'}
		: exists $ff->{'limit'}->{'enumLimits'}->{'regularList'}
		? $ff->{'limit'}->{'enumLimits'}->{'regularList'}
		: exists $ff->{'limit'}->{'enumLimits'}->{'customList'}
		? $ff->{'limit'}->{'enumLimits'}->{'customList'}
		: undef;
	if (!$et) {}
	elsif (!ref($et->[0])) {
		$ff->{-hashOut} ={map {($_ => $et->[$_])} (0..$#$et)}
	}
	elsif ((ref($et->[0]) eq 'HASH') && defined($et->[0]->{itemNumber})) {
		$ff->{-hashOut} ={map {($et->[$_]->{itemNumber} => $et->[$_]->{itemName})} (0..$#$et)}
	}
 }
 $ff && $ff->{-hashOut}
}


sub schvals {	# Enum field [values]
		# (schema, fieldId) -> [value,...]
 my($s,$f,$ff) =@_;
 $ff =ref($ff) ? $ff
	: !$s->{-meta} || !$s->{-meta}->{$f} ? return(undef)
	: $ff =~/^\d+$/ ? $s->{-meta}->{$f}->{-fldids}->{$ff}
	: $s->{-meta}->{$f}->{-fields}->{$ff}; 
 if ($ff && !$ff->{-listVals} && ($ff->{dataType} eq 'enum')) {
	my $et =ref($ff->{'limit'}->{'enumLimits'}) eq 'ARRAY'
		? $ff->{'limit'}->{'enumLimits'}
		: exists $ff->{'limit'}->{'enumLimits'}->{'regularList'}
		? $ff->{'limit'}->{'enumLimits'}->{'regularList'}
		: exists $ff->{'limit'}->{'enumLimits'}->{'customList'}
		? $ff->{'limit'}->{'enumLimits'}->{'customList'}
		: undef;
	if (!$et) {}
	elsif (!ref($et->[0])) {
		$ff->{-listVals} =[0..$#$et]
	}
	elsif ((ref($et->[0]) eq 'HASH') && defined($et->[0]->{itemNumber})) {
		$ff->{-listVals} =[map {$et->[$_]->{itemNumber}} (0..$#$et)]
	}
 }
 $ff && $ff->{-listVals}
}



sub strOut {	# Convert field value for output, using '-meta'
		# (schema, fieldId, fieldValue) -> fieldValue
 my($s,$f,$ff,$v) =@_;
 $ff =ref($ff) ? $ff : $ff =~/^\d+$/ ? $s->{-meta}->{$f}->{-fldids}->{$ff} : $s->{-meta}->{$f}->{-fields}->{$ff}; 
 if (!defined($v) ||!$ff ||!$s->{-strFields}) {
 }
 elsif ($ff->{-hashOut}) {
	if (exists($ff->{-hashOut}->{$v})) {
		$v =$ff->{-hashOut}->{$v}
	}
	else {
		# return(&{$s->{-die}}($s->efmt('Could not transate value',$s->{-cmd},undef,'strOut',$f,$ff->{fieldName},$v)))
	}
 }
 elsif ($ff->{dataType} eq 'enum') {
	schlbls(@_);
	$v =strOut(@_) if $ff->{-hashOut};
 }
 elsif ($ff->{dataType} eq 'time') {
	$v =strtime($s,$v)
 }
 $v
}


sub strIn {	# Convert input field value, using '-meta'
		# (schema, fieldId, fieldValue) -> fieldValue
 my($s,$f,$ff,$v) =@_;
 $ff =ref($ff) ? $ff : $ff =~/^\d+$/ ? $s->{-meta}->{$f}->{-fldids}->{$ff} : $s->{-meta}->{$f}->{-fields}->{$ff};
 if (!defined($v) ||!$ff ||!$s->{-strFields}) {
 }
 elsif ($v =~/^\d+$/) {
 }
 elsif ($ff->{-hashIn}) {
	if (exists($ff->{-hashIn}->{$v})) {
		$v =$ff->{-hashIn}->{$v};
	}
	else {
		return(&{$s->{-die}}($s->efmt('Could not transate value',$s->{-cmd},undef,'strIn',$f,$ff->{fieldName},$v)))
	}
 }
 elsif ($ff->{dataType} eq 'enum') {
	my $et =  ref($ff->{'limit'}->{'enumLimits'}) eq 'ARRAY'
		? $ff->{'limit'}->{'enumLimits'}
		: exists $ff->{'limit'}->{'enumLimits'}->{'regularList'}
		? $ff->{'limit'}->{'enumLimits'}->{'regularList'}
		: exists $ff->{'limit'}->{'enumLimits'}->{'customList'}
		? $ff->{'limit'}->{'enumLimits'}->{'customList'}
		: undef;
	if (!$et) {}
	elsif (!ref($et->[0])) {
		$ff->{-hashIn} ={map {($et->[$_] => $_)} (0..$#$et)};
		$v =strIn(@_);
	}
	elsif ((ref($et->[0]) eq 'HASH') && defined($et->[0]->{itemNumber})) {
		$ff->{-hashIn} ={map {($et->[$_]->{itemName} => $et->[$_]->{itemNumber})} (0..$#$et)};
		$v =strIn(@_);
	}
	else {
		$et =undef
	}
	return(&{$s->{-die}}($s->efmt('Could not transate value',$s->{-cmd},undef,'strIn',$f,$ff->{fieldName},$v)))
		if $et && ($v !~/^\d+$/);
 }
 elsif ($ff->{dataType} eq 'time') {
	$v =timestr($s,$v);
 }
 $v
}


sub lsflds {	# List fields from '-meta'
		# (additional field options)
 my ($s, @a) =@_;
 @a =('fieldLblc', 'helpText') if !@a;
 unshift @a, 'fieldName', 'fieldId', 'dataType', 'option', 'createMode';
 map {	my $f =$_;
	$f =~/^-/
	? ()
	: map {	my $ff =$s->{-meta}->{$f}->{-fields}->{$_};
		join("\t", $f
			, $ff->{option} && ($ff->{option} == 4) ? 'ro' : ()
			, (map {!defined($ff->{$_})
				? ''
				: $_ eq 'option'
				? (!$ff->{$_} ? '' : $ff->{$_} == 4 ? 'r' : $ff->{$_} == 2 ? 'o' : $ff->{$_} == 1 ? 'm' : '')
				: $ff->{$_}
				} @a[0..$#a-1])
			, defined($ff->{$a[$#a]}) ? $ff->{$a[$#a]} : ())
		} sort keys %{$s->{-meta}->{$f}->{-fields}}
	} sort keys %{$s->{-meta}}
}


sub query {	# ars_GetListEntry / ars_LoadQualifier
 #		(-clause=>val) -> list
 #		(...-for=>sub{}) -> self
 #		Field Ids translated using -metadn/-metaid
 # -from ||-form ||-schema => schema name
 # -where || -query ||-qual => search condition
 #		Syntax:
 #		'fieldId' || 'fieldName' - fields
 #		"string value" - strings
 #		digits - numeric value, number of seconds as date value
 #		strIn(form, fieldName, value) - to encode value for '-where'
 #
 # -fields => [{fieldId=>1, columnWidth=>9, separator=>"\t"},...
 #		,[{fieldName=>name, width=>9},...
 #		,[{field=>name|id, width=>9},...] # 128 bytes limit strings
 # ||-fields => [fieldId | fieldName,...]	# using ars_GetListEntryWithFields()
 # ||-fields => '*' | 1 | '*-$', -xfields=>sub{} || [fieldName| fieldId,...]
 # ||-fetch => '*' | 1 | [fieldId|fieldName,...] # using ars_GetEntry() for each record
 # -order ||-sort => [fieldId, (1||2),...] # 1 - asc, 2 - desc
 #			[..., fieldName, field=>'desc', field=>'asc',...]
 # -limit ||-max => maxRetrieve
 # -first ||-start => firstRetrieve
 # -for ||-foreach => sub(self, form, id|string, ?{record}){die "last\n", die "next\n"} -> self
 # ?-echo=>1
 #
 # ars_GetListEntry(ctrl, schema, qualifier, maxRetrieve=0, firstRetrieve=0,...)
 #		..., getListFields, sortList,... 
 # ars_LoadQualifier(ctrl, schema, qualifier string)
 #
 # Using the advanced search bar:
 # 'Currency Field.VALUE'	'Currency Field' = $NULL$
 # ??? BookValue=> {conversionDate=> 1090544110, currencyCode=> 'USD', funcList=> [{currencyCode=> 'USD', value=> '0.00'}, {currencyCode=> 'EUR', value=> ''}, {currencyCode=> 'GBP', value=> ''}, {currencyCode=> 'JPY', value=> ''}, {currencyCode=> 'CAD', value=> ''}], value=> '0.00'}
 # 'Status History.Fixed.TIME' < "07/01/99"
 # 'Create date' > "10:00:00"
 #
 my $s =shift;
 my %a =@_;
 my $f =$a{-schema} ||$a{-form} ||$a{-from};
 my $c =$a{-for} ||$a{-foreach};

 $a{-fields} =	$a{-fields} =~/^-\$/
		? [map {  my $ff =$s->{-meta}->{$f}->{-fields}->{$_};
			!$ff->{dataType} || !$ff->{fieldId}
			|| ($ff->{dataType} =~/^(currency|trim|control|table|column|page|attach)/)
			|| ($ff->{fieldId} eq '15') # 'Status-History' # ars_GetListEntryWithFields() -> [ERROR] (ORA-00904: "C15": invalid identifier) (ARERR #552)
			|| (!$a{-xfields} ? 0 : ref($a{-xfields}) eq 'CODE' ? &{$a{-xfields}}($s, $ff) :  grep {($_ eq $ff->{fieldId}) || ($_ eq $ff->{fieldName})} @{$a{-xfields}})
			? ()
			: ($ff->{fieldId})
			} sort keys %{$s->{-meta}->{$f}->{-fields}}]
		: [map {  my $ff =$s->{-meta}->{$f}->{-fields}->{$_};
			!$ff->{dataType} || !$ff->{fieldId}
			|| ($ff->{dataType} =~/^(trim|control|table|column|page|attach)/)
			|| ($ff->{fieldId} eq '15') # 'Status-History' # ars_GetListEntryWithFields() -> [ERROR] (ORA-00904: "C15": invalid identifier) (ARERR #552)
			|| (!$a{-xfields} ? 0 : ref($a{-xfields}) eq 'CODE' ? &{$a{-xfields}}($s, $ff) :  grep {($_ eq $ff->{fieldId}) || ($_ eq $ff->{fieldName})} @{$a{-xfields}})
			? ()
			: ($ff->{fieldId})
			} sort keys %{$s->{-meta}->{$f}->{-fields}}]
		if $a{-fields} && !ref($a{-fields});

 $a{-fetch} =1	if $a{-fields} && !ref($a{-fields});
 delete $a{-fields}	if $a{-fetch};

 local $s->{-cmd} ="query(" .join(', ',map {!defined($a{$_}) ? () : ref($a{$_}) ? "$_=>" .dsquot($s,$a{$_}) : ("$_=>" .strquot($s,$a{$_}))
		} qw(-schema -form -from -fields -fetch -qual -query -where -sort -order -limit -max -maxRetrieve -first -start))
		.")";

 my $fl = ref($a{-fetch})
	? [map {/^\d+$/ ? $_ : schdn($s,$f,$_)->{fieldId}} @{$a{-fetch}}]
	: $a{-fields} && ref($a{-fields}->[0])
	? [map {ref($_)
			? {fieldId=>$_->{fieldId} ||schdn($s,$f, $_->{fieldName} ||$_->{field})->{fieldId}
				, separator=>$_->{separator} ||"\t"
				, columnWidth=>$_->{columnWidth} ||$_->{width} ||10
				}
			: {fieldId=>/^\d+$/ ? $_ : schdn($s,$f,$_)->{fieldId}
				, separator=>"\t"
				, columnWidth=>10
				}
			} @{$a{-fields}}]
	: $a{-fields}
	? [map {/^\d+$/ ? $_ : schdn($s,$f,$_)->{fieldId}} @{$a{-fields}}]
	: [];
 my @fs;
	{my ($v, $x, @r) =($a{-sort} ||$a{-order});
	@fs =	$v
		? (map {if (!$x) {$x =$_; @r=()}
			elsif(/^(desc|2)$/) {@r =($x=~/^\d+$/ ? $x : schdn($s,$f,$x)->{fieldId}, 2); $x =undef}
			else {@r=($x =~/^\d+$/ ? $x : schdn($s,$f,$x)->{fieldId},1); $x=undef if /^(asc|1)$/}
			@r} @$v)
		: ();
	push @fs, $x =~/^\d+$/ ? $x : schdn($s,$f,$x)->{fieldId}, 1
		if $x}
 my $q =$s->_qsubst('',$a{-qual} ||$a{-query} ||$a{-where}, $f);
 $s->{-cmd} .=": subst(-from=>'$f'"
		.(@$fl ? ',-fields=>' .join(', ', map {ref($_) ? "'" .$_->{fieldId} ."'(" .$_->{columnWidth} .")" : "'$_'"
			} @$fl) : '')
		.($q ? ",-where=>$q" : '')
		.(@fs ? ',-order=>' .join(', ', map {"'$_'"} @fs) : '')
		.")" 
		if 0;
 $q =ARS::ars_LoadQualifier($s->{-ctrl}, $f, $q);
 return(&{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd})))
	if !$q;
 $s->{-cmd} .=": qual". $s->dsquot(ARS::ars_perl_qualifier($s->{-ctrl}, $q))
	if 0;

 print $s->cpcon(join(";\n", split /\):\s/, $s->{-cmd})), "\n"
	if $s->{-echo} ||$a{-echo};

 if ($c && $a{-fields} && !ref($a{-fields}->[0])) {
	my $id;
	local $_;
	foreach my $e (ARS::ars_GetListEntryWithFields($s->{-ctrl}, $f, $q
		, $a{-limit} ||$a{-max} ||$s->{-maxRetrieve} ||0
		, $a{-first} ||$a{-start} ||0
		, $fl
		, @fs)) {
		if (!ref($e)) {
			$_ =$id =$e
		}
		elsif (!defined(eval{&$c($s, $f, $_ =$id, entryOut($s, $f, $e))}) && $@) {
			last if $@ =~/^last[\r\n]*$/;
			next if $@ =~/^next[\r\n]*$/;
			return(&{$s->{-die}}($s->efmt($@,$s->{-cmd},'eval(-for)')));
		}
	}
	return(&{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd},'undef','ars_GetListEntryWithFields')))
		if !defined($id) && $ARS::ars_errstr;
	return($s);
 }
 elsif ($c) {
	my $i =undef;
	local $_ ='';
	foreach my $e (ARS::ars_GetListEntry($s->{-ctrl}, $f, $q
		, $a{-limit} ||$a{-max} ||$s->{-maxRetrieve} ||0
		, $a{-first} ||$a{-start} ||0
		, $fl
		, @fs)) {
		if ($i)	{
			$i =0;
			$_ =$_ .($fl->[0]->{separator}) .$e
				if $a{-fields};
		}
		else {
			$i =1;
			$_ =$e;
			next
		}
		if (!defined(eval{&$c($s, $f, $_
			, $a{-fetch}
				? $s->entry(-from=>$f, -id=>$_
					, ref($a{-fetch}) ? (-fields => $a{-fetch}) : ())
				: ())}) && $@) {
			last if $@ =~/^last[\r\n]*$/;
			next if $@ =~/^next[\r\n]*$/;
			return(&{$s->{-die}}($s->efmt($@,$s->{-cmd},'eval(-for)')));
		}
	}
	return(&{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd},undef,'ars_GetListEntry')))
		if !defined($i) && $ARS::ars_errstr;
	return($s)
 }
 elsif ($a{-fields} && !ref($a{-fields}->[0])) {
	my @r =ARS::ars_GetListEntryWithFields($s->{-ctrl}, $f, $q
		, $a{-limit} ||$a{-max} ||$s->{-maxRetrieve} ||0
		, $a{-first} ||$a{-start} ||0
		, $fl
		, @fs);
	if (@r) {
		my @rr;
		for (my $i =0; $i <$#r; $i +=2) {
			push @rr, entryOut($s, $f, $r[$i+1])
		}
		return(@rr)
	}
	return(&{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd},undef,'ars_GetListEntryWithFields')))
		if $ARS::ars_errstr;
	return(())
 }
 else {
	my @r =ARS::ars_GetListEntry($s->{-ctrl}, $f, $q
		, $a{-limit} ||$a{-max} ||$s->{-maxRetrieve} ||0
		, $a{-first} ||$a{-start} ||0
		, $fl
		, @fs);
	if (@r) {
		my @rr;
		if ($a{-fields}) {
			for (my $i =0; $i <$#r; $i +=2) {
				push @rr, $r[$i]
					.($fl->[0]->{separator})
					. $r[$i+1]
			}
		}
		elsif ($a{-fetch}) {
			for (my $i =0; $i <$#r; $i +=2) {
				push @rr
				, $s->entry(-from=>$f, -id=>$r[$i]
					, ref($a{-fetch}) ? (-fields=>$a{-fetch}) : ())
			}
		}
		else {
			for (my $i =0; $i <$#r; $i +=2) { push @rr, $r[$i] }
		}
		return(@rr)
	}
	return(&{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd},undef,'ars_GetListEntry')))
		if $ARS::ars_errstr;
	return(())
 }
}


sub _qsubst {	# query condition string substitutions
		# (''|char, expr string, form) -> translated
 my ($s, $c, $q, $f) =@_;
 return($q) if !defined($q) ||($q eq '');
 my $r ='';
 if (!$c) {
	while ($q =~/^(.*?)(['"]|#[\w]+[\w\d]+\()(.*)/) {
		$r .=$1;
		$q  =$3;
		if (!defined($q)) {
			$q =''
		}
		elsif (substr($2,0,1) eq "'") {
			if ($q =~/^([^']+)'(.*)/) {
				$q =$2;
				my $n =$1;
				$r .="'" .($n =~/^\d+$/ ? $n : schdn($s,$f,$n)->{fieldId}) ."'";
			}
			else {
				$r .="'"
			}
		}
		else {
			$r .=_qsubst($s, $2, $q, $f)
		}
	}
	$r .=$q if defined($q);
 }
 elsif ($c eq '(') {
	$r =$c;
	while ($q =~/^(.*?)([()'"])(.*)/) {
		$q  =$3;
		$r .=$1;
		if ($2 eq ')')	{$r .=$2; last}
		else		{$r .=_qsubst($s, $2, $q, $f)}
	}
	$_[2] =$q;
 }
 elsif ($c =~/['"]/) {
	my $cq =$s->strquot($c);
	$cq =substr($cq,1,-1);
	$r =$c;
	while ($q =~/^(.*?)(\Q$c\E|\Q$cq\E)(.*)/) {
		$q =$3;
		$r .=$1 .$2;
		last if $2 eq $c;
	}
	$_[2] =$q;
 }
 elsif ($c eq ',') {
	my @r;
	while ($q =~/^(.*?)(['"(]|\Q$c\E)(.*)/i) {
		$q =$3;
		$r .=$1;
		if ($2 eq $c) {
			push @r, ($r =~/^\s*(.*?)\s*$/ ? $1 : $r);
			$r ='';
		}
		else {
			$r .=_qsubst($s, $2, $q, $f);
		}
	}
	$r .=$q;
	push @r, ($r =~/^\s*(.*?)\s*$/ ? $1 : $r) if $r ne '';
	return(@r)
 }
 else {
	$r =$c .$q
 }
 $r
}


sub entry {	# ars_GetEntry
		# (-from=>form, -id=>entryId, ?-for=>{}, ?-fields=>[internalId,...])
		#	-> {fieldName => value}
 #		# Field Ids translated using -schdn/-schid
 # -from ||-form ||-schema => schema name
 # -id => entryId
 # -fields => [internalId, fieldName,...]
 # -for => {} # steady hash to store each entry fetched
 # ?-echo=>1
 #
 # ars_GetEntry(ctrl,schema,entry_id,...) -> (internalId => value,...)
 # no ars_GetEntryBLOB(ctrl,schema,entry_id,field_id,locType,locFile)
 # no ars_EncodeDiary(diaryEntryHash1, ... diaryEntryHashN)
 # encoded 'Status-History'
 # decoded 'diary'
 #
 my ($s, %a) =@_;
 my $f =$a{-schema} ||$a{-form} ||$a{-from};
 print $s->cpcon("entry(-form=>'$f',-id=>'$a{-id}')\n")
	if $s->{-echo} || $a{-echo};
 my %r =ARS::ars_GetEntry($s->{-ctrl},$f,$a{-id}
	,$a{-fields} 
		? (map {/^\d+$/ ? $_ : schdn($s, $f, $_)->{fieldId}} @{$a{-fields}})
		: ()
	);
 if (%r) {
	my $rr =$a{-for} ||{};
	undef(@{$rr}{keys %$rr}) if %$rr;
	# @{$rr}{map {schid($s,$f,$_)->{fieldName}} keys %r} =values %r;
	# return($rr);
	local $_;
	local $s->{-cmd} =($s->{-cmd} ? $s->{-cmd} .': ' : '') ."entry(-form=>'$f',-id=>'$a{-id}')";
	foreach my $id (keys %r) {
		my $ff =schdi($s,$f,$id);
		if ($ff) {
			$rr->{$ff->{fieldName}} 
				= !$s->{-strFields}
				? $r{$id}
				: $ff->{strOut}
				? &{$ff->{strOut}}($s,$f,$ff,$_=$r{$id})
				: strOut($s,$f,$id,$r{$id})
		}
		else {
			$rr->{$id} =$r{$id}
		}
	}
	return($rr)
 }
 return($ARS::ars_errstr
	? &{$s->{-die}}($s->efmt($ARS::ars_errstr,$s->{-cmd},undef,'entry',-form=>$f,-id=>$a{-id}))
	: {})
}


sub entryOut {	# Format entry hash ref for output
		# (schema, entry, ?sample) -> entry
 my ($s, $f, $r, $rr) =@_;
 if ($rr) {
	undef(@{$rr}{keys %$rr}) if %$rr;
 }
 else {
	$rr ={}
 }
 local $_;
 foreach my $id (keys %$r) {
	my $ff =schdi($s,$f,$id);
	my $v  =$r->{$id};
	if ($ff) {
		$rr->{$ff->{fieldName}} 
			= !$s->{-strFields}
			? $r->{$id}
			: $ff->{strOut}
			? &{$ff->{strOut}}($s,$f,$ff,$_=$v)
			: strOut($s,$f,$id,$v);
	}
	else {
		$rr->{$id} =$r->{$id}
	}
 }
 $rr
}


sub entryDif {	# Diff hash refs
		# ({old}, {new}, exclude empty) -> {to update}
 my($s, $ds1, $ds2, $ee) =@_;
 return(undef) if (ref($ds1) ||'') ne (ref($ds2) ||'');
 return(undef) if (ref($ds1) ||'') ne 'HASH';
 my ($r, $rr) =({});
 foreach my $k (keys %$ds2) {
	next if !defined($ds1->{$k}) && !defined($ds2->{$k});
	next if (ref($ds1->{$k}) && ref($ds2->{$k}))
		&& !dscmp($s,$ds1,$ds2);
	next if (defined($ds1->{$k}) && defined($ds2->{$k}))
		&& ($ds1->{$k} eq $ds2->{$k});
	next if $ee && (!defined($ds2->{$k}) ||($ds2->{$k} eq ''))
		&& (!defined($ds1->{$k}) ||($ds1->{$k} eq ''));
	$r->{$k} =$ds2->{$k}; $rr =1;
 }
 $rr ? $r : undef
}


sub entryNew {	# New {field => value}
		# (-form=>form, field=>value,...) -> {field=>value,...}
		# ?'Incident Number'=>1 for 'HPD:Help Desk'
 my ($s, %a) =@_;
 my $f =$a{-schema} ||$a{-form} ||$a{-into} ||$a{-for};
 delete @a{qw(-schema -form -from -into -for)};
 local $_;
 local $s->{-cmd} =($s->{-cmd} ? $s->{-cmd} .': ' : '') ."entryNew(-form=>'$f'," 
		.join(',', map {!defined($a{$_}) 
			? "$_=>undef"
			: ref($a{$_})
			? ("$_=>" .dsquot($s, $a{$_}))
			: ("$_=>" .strquot($s, $a{$_}))
			} sort keys %a)
		.')';
 foreach my $k (%{$s->{-meta}->{$f}->{-fields}}) {
	my $ff =$s->{-meta}->{$f}->{-fields}->{$k};
	next	if !$ff
		|| exists($a{$k})
		|| ((!defined($ff->{defaultVal}) || ref($ff->{defaultVal}))
		   && !$s->{-metaid}->{$ff->{fieldId}}->{defaultVal});
	$a{$k} =defined($s->{-metaid}->{$ff->{fieldId}}->{defaultVal})
		? $s->{-metaid}->{$ff->{fieldId}}->{defaultVal}
		: $ff->{defaultVal};
	$a{$k} =$s->{-metaid}->{$ff->{fieldId}}->{strOut}
		? &{$s->{-metaid}->{$ff->{fieldId}}->{strOut}}($s,$f,$s->{-metaid}->{$ff->{fieldId}},$_=$a{$k})
		: strOut($s, $f, $ff->{fieldId},$_=$a{$k})
		if $s->{-strFields};
 }
 if ($f eq 'HPD:Help Desk') {
	if ($a{'Incident Number'} && (length($a{'Incident Number'}) ==1)) {
		$a{'Incident Number'} =$s->entryIns(-form=>'HPD:CFG Ticket Num Generator', 'DataTags'=>'za')
	}
	elsif (defined($a{'Incident Number'}) && !$a{'Incident Number'}) {
		delete $a{'Incident Number'}
	}
 }
 \%a
}


sub entryIns {	# ars_CreateEntry
		# (-form=>form, field=>value) -> id
		# ?-echo=>1
		# ?'Incident Number'=>1 for 'HPD:Help Desk'
 my ($s, %a) =@_;
 my $f =$a{-schema} ||$a{-form} ||$a{-into};
 my $r;
 print $s->cpcon("entryIns(-form=>'$f')\n")
	if $s->{-echo} ||$a{-echo};
 delete @a{qw(-schema -form -from -into -echo)};
 local $_;
 local $s->{-cmd} =($s->{-cmd} ? $s->{-cmd} .': ' : '') ."entryIns(-form=>'$f'," 
		.join(',', map {!defined($a{$_}) 
			? "$_=>undef"
			: ref($a{$_})
			? ("$_=>" .dsquot($s, $a{$_}))
			: ("$_=>" .strquot($s, $a{$_}))
			} sort keys %a)
		.')';
 %a = map {	my ($k, $v) =($_, $a{$_});
		if ($k !~/^\d+$/) {
			my $ff =schdn($s,$f,$k);
			$k =$ff->{fieldId};
			$v =$ff->{strIn}
			   ? &{$ff->{strIn}}($s,$f,$ff,$_=$v)
			   : strIn($s,$f,$k,$v)
				if $s->{-strFields};
		}
		($k => $v)
		} keys %a;
 delete $s->{-entryNo};
 if ($f eq 'HPD:Help Desk') {
	my $ii=schdn($s,$f,'Incident Number')->{fieldId};
	$a{$ii} =$s->entryIns(-form=>'HPD:CFG Ticket Num Generator', 'DataTags'=>'za')
		if length($a{$ii}) <2;
	$s->{-entryNo} =$a{$ii};
	$r =ARS::ars_CreateEntry($s->{-ctrl}, $f, %a)
 }	
 else {
	$r =$s->{-entryNo} =ARS::ars_CreateEntry($s->{-ctrl}, $f, %a)
 }
 if (!$r) {
	my $t =$s->efmt($ARS::ars_errstr,$s->{-cmd});
	return(&{$s->{-die}}($t))	if !$r &&  $ARS::ars_errstr;
	# warn($t)			if !$r && !$ARS::ars_errstr;
 }
 $r ||$s
}


sub entryUpd {	# ars_SetEntry(ctrl,schema,entry_id,getTime,...)
		# (-form=>form, -id=>entryId, field=>value) -> id
		# ?-echo=>1
 #
 # ??? ARMergeEntry()/ars_MergeEntry(ctrl, schema, mergeType, ...)
 # ??? ars_EncodeDiary(diaryEntryHash1, ... diaryEntryHashN)
 #
 my ($s, %a) =@_;
 my $f =$a{-schema} ||$a{-form} ||$a{-into};
 my $id=$a{-id};
 print $s->cpcon("entryUpd(-form=>'$f',-id=>'$id')\n")
	if $s->{-echo} ||$a{-echo};
 delete @a{qw(-schema -form -from -into -id -echo)};
 local $_;
 local $s->{-cmd} =($s->{-cmd} ? $s->{-cmd} .': ' : '') 
	."entryUpd(-form=>'$f',-id=>'$id',"
	.join(',', map {!defined($a{$_}) 
			? "$_=>undef"
			: ref($a{$_})
			? ("$_=>" .dsquot($s, $a{$_}))
			: ("$_=>" .strquot($s, $a{$_}))
			} sort keys %a)
	.')';
 %a = map {	my ($k, $v) =($_, $a{$_});
		if ($k !~/^\d+$/) {
			my $ff =schdn($s,$f,$k);
			$k =$ff->{fieldId};
			$v =$ff->{strIn}
			   ? &{$ff->{strIn}}($s,$f,$ff,$_=$v)
			   : strIn($s,$f,$k,$v)
				if $s->{-strFields}
		}
		($k => $v)
		} keys %a;
 my $r =ARS::ars_SetEntry($s->{-ctrl}, $f, $id, 0, %a);
 return(&{$s->{-die}}($s->efmt($ARS::ars_errstr, $s->{-cmd})))
	if !$r && $ARS::ars_errstr;
 $id
}


sub entryDel {	# ars_DeleteEntry
		# (-form=>form, -id=>entryId) -> id
		# ?-echo=>1
 my ($s, %a) =@_;
 my $f =$a{-schema} ||$a{-form} ||$a{-from} ||$a{-into};
 my $id=$a{-id};
 print $s->cpcon("entryDel(-form=>'$f',-id=>'$id')\n")
	if $s->{-echo} ||$a{-echo};
 delete @a{qw(-schema -form -from -into -id -echo)};
 my $r =ARS::ars_DeleteEntry($s->{-ctrl}, $f, $id);
 return(&{$s->{-die}}($s->efmt($ARS::ars_errstr
		,"entryDel(-form=>'$f',-id=>'$id')")))
	 if !$r && $ARS::ars_errstr;
 $id
}


sub dbi {	# DBI connection object
 return($_[0]->{-dbi}) if $_[0]->{-dbi};
 dbiconnect(@_)
}


sub dbiconnect {# DBI connect to any database
		# (-dbiconnect=>[]) -> dbi object
 set(@_);
 set($_[0],-die=>'Carp') if !$_[0]->{-die};
 print $_[0]->cpcon("dbiconnect()\n")
	if $_[0]->{-echo};
 eval('use DBI; 1') ||return(&{$_[0]->{-die}}($_[0]->efmt('No DBI')));
 $_[0]->{-dbi} =DBI->connect(ref($_[0]->{-dbiconnect}) ? @{$_[0]->{-dbiconnect}} : $_[0]->{-dbiconnect})
	|| &{$_[0]->{-die}}($_[0]->efmt(DBI->errstr,undef,undef,'dbiconnect') ."\n");
}


sub dbiquery {	# DBI query
		# (dbi query args) -> dbi cursor object
		# (-echo=>1,...)
 my($s, @q) =@_;
 my(%a); while ($#q && ($q[0] =~/^-/)) {$a{$q[0]} =$q[1]; shift @q; shift @q};
 print $s->cpcon("dbiquery($q[0])\n")
	if $s->{-echo} ||$a{-echo};
 my $op =$s->{-dbi}->prepare(@q)
	|| return(&{$s->{-die}}($s->efmt($s->{-dbi}->errstr, undef, undef, 'dbiprepair',@q)));
 $op->execute()
	|| return(&{$s->{-die}}($s->efmt($s->{-dbi}->errstr, undef, undef, 'dbiexecute',@q)));
 $op;
}


sub dbido {	# DBI do
		# (dbi do args) -> dbi cursor object
		# (-echo=>1,...)
 my($s, @q) =@_;
 my(%a); while ($#q && ($q[0] =~/^-/)) {$a{$q[0]} =$q[1]; shift @q; shift @q};
 print $s->cpcon("dbiquery($q[0])\n")
	if $s->{-echo} ||$a{-echo};
 $s->{-dbi}->do(@q)
	|| &{$s->{-die}}($s->efmt($s->{-dbi}->errstr, undef, undef, 'dbido',@q));
}


sub dbierrstr {	# Last DBI error
 $_[0]->{-dbi}->errstr
}


sub cgi {	# CGI object
 return($_[0]->{-cgi}) if $_[0]->{-cgi};
 cgiconnect(@_)
}


sub cgiconnect {# Connect CGI
 my $s =shift;
 no warnings;
 local $^W =0; 
 $ENV{HTTP_USER_AGENT} =$ENV{HTTP_USER_AGENT}||'';
 $ENV{PERLXS} ='PerlIS' if !$ENV{PERLXS} && ($^O eq 'MSWin32') && $0 =~/[\\\/]perlis\.dll$/i;
 eval('use CGI; 1')
	||return(&{$s->{-die}}($s->efmt('No CGI')));
 $s->{-cgi} =$CGI::Q =$CGI::Q =eval{CGI->new(@_)}
	||return($s->{-die}
		? &{$s->{-die}}($s->efmt($@, undef, undef, 'cgi'))
		: CORE::die($s->efmt($@, undef, undef, 'cgi')));
 $s->set(-die=>'CGI::Carp fatalsToBrowser') if !$s->{-die};
 return(&{$s->{-die}}($s->efmt($s->{-cgi}->{'.cgi_error'}, undef, undef, 'cgi')))
	if $s->{-cgi}->{'.cgi_error'};
 if (1) {	# parse parameters
		# __C_ change(d), 
		# __O_ open, __L_ listbox choise, __S_ set, __X_ close
		# __P_ previous value
		# __B_ button for javascript
	foreach my $p ($s->{-cgi}->param) {
		if ($p =~/^(.+?)__S_$/) {
			$s->{-cgi}->param($1, $s->{-cgi}->param("$1__L_"));
			$s->{-cgi}->param("$1__C_", 1);
			$s->{-cgi}->delete("$1__L_");
		}
		elsif ($p =~/^(.+?)__X_$/) {
			if (defined($s->{-cgi}->param("$1__P_"))) {
				$s->{-cgi}->param($1, $s->{-cgi}->param("$1__P_"));
			}
			else {
				$s->{-cgi}->delete($1);
			}
			$s->{-cgi}->delete("$1__L_");
		}
	}
	foreach my $p ($s->{-cgi}->param) {
		if ($p =~/^(.+?)__L_$/) {
		#	$s->{-cgi}->param($1, $s->{-cgi}->param("$1__L_"));
		#	$s->{-cgi}->param("$1__C_", 1);
			$s->{-cgi}->delete("$1__L_");
		}
	}
 }
 $s->{-cgi}
}


sub cgipar {	# CGI parameter
 $_[0]->{-cgi}->param(@_[1..$#_])
}


sub cgiurl {	# CGI script URL
 local $^W =0;	# $ENV{PATH_INFO}
 if ($#_ >0) {
	my $v =($_[0]->{-cgi}||$_[0]->cgi)->url(@_[1..$#_]);
	if ($v) {}
	elsif (!($ENV{PERLXS} ||(($ENV{GATEWAY_INTERFACE}||'') =~/PerlEx/))) {}
	elsif (($#_ >2) ||(($#_ ==2) && !$_[2])) {}
	elsif ($_[1] eq '-relative') {
		$v =$ENV{SCRIPT_NAME};
		$v =$1 if $v =~/[\\\/]([^\\\/]+)$/;
	}
	elsif ($_[1] eq '-absolute') {
		$v =$ENV{SCRIPT_NAME}
	}
	return($v)
 }
 else {	
	# MSDN: "GetServerVariable (ISAPI Extensions)"
	# ms-help://MS.MSDNQTR.v90.en/wcecomm5/html/wce50lrfGetServerVariableISAPIExtensions.htm
	# http:// $ENV{HTTP_HOST} : $ENV{SERVER_PORT} / ($ENV{PATH_INFO} | $ENV{SCRIPT_NAME})
	# + $ENV{QUERY_STRING}
	my $v =($_[0]->{-cgi}||$_[0]->cgi)->url();
	if ($ENV{PERLXS} ||(($ENV{GATEWAY_INTERFACE}||'') =~/PerlEx/)) {
		$v .= (($v =~/\/$/) ||($ENV{SCRIPT_NAME} =~/^\//) ? '' : '/')
			.$ENV{SCRIPT_NAME}
			if ($v !~/\w\/\w/) && $ENV{SCRIPT_NAME};
	}
	return($v)
 }
}


sub cgitext {	# CGI textarea field
 $_[0]->{-cgi}->textarea(@_[1..$#_])
	# -default=>$v, -override=>1
}


sub cgistring {	# CGI string field
 $_[0]->{-cgi}->textfield(@_[1..$#_])
}


sub cgiselect {	# CGI selection field composition
		# -onchange=>1 reloads form
 my ($s, %a) =@_;
 my $cs =$a{-onchange} && (length($a{-onchange}) ==1);
 ($cs
 ? '<input type="hidden" name="' .$a{-name} .'__C_" value="" />'
 : '')
 .$s->{-cgi}->popup_menu(%a
	, $a{-labels} && !$a{-values}
	? (-values => do{use locale; [sort {$a{-labels}->{$a} cmp $a{-labels}->{$b}} keys %{$a{-labels}}]})
	: ()
	, $cs
	? (-onchange => '{window.document.forms[0].' .$a{-name} .'__C_.value="1"; window.document.forms[0].submit(); return(false)}')
	: ()
	)
 .( $cs && ($a{-onchange}=~/^\d/) && $s->{-cgi}->param($a{-name} .'__C_')
  ? '<script for="window" event="onload">window.document.forms[0].' .$a{-name} .'.focus()</script>'
  : '')
}


sub cgiddlb {	# CGI drop-down listbox field composition
		# -strict=> - disable text edit, be alike cgiselect
 my ($s, %a) =@_;
 $s->cgi();
 my $n =$a{-name};
 my $nl="${n}__L_";
 my $av=sub{	return($a{-values}) if $a{-values};
		use locale;
		$a{-values} =[
			  $a{-labels0}
			? sort {(defined($a{-labels0}->{$a}) ? $a{-labels0}->{$a} : '') 
			cmp (defined($a{-labels0}->{$b}) ? $a{-labels0}->{$b} : '')
				} keys %{$a{-labels0}}
			: ()
			, (sort {(defined($a{-labels}->{$a}) ? $a{-labels}->{$a} : '') 
			cmp (defined($a{-labels}->{$b}) ? $a{-labels}->{$b} : '')
				} keys %{$a{-labels}})
			, $a{-labels1}
			? sort {(defined($a{-labels1}->{$a}) ? $a{-labels1}->{$a} : '') 
			cmp (defined($a{-labels1}->{$b}) ? $a{-labels1}->{$b} : '')
				} keys %{$a{-labels1}}
			: ()
				];
		foreach my $e ('-labels0','-labels1') {
			next if !$a{$e};
			foreach my $k (keys %{$a{$e}}) {
				$a{-labels}->{$k} =$a{$e}->{$k}
			}
		}
		$a{-values}
		};
 my $ac=$a{-class} ? ' class="' .$a{-class} .'"' : '';
 my $as=$a{-style} ? ' style="' .$a{-style} .'"' : '';
 my $aw=$a{-size} ||80;
 my $v =!defined($s->{-cgi}->param($n)) ||$a{-override}
	? $a{-default}
	: $s->{-cgi}->param($n);
    $v =&$av()->[0]
		if $a{-strict} && (!defined($v) || !grep /^\Q$v\E$/, @{&$av()});
    $s->{-cgi}->param($n, defined($v) ? $v : '');
 my $ek =$s->{-cgi}->user_agent('MSIE') ? 'window.event.keyCode' : 'event.which';
 my $fs =sub{
	'{var k;'
	."var l=window.document.forms[0].$nl;"
	."if(l.style.display=='none'){"
	.($_[0] eq '4' ? '' : 'return(true)') .'}else{'
	.(!$_[0]	# onkeypess - input
	? "if (String.fromCharCode($ek) ==\"\\r\") {${n}__S_.focus(); ${n}__S_.click(); return(true)}; k=window.document.forms[0].$n.value +String.fromCharCode($ek);"
	: $_[0] eq '1'	# onkeypess - list -> input (first char)
	? "if (String.fromCharCode($ek) ==\&quot;\\r\&quot;) {${n}__S_.focus(); ${n}__S_.click(); return(true)}; window.document.forms[0].$n.focus(); k=window.document.forms[0].$n.value =String.fromCharCode($ek); "
	: $_[0] eq '2'	# onkeypess - list -> prompt (selected char)
	# ? "k=prompt('Enter search string',String.fromCharCode($ek));"
	? "if (String.fromCharCode($ek) ==\&quot;\\r\&quot;) {${n}__S_.focus(); ${n}__S_.click(); return(true)}; k =String.fromCharCode($ek); for (var i=0; i <l.length; ++i) {if (l.options.item(i).value.toLowerCase().indexOf(k)==0 || l.options.item(i).text.toLowerCase().indexOf(k)==0){l.selectedIndex =i; break;}}; var q=prompt('Continue search string',''); k=q ? k +q : q; "
	: $_[0] eq '3'	# button - '..'
	? "k=prompt('Enter search substring',''); $nl.focus();"
	: $_[0] eq '4'	# onload - document
	? "k=window.document.forms[0].$n.value; window.document.forms[0].$nl.focus();"
	: ''
	)
	.'if(k){'
	.'k=k.toLowerCase();'
	.'for (var i=0; i <l.length; ++i) {'
	.($_[0] eq '4'
	? 'if (l.options.item(i).value.toLowerCase() ==k){'
	: $s->{-cgi}->user_agent('MSIE')
	? "if (l.options.item(i).innerText !='' ? l.options.item(i).innerText.toLowerCase().indexOf(k)"
		.($_[0] eq '3' ?'>=' :'==') .'0 : l.options.item(i).value.toLowerCase().indexOf(k)'
		.($_[0] eq '3' ?'>=' :'==') .'0){'
	: "if (l.options.item(i).text !='' ? l.options.item(i).text.toLowerCase().indexOf(k)"
		.($_[0] eq '3' ?'>=' :'==') .'0  : l.options.item(i).value.toLowerCase().indexOf(k)'
		.($_[0] eq '3' ?'>=' :'==') .'0){')
	.'l.selectedIndex =i; break;};}};'
	.($_[0] && ($_[0] ne '4') 
	 ? 'return(false);' 
	 : $_[0] && ($_[0] eq '2')
	 ? 'return(false);'
	 : '')
	.'}}'};

 ($s->{-cgi}->param("${n}__O_")
	? "<div><script for=\"$n\" event=\"onkeypress\">" .&$fs(0) ."</script>\n"
	: '')
 .$s->{-cgi}->textfield((map {defined($_) && defined($a{$_})
				? ($_ => $a{$_})
				: $a{-textfield} && $a{-textfield}->{$_} && !$s->{-cgi}->param("${n}__O_")
				? ($_ => $a{-textfield}->{$_})
				: ()
		} qw(-name -title -class -style -size -maxlength))
		, -default=>$v
		, -override=>1
		, ($a{-strict} && !$s->{-cgi}->param("${n}__O_")
			? (-readonly=>1) # ,-hidefocus=>0, -disabled=>0
			: ())
	)
 .($s->{-cgi}->param("${n}__O_")
	? ("<input type=\"submit\" name=\"${n}__X_\" value=\"X\" title=\"close\"$ac$as />"
	  ."<input type=\"hidden\" name=\"${n}__P_\" value=\"" .(defined($v) ? $s->{-cgi}->escapeHTML($v) : '') ."\"$ac$as />\n"
	  ."<br />\n"
	  ."<select name=\"${n}__L_\" title=\"select value\" size=\"10\""
	  ."$ac$as"
	  ." ondblclick=\"{${n}__S_.focus(); ${n}__S_.click(); return(true)}\"" 
	  ." onkeypress=\"" .($s->{-cgi}->user_agent('MSIE') ? &$fs(1) : &$fs(2)) 
	  ."\">\n"
	  .join('',map {'<option'
			.((defined($v) ? $v : '') eq (defined($_) ? $_ : '') ? ' selected' : '')
			.' value="' .$s->{-cgi}->escapeHTML(defined($_) ? $_ : '') .'">' 
				.$s->{-cgi}->escapeHTML(
					!defined($_)
					? ''
					: !$a{-labels}
					? (length($_) > $aw ? substr($_,0,$aw) .'...' : $_)
					: defined($a{-labels}->{$_})
					? (length($a{-labels}->{$_}) > $aw ? substr($a{-labels}->{$_},0,$aw) .'...' : $a{-labels}->{$_})
					: '') ."</option>\n"
			} @{&$av()})
	  ."</select>\n"
	  ."<input type=\"submit\" name=\"${n}__S_\" value=\"&lt;\" title=\"set\"$ac$as />"
	  .$s->{-cgi}->button(-value=>'...', -title=>'find', -onClick=>&$fs(3))
	  ."<input type=\"submit\" name=\"${n}__X_\" value=\"X\" title=\"close\"$ac$as />"
	  ."</div>\n"
	  ."<script for=\"window\" event=\"onload\">{window.document.forms[0].${n}__L_.focus()}</script>"
		)
	: ("<input type=\"submit\" name=\"${n}__O_\" value=\"...\" title=\"open\"$ac$as />"
	 .($s->{-cgi}->param("${n}__C_") ||$s->{-cgi}->param("${n}__X_")
		? "<script for=\"window\" event=\"onload\">{window.document.forms[0].${n}__O_.focus()}</script>"
		: ''
		))
	)
}


sub cgiesc {	# escape strings to html
	$_[0]->{-cgi}->escapeHTML(@_[1..$#_])
}


sub cgitfrm {	# table form layot
		# -form =>{form attrs}, -table=>{table attrs}, -tr=>{tr attrs}, -td=>{}, -th=>{}
 my ($s, %a) =$_[0];
 my $i =1;
 while (ref($_[$i]) ne 'ARRAY') {$a{$_[$i]} =$_[$i+1]; $i +=2};
 $s->cgi->start_form(-method=>'POST',-action=>'', $a{-form} ? %{$a{-form}} : ())
	# ,-name=>'test'
 .$s->{-cgi}->table($a{-table} ? $a{-table} : (), "\n"
 .join(''
	, map {	my $r =$_;
		$s->{-cgi}->Tr($a{-tr} ? $a{-tr} : (), "\n"
		.join(''
			, map { ($_ =~/^</
				? $s->{-cgi}->td($a{-td} || {-align=>'left', -valign=>'top'}, $_)
				: $s->{-cgi}->th($a{-th} || $a{-td} || {-align=>'left', -valign=>'top'}, $_)
				) ."\n"
				} @$r)
		) ."\n"
		} @_[$i..$#_])) ."\n"
 .$s->cgi->end_form()
}


sub smtpconnect {# Connect SMTP
 set(@_);	# (-smtphost) -> self->{-smtp}
 set($_[0],-die=>'Carp') if !$_[0]->{-die};
 my $s =shift;
 no warnings;
 local $^W =0; 
 eval('use Net::SMTP; 1') ||return(&{$s->{-die}}($@, $s->efmt('Net::SMTP')));
 $s->{-smtp} =eval {
		local $^W=undef; 
		eval("use Net::SMTP"); 
		$s->{-smtphost}
			? Net::SMTP->new($s->{-smtphost})
			: CORE::die($s->efmt('SMTP host name required'))
	};
 return(&{$s->{-die}}("SMTP host '" .($s->{-smtphost}||'') ."': $@\n")) 
	if !$s->{-smtp} ||$@;
 $s->{-smtp}
}


sub smtp {	# SMTP connection object
 return($_[0]->{-smtp}) if $_[0]->{-smtp};
 smtpconnect(@_)
}


sub smtpsend {	# SMTP mail msg send
		# -from||-sender, -to||-recipient, 
		# -data|| -subject + (-text || -html)
 my ($s, %a) =@_;
 return(&{$s->{-die}}("SMTP host not defined"))
	 if !$s->{-smtphost};
 local $s->{-smtpdomain} =$s->{-smtpdomain} 
			|| ($s->{-smtphost} && $s->smtp(sub{$_[1]->domain()}))
			|| 'nothing.net';
 $a{-from}	=$a{-from} ||$a{-sender} ||$ENV{REMOTE_USER} ||$ENV{USERNAME};
 $a{-from}	=&{$a{-from}}($s,\%a)	if ref($a{-from}) eq 'CODE';
 $a{-to}	=&{$a{-to}}($s,\%a)	if ref($a{-to}) eq 'CODE';
 $a{-to}	=[grep {$_} split /\s*[,;]\s*/, ($a{-to} =~/^\s*(.*)\s*$/ ? $1 : $a{-to})]
					if $a{-to} && !ref($a{-to}) && ($a{-to} =~/[,;]/);
 $a{-sender}	=$a{-sender} ||$a{-from};
 $a{-recipient}	=$a{-recipient} ||$a{-to};
 $a{-recipient}	=&{$a{-recipient}}($s,\%a) if ref($a{-recipient}) eq 'CODE';
 $a{-recipient}	=[grep {$_} split /\s*[,;]\s*/, ($a{-recipient} =~/^\s*(.*)\s*$/ ? $1 : $a{-recipient})]
					if $a{-recipient} && ref($a{-recipient}) && ($a{-recipient} =~/[,;]/);
 return(&{$s->{-die}}("SMTP e-mail recipients not defined"))
	if !$a{-recipient};
 if (!defined($a{-data})) {
	my $koi =(($a{-charset}||$s->charset()||'') =~/1251/);
	$a{-subject} =    ref($a{-subject}) eq 'CODE'
			? &{$a{-subject}}($s,\%a)
			: 'ARSObject'
		if ref($a{-subject}) ||!defined($a{-subject});
	$a{-data}  ='';
	$a{-data} .='From: ' .($koi	? $s->cptran('ansi','koi',$a{-from}) 
					: $a{-from})
			."\cM\cJ";
	$a{-data} .='Subject: '
			.($koi
			? $s->cptran('ansi','koi',$a{-subject})
			: $a{-subject}) ."\cM\cJ";
	$a{-data} .='To: ' 
			.($koi	
			? $s->cptran('ansi','koi', ref($a{-to}) ? join(', ',@{$a{-to}}) : $a{-to}) 
			: (ref($a{-to}) ? join(', ',@{$a{-to}}) : $a{-to}))
			."\cM\cJ" 
			if $a{-to};
	foreach my $k (keys %a) {
		next if $k =~/^-(data|subject|html|text|from|to|sender|recipient)$/;
		next if !defined($a{$k});
		my $n =$k =~/^-(.+)/ ? ucfirst($1) .':' : $k;
		$a{-data} .=$n .' ' .$a{$k} ."\cM\cJ";
	}
	$a{-data} .="MIME-Version: 1.0\cM\cJ";
	$a{-data} .='Content-type: '  .($a{-html} ? 'text/html' : 'text/plain')
			.'; charset=' .($a{-charset}||$s->charset())
			."\cM\cJ";
	$a{-data} .='Content-Transfer-Encoding: ' .($a{-encoding} ||'8bit') ."\cM\cJ";
	$a{-data} .="\cM\cJ";
	$a{-data} .=$a{-html} ||$a{-text} ||'';
 }
 local $^W=undef;
 $s->smtp->mail($a{-sender} =~/<\s*([^<>]+)\s*>/ ? $1 : $a{-sender})
	||return(&{$s->{-die}}("SMTP sender \'" .$a{-sender} ."' -> " .($s->smtp->message()||'?')));
 $s->smtp->to(ref($a{-recipient})
		? (map { !$_ ? () : /<\s*([^<>]+)\s*>/ ? $1 : $_ } @{$a{-recipient}})
		: $a{-recipient}, {'SkipBad'=>1}) # , {'SkipBad'=>1}
	|| return(&{$s->{-die}}("SMTP recipient \'" 
		.(ref($a{-recipient}) ? join(', ', (map { !$_ ? () : /<\s*([^<>]+)\s*>/ ? $1 : $_ } @{$a{-recipient}})) : $a{-recipient}) ."' -> " .($s->smtp->message()||'?')));
 $s->smtp->data($a{-data})
	||return(&{$s->{-die}}("SMTP data '" .$a{-data} ."' -> " .($s->smtp->message()||'?')));
 my $r =$s->smtp->dataend()
	||return(&{$s->{-die}}("SMTP dataend -> " .($s->smtp->message()||'?')));
 $r ||1;
}


sub soon {	# Periodical execution of this script
		# (minutes ||sub{}, ?log file, ?run command, ?soon command)
		# minutes: undef - clear sched || sub{} -> number
		# log file: empty || full file name || var file name
		# run  command: empty || 'command line' || [command line] || sub{}
		# soon command: empty || 'command line' || [command line] || []
		# empty run command - only soon command will be scheduled
		# empty soon command - sleep(minutes*60) will be used
 my ($s, $mm, $lf, $cr, $cs) =@_;
 $lf =$s->vfname($lf) if $lf && ($lf !~/[\\\/]/);
 my $r =1;
 if (ref($cs) ? scalar(@$cs) : $cs) {
	my $qry =$cs;
	if (ref($cs)) {
		return(&{$s->{-die}}("MSWin32 required for `at` in soon()\n"))
			if $^O ne 'MSWin32';
		$cs->[0] =Win32::GetFullPathName($cs->[0])
			if ($^O eq 'MSWin32') && ($cs->[0] !~/[\\\/]/);
		$cs->[0] = $cs->[0]=~/^(.+?)[^\\\/]+$/ ? $1 .'perl.exe' : $cs->[0]
			if $cs->[0] =~/\.dll$/i;
		# for(my $i=1; $i <=$#$cs; $i++) {
		#	next if $cs->[$i] =~/^[-\\\/]/;
		#	$cs->[$i] =Win32::GetFullPathName($cs->[$i])
		#		if ($^O eq 'MSWin32');
		#	last;
		# }
		$qry =join(' ', @$cs);
	}
	print "Starting '$qry'...\n" if defined($mm);
	$s->fstore(">>$lf", "\n" .$s->strtime() ."\t$$\tStarting '$qry'...\n")
		if $lf && defined($mm);
	foreach my $l (`at`) {
		next if $l !~/\Q$qry\E[\w\d\s]*[\r\n]*$/i;
		next if $l !~/(\d+)/;
		my $v =$1;
		my $cmd ="at $v /d";
		print("$cmd # $l\n");
		$s->fstore(">>$lf", $s->strtime() ."\t$$\t$cmd # $l\n")
			if $lf;
		system($cmd);
	}
 }
 while (1) {
	if (!$cr) {
	}
	elsif (ref($cr) eq 'CODE') {
		local *OLDOUT;
		local *OLDERR;
		if ($lf) {
			eval{fileno(STDOUT) && open(OLDOUT, '>&STDOUT')};
			eval{fileno(STDERR) && open(OLDERR, '>&STDERR')};
			open(STDOUT, ">>$lf");
			open(STDERR, ">>$lf");
		}
		$r =&$cr(@_);
		if ($lf) {
			eval{fileno(OLDOUT) && close(STDOUT) && open(STDOUT, '>&OLDOUT')};
			eval{fileno(OLDERR) && close(STDERR) && open(STDERR, '>&OLDERR')};
		}
	}
	else {
		my $cmd =$cr;
		if (ref($cr) eq 'ARRAY') {
			$cr->[0] =Win32::GetFullPathName($cr->[0])
				if ($^O eq 'MSWin32') && ($cr->[0] !~/[\\\/]/);
			$cr->[0] = $cr->[0]=~/^(.+?)[^\\\/]+$/ ? $1 .'perl.exe' : $cr->[0]
				if $cr->[0] =~/\.dll$/i;
			# for(my $i=1; $i <=$#$cr; $i++) {
			#	next if $cr->[$i] =~/^[-\\\/]/;
			#	$cr->[$i] =Win32::GetFullPathName($cr->[$i])
			#		if ($^O eq 'MSWin32');
			#	last;
			# }
			$cmd =join(' ', @$cr);
		}
		if ($lf) {
			$cmd ="$cmd >>$lf 2>>\&1";
			print(($cs ? '' : "\n") ."$cmd\n");
			$s->fstore(">>$lf", ($cs ? '' : "\n") .$s->strtime() ."\t$$\t$cmd\n");
			if (system($cmd) <0) {
				$r =0;
				print("Error $!\n");
				$s->fstore(">>$lf", $s->strtime() ."\t$$\t$!\n");
			}
		}
		else {
			print(($cs ? '' : "\n") ."$cmd\n");
			if (system(ref($cr) ? @$cr : $cr) <0) {
				$r =0;
				print("Error $!\n");
			}
		}


	}
	last if $cs;
	my $mmm =ref($mm) eq 'CODE' ? &$mm($s) : $mm;
	print "sleep($mmm)...\n";
	$s->fstore(">>$lf", $s->strtime() ."\t$$\tsleep($mmm)...\n")
		if $lf;
	sleep($mmm *60);
 }
 if (defined($mm) && (ref($cs) ? scalar(@$cs) : $cs)) {
	my $t1 =$s->strtime($s->timeadd(time(),0,0,0,0, ref($mm) eq 'CODE' ? &$mm($s) : $mm));
	$t1 =$1 if $t1 =~/\s([^\s]+)/;
	my $cmd ="at $t1 /interactive " .(ref($cs) ? join(' ', @$cs) : $cs);
	print("$cmd\n");
	$s->fstore(">>$lf", $s->strtime() ."\t$$\t$cmd\n")
		if $lf;
	if (system($cmd) <0) {
		print("Error $!\n");
		$s->fstore(">>$lf", $s->strtime() ."\t$$\t$!\n")
			if $lf;
	}
 }
 $r
}



sub cfpinit {	# Field Player: init data structures
 my ($s) =@_;	# (self) -> self
 $s->{-fphc} ={};
 $s->{-fphd} ={};
 my $dh ={};
 my $dp =undef;
 my $ah ={};
 my $ak;
 my $bf =undef;
 foreach my $f (@{$s->{-fpl}}) {
	if (ref($f) && $f->{-key} && $f->{-namecgi}) {
		$ak =$f->{-namecgi};
		last
	}
 }
 foreach my $f (@{$s->{-fpl}}) {
	if (ref($f) ne 'HASH') {
		if (!defined($dp)) {
			$dp =$f ||'-unknown';
		}
		elsif (!defined($f)) {
			delete $dh->{$dp};
			delete $dh->{-record} if $dp eq '-formdb';
			$dp =undef;
		}
		else {
			$dh->{$dp} =$f;
			delete $dh->{-record} if $dp eq '-formdb';
			$dp =undef;
		}
	}
	else {
		@$f{keys %$dh} =values %$dh;
		if (!$f->{-namecgi}) {
			$f->{-namecgi} =$f->{-name};
			$f->{-namecgi} =~s/[\s-]/_/g
				if $f->{-namecgi};
		}
		if (!$f->{-namedb}) {
			$f->{-namedb} =$f->{-name};
		}
		$s->{-fphc}->{$f->{-namecgi}} =$f if $f->{-namecgi};
		$s->{-fphd}->{$f->{-namedb}}  =$f if $f->{-namedb};
		$f->{-namecmt} =$f->{-namelbl} ||$f->{-namecgi} ||$f->{-namedb} ||$f->{-name} if !$f->{-namecmt};

		$f->{-values} =schvals($s, $f->{-formdb}, $f->{-namedb})
			if $f->{-namedb} && $f->{-formdb}
			&& !($f->{-values} ||$f->{-labels})
			&& schvals($s, $f->{-formdb}, $f->{-namedb});

		$f->{-labels} =schlbls($s, $f->{-formdb}, $f->{-namedb})
			if $f->{-namedb} && $f->{-formdb}
			&& !$f->{-labels}
			&& schlbls($s, $f->{-formdb}, $f->{-namedb});

		if ((ref($f->{-labels}) eq 'HASH')) {
			foreach my $k (keys %{$f->{-labels}}) {
				last if !ref($f->{-labels}->{$k});
				$f->{-changelb} ={} if !$f->{-changelb};
				my $n =defined($f->{-labels}->{$k}->{-label})
					? $f->{-labels}->{$k}->{-label}
					: defined($f->{-labels}->{$k}->{-name})
					? $f->{-labels}->{$k}->{-name}
					: '';
				$f->{-changelb}->{$k} =$f->{-labels}->{$k};
				$f->{-labels}->{$k} =$n;
			}
		}
		if ((ref($f->{-values}) eq 'ARRAY')) {
			for (my $i =0; $i <=$#{$f->{-values}}; $i++) {
				last if !ref($f->{-values}->[$i]);
				$f->{-changelb} ={} if !$f->{-changelb};
				my $n =defined($f->{-values}->[$i]->{-name})
					? $f->{-values}->[$i]->{-name}
					: defined($f->{-values}->[$i]->{-label})
					? $f->{-values}->[$i]->{-label}
					: '';
				$f->{-changelb}->{$n} =$f->{-values}->[$i];
				$f->{-values}->[$i] =$n;
			}
		}

		if ($f->{-change} ||$f->{-changelb}) {
			$f->{-onchange} =1
		}

		if (exists($f->{-computed}) && !($f->{-readonly} ||$f->{-disabled})) {
			$f->{-disabled} =1
		}

		if (!$f->{-namecgi} || !$f->{-action}) {
		}
		elsif (!$ah->{$f->{-namecgi}}) {
			$ah->{$f->{-namecgi}} =$f
		}
		elsif (ref($f->{-action}) ||($f->{-action} =~/^../)) {
		}
		else {
			$f->{-used} =$ah->{$f->{-namecgi}}->{-used}
				if !exists($f->{-used})
				&& exists($ah->{$f->{-namecgi}}->{-used});
			$f->{-unused} =$ah->{$f->{-namecgi}}->{-unused}
				if !exists($f->{-unused})
				&& exists($ah->{$f->{-namecgi}}->{-unused});
			$ah->{$f->{-namecgi}}->{-widget} =undef
				if !exists($ah->{$f->{-namecgi}}->{-widget});
		}
		if (exists($f->{-used}) ||exists($f->{-unused})) {
		}
		elsif ($ak && ($f->{-action}||$f->{-preact})
		&& (($f->{-action}||$f->{-preact}) =~/^(?:entryUpd|entryDel|entry|vfentry|vfhash)$/)) {
			$f->{-used} =sub{$_[0]->cgipar($ak)}
		}
		else {
			$f->{-used} =1
		}
		$f->{-widget} =undef
			if $f->{-preact} && !exists($f->{-widget});
		$bf =1
			if $f->{-action} && ($f->{-action} =~/^\d$/);
	}
 }
 if (!$bf) {
	my @bl;
	foreach my $f (@{$s->{-fpl}}) {
		next	if ref($f) ne 'HASH';
		next	if !$f->{-namecgi} || !$f->{-action};
		$f->{-widget} =undef;
		next	if exists($f->{-computed}) || exists($f->{-value})
			|| !$ah->{$f->{-namecgi}};
		push @bl, {%$f, -action=>1};
		delete $bl[$#bl]->{-widget};
		delete $ah->{$f->{-namecgi}};
	}
	push @{$s->{-fpl}}, @bl;
 }
 $s
}


sub cfpused {	# Field Player: field should be used?
		# (self, field) -> yes?
 my ($s, $f) =@_;
 return(map {ref($_) && cfpused($s, $_) ? $_ : ()} @{$s->{-fpl}})
	if !$f;
 $f =$s->{-fphc}->{$f} ||$s->{-fphd}->{$f}
	if !ref($f);
 !ref($f) || (ref($f) ne 'HASH')
 ? 0
 : (	!exists($f->{-used})
	? 1
	: !$f->{-used}
 	? 0
	: (ref($f->{-used}) eq 'CODE')
	? &{$f->{-used}}($s, $f)
	: (ref($f->{-used}) eq 'ARRAY')
	? !scalar(grep {my $v =cfpused($s, $_) && cfpvv($s, $_);
			!defined($v) || ($v eq '')} @{$f->{-used}})
	: !ref($f->{-used}) && ($f->{-used} !~/^\d/)
	? (do{	my $v =cfpused($s, $f->{-used}) && cfpvv($s, $f->{-used});
		defined($v) && ($v ne '')})
	: ($f->{-used} && 1)
	)
 && (	!exists($f->{-unused})
	? 1
	: !$f->{-unused}
 	? 1
	: (ref($f->{-unused}) eq 'CODE')
	? !&{$f->{-unused}}($s, $f)
	: (ref($f->{-unused}) eq 'ARRAY')
	? scalar(grep {my $v =cfpused($s, $_) && cfpvv($s, $_);
			!defined($v) || ($v eq '')} @{$f->{-unused}})
	: !ref($f->{-unused}) && ($f->{-unused} !~/^\d/)
	? !(do{	my $v =cfpused($s, $f->{-unused}) && cfpvv($s, $f->{-unused});
		defined($v) && ($v ne '')})
	: ($f->{-unused} && 1)
	)
}


sub cfpn {	# Field Player: field name
		# (self, field || fieldname) -> cgi field name
 ref($_[1])
 ? $_[1]->{-namecgi}
 : (($_[0]->{-fphc}->{$_[1]} ||$_[0]->{-fphd}->{$_[1]} ||{})->{-namecgi} ||$_[1])
}


sub cfpnd {	# Field Player: field name
		# (self, field || fieldname) -> db field name
 ref($_[1])
 ? $_[1]->{-namedb}
 : (($_[0]->{-fphc}->{$_[1]} ||$_[0]->{-fphd}->{$_[1]} ||{})->{-namedb} ||$_[1])
}


sub cfpv {	# Field Player: field value
		# (self, field || fieldname) -> value
 my $f =ref($_[1])
	? $_[1]
	: ($_[0]->{-fphc}->{$_[1]} ||$_[0]->{-fphd}->{$_[1]});
 !$f
 ? $_[0]->{-cgi}->param($_[1])
 : !$f->{-namecgi} || !defined($_[0]->{-cgi}->param($f->{-namecgi}))
 ? (exists($f->{-computed})
	? (ref($f->{-computed}) eq 'CODE'
		? &{$f->{-computed}}($_[0], $f)
		: ref($f->{-computed}) eq 'ARRAY'
		? cfpv($_[0], @{$f->{-computed}})
		: $f->{-computed})
	: undef)
 : $_[0]->{-cgi}->param($f->{-namecgi})
}


sub cfpvl {	# Field Player: field values list
		# (self, field || fieldname) -> [list]
 my $f =ref($_[1])
	? $_[1]
	: ($_[0]->{-fphc}->{$_[1]} ||$_[0]->{-fphd}->{$_[1]});
 !$f
 ? []
 : !$f->{-values}
 ? (!$f->{-labels}
	? []
	: (do{  local $_ =cfpv(@_);
		my $ll =ref($f->{-labels}) eq 'CODE'
			? &{$f->{-labels}}($_[0], $f, $_)
			: $f->{-labels};
		use locale;
		[sort {lc($ll->{$a}) cmp lc($ll->{$b})
			} keys %$ll]}))
 : ref($f->{-values}) eq 'CODE'
 ? (do{	local $_ =cfpv(@_);
	&{$f->{-values}}($_[0], $f, $_)})
 : $f->{-values}
}


sub cfpvv {	# Field Player: field value or default
		# (self, field || fieldname) -> value
 my $v =cfpv(@_);
 defined($v) ? $v : cfpvd(@_)
}


sub cfpvd {	# Field Player: field default value
		# (self, field || fieldname) -> value
 my $f =ref($_[1])
	? $_[1]
	: ($_[0]->{-fphc}->{$_[1]} ||$_[0]->{-fphd}->{$_[1]});
 !$f
 ? undef
 : exists($f->{-computed})
 ? (	  ref($f->{-computed}) eq 'CODE'
	? &{$f->{-computed}}($_[0], $f)
	: ref($f->{-computed}) eq 'ARRAY'
	? cfpvv($_[0], @{$f->{-computed}})
	: $f->{-computed})
 : !exists($f->{-value})
 ? ($f->{-values} ||$f->{-labels} ? cfpvl($_[0], $f)->[0] : undef)
 : ref($f->{-value}) eq 'CODE'
 ? &{$f->{-value}}($_[0], $f)
 : ref($f->{-value}) eq 'ARRAY'
 ? cfpvv($_[0], @{$f->{-value}})
 : $f->{-value}
}


sub cfpvp {	# Field Player: field previous value
		# (self, field || fieldname) -> value
 $_[0]->{-cgi}->param((ref($_[1])
		? $_[1]->{-namecgi} ||''
		: (($_[0]->{-fphc}->{$_[1]} ||$_[0]->{-fphd}->{$_[1]} ||{})->{-namecgi} ||$_[1])
	) .'__PV_')
}


sub cfpvc {	# Field Player: field value changed since form open?
		# (self, field || fieldname) -> changed?
 my ($v1, $v0) =(cfpv(@_), cfpvp(@_));
   defined($v1) && defined($v0)
 ? $v1 ne $v0
 : !defined($v1) && !defined($v0)
 ? 0
 : 1
}


sub cfpvcc {	# Field Player: field value changed in the last form submit?
		# (self, field || fieldname) -> changed?
 my $f =ref($_[1])
	? $_[1]
	: ($_[0]->{-fphc}->{$_[1]} ||$_[0]->{-fphd}->{$_[1]});
 my $fn =ref($f) ? $f->{-namecgi} ||'' : '';
 $f->{-onchange} ||$f->{-values}
 ? $_[0]->{-cgi}->param("${fn}__C_") ||!defined($_[0]->{-cgi}->param("${fn}__C_"))
 : cfpvc(@_)
}


sub cfpaction {	# Field Player: execute action
		# (self, {action}||'action'
		# , '-preact'||'-action', {key field}) -> success
 my ($s, $act, $ord, $rp, $f) =@_;
 my $r =1;
 my $af=ref($act) eq 'HASH' ? $act : {};
 my $ae=ref($act) eq 'HASH' ? $act->{$ord} : $act;
 my $frm =$f->{-formdb}|| $af->{-formdb} ||'';
 my $frn =$f->{-record}|| $af->{-record} ||'';
 my $frk =undef;
 my $ffc =sub{	my $f =$_[1];
		!ref($f)
		|| !$f->{-namedb} || $f->{-key}
		|| !$f->{-formdb} || ($f->{-formdb} ne $frm)
		|| (($f->{-record}||'') ne $frn)
		};
 my $vy  =0;
 my $fvu =sub{	return(undef)
			if (ref($_[1]->{-values}) eq 'ARRAY') 
			&& !scalar(@{$_[1]->{-values}});
		my $v =cfpvv(@_);
		$v =undef  if defined($_[1]->{-undef}) && defined($v) && ($_[1]->{-undef} eq $v);
		$vy=1 if defined($v) && ($v ne '') && (!$_[1]->{-master} ||$_[1]->{-key});
		$v =cfpvv($_[0], $_[1]->{-master}) if $_[1]->{-master} && !$_[1]->{-key};
		return($v) if !$_[2] || (defined($_[1]->{-vftran}) && !$_[1]->{-vftran});
		!defined($v)
		? $v
		: (ref($_[1]->{-labels}) eq 'HASH') && exists($_[1]->{-labels}->{$v})
		? $_[1]->{-labels}->{$v}
		: $v;
		};
 local $_;
 if ($frn || $s->{-fpbn}) {
	my $n =$frn =~/^(.+?)\d+$/ ? $1 : $frn;
	if ($n ne ($s->{-fpbn}||'')) {
		$s->{-fpbn} =$n;	# buffer values
		$s->{-fpbv} =undef;     # buffer name == record common name
		if ($ae =~/^(?:vfentry|entry)$/) {
			foreach my $ff (@{$s->{-fpl}}) {
				next if &$ffc($s, $ff) || !$ff->{-master};
				$frk =$ff;
				last;
			}
		}
	}
 }
 if (!$ae) {
 }
 elsif (ref($ae) eq 'CODE' && ($ord eq '-action')) {
	$r =eval{&$ae($s, $act, $ord, $rp, $f, $_ =cfpvv($s,$f), cfpvp($s,$f)
		, {map {&$ffc($s, $_)
			? ()
			: ($_->{-namedb} => &$fvu($s, $_))
			} cfpused($s)}
		)}
 }
 elsif (ref($ae) eq 'CODE') {	# -preact
	$r =eval{&$ae($s, $act, $ord, $rp, $f, $_ =cfpvv($s, $f), cfpvp($s,$f)
		, {map {&$ffc($s, $_) || !defined(cfpv($s, $_))
			? ()
			: ($_->{-namedb} => cfpv($s, $_))
			} @{$s->{-fpl}}}
		)}
 }
 elsif ($ae =~/^(?:vfentry|entry)$/ && ref($s->{-fpbv})) {
	$r =shift @{$s->{-fpbv}} if scalar(@{$s->{-fpbv}});
	$r ={} if !$r;
 }
 elsif ($ae eq 'vfentry') {	# -preact
	my $fs =$f->{-vfname} ||$af->{-vfname};
	my $fn =undef;
	my $fv =undef;
	if ($frk && $fs && ($fn =$frk->{-namedb}) && defined($fv=cfpv($s, $frk->{-master}))) {
		$s->{-fpbv} =$f->{-namedb}
			? $s->vfdata($fs, sub{defined($_->{$fn}) && ($_->{$fn} eq $fv)})
			: [];
		$r =shift @{$s->{-fpbv}} if $s->{-fpbv} && scalar(@{$s->{-fpbv}});
		$r ={} if !$r;
	}
	elsif ($fs) {
		$r =undef;
		if (defined($fv=cfpv($s, $f))) {
			$fn =$f->{-namedb}
		}
		elsif ($af->{-namedb} && ($fv =cfpv($s, $af))) {
			$fn =$af->{-namedb};
		}
		elsif ($fn =cfpnd($s, cfpv($s, $af))) {
			$fv =cfpv($s, $fn)
		}
		if ($fn && defined($fv)) {
			$r =undef;
			my $fa =$s->vfdata($fs);
			foreach my $e (@$fa) {
				next if !defined($e->{$fn}) || ($e->{$fn} ne $fv);
				$r =$e;
				last
			}
			$@="Not found '$fn'=\"$fv\""
				if !$r;
		}
		else {
			$@ =$fn ? "Key '$fn' not defined" : "Key not defined"
		}
	}
	else {
		$@ ="Nothing to do, '-vfname' not defined"
	}
 }
 elsif ($ae eq 'vfhash') {	# -preact
	my $fs =$f->{-vfname} ||$af->{-vfname};
	my $fn =undef;
	my $fv =undef;
	if (defined($fv=cfpv($s, $f))) {
		$fn =$f->{-namedb}
	}
	elsif ($af->{-namedb} && ($fv =cfpv($s, $af))) {
		$fn =$af->{-namedb};
	}
	elsif ($fn =cfpnd($s, cfpv($s, $af))) {
		$fv =cfpv($s, $fn)
	}
	$r =undef;
	if (!$fs) {
		$@ ="Nothing to do, '-vfname' not defined"
	}
	elsif (!$fn) {
		$@ ="Key not defined"
	}
	elsif (!defined($fv)) {
		$@ ="Key '$fn' not defined"		
	}
	else {
		$r =$s->vfhash($fs, $fn, $fv);
		$@="Not found '$fn'=\"$fv\""
			if !$r;
	}
 }
 elsif ($ae eq 'entry') {	# -preact
	my $fn =undef;
	my $fv =undef;
	if (!$frm) {
		$r =undef;
		$@ ="Form not defined"
	}
	elsif ($frk && ($fn=$frk->{-namedb}) && ($fv =cfpv($s, $frk->{master}))) {
		$s->{-fpbv} =$f->{-namedb}
			? eval{$s->connect()
				&& $s->query(-form=>$frm
				,-fields=>'*'
				,-where=>"'$fn'=" .$s->arsquot($fv))}
			: [];
		if ($s->{-fpbv}) {
			$r =shift @{$s->{-fpbv}} if scalar(@{$s->{-fpbv}});
			$r ={} if !$r;
		}
		else {
			$r =undef
		}
	}
	elsif ($f && ($fv =cfpv($s, $f))) {
		$r =eval{$s->connect()
				&& $s->entry(-form=>$frm
				,-id=>$fv)};
	}
	elsif (   (($fn =$af->{-namedb}) && defined($fv =cfpv($s, $af)))
	       || (($fn =cfpnd($s, cfpv($s, $af))) && defined($fv =cfpv($s, $fn)))
		) {
		$r =eval{$s->connect()
				&& $s->query(-form=>$frm
				,-fields=>'*'
				,-where=>"'$fn'=" .$s->arsquot($fv))};
		if ($r) {
			$r =shift @$r;
			$@ ="Not found '$fn'=\"$fv\""
				if !$r
		}
	}
	else {
		$r =undef;
		$@ ="Key not defined"
	}
 }
 elsif ($ae eq 'entryNew') {	# -preact
	$r =eval{$s->connect()
		&& $s->entryNew(-form => $frm)}
		if $frm;
 }
 elsif ($ae eq 'entryIns') {	# -action
	my $fs =$f->{-vfname} ||$af->{-vfname};
	$r =eval{$s->connect()
		&& $s->entryIns(-form=>$frm
			, map {	&$ffc($s, $_)
			? ()
			: ($_->{-namedb} => &$fvu($s, $_))
			} cfpused($s))}
		if $frm;
	$r =1 if ref($r);
	if (!$r) {
		$@ ="Unknown 'entryIns' error" if !$@
	}
	elsif (!$fs ||!$f->{-key}) {
	}
	elsif (($af->{-vfclear} || $f->{-vfclear}) && $s->{"${fs}-calc"}) {
		$s->vfclear($fs);
	}
	elsif (($af->{-vfrenew} || $f->{-vfrenew}) && $s->{"${fs}-store"}) {
		eval{$s->vfclear($fs); $s->vfrenew($fs)}
	}
	elsif ($af->{-vfedit} || $f->{-vfedit}) {
		my $fn =$f->{-namedb} ||$af->{-namedb};
		my $ft =defined($f->{-vftran}) ? $f->{-vftran} : $af->{-vftran};
		my $fv =cfpv($s, $f);
		my $fa =$s->vfdata($fs);
		push @$fa, {$f->{-namedb} ? ($f->{-namedb}=>$r) : ()
				,map { &$ffc($s, $_)
					? ()
					: ($_->{-namedb} => &$fvu($s, $_, $ft))
					} cfpused($s)};
		$s->vfstore($fs);
		$s->vfclear($fs);
	}
 }
 elsif ($ae eq 'entryUpd') {	# -action
	my $fs =$f->{-vfname} ||$af->{-vfname};
	$r =eval{$s->connect()
		&& $s->entryUpd(-form=>$frm, -id=>cfpvv($s,$f)
		, map { &$ffc($s, $_)
			? ()
			: ($_->{-namedb} => &$fvu($s, $_))
			} cfpused($s))}
		if $frm && cfpvv($s,$f);
	if (!$r) {
		$@ ="Unknown 'entryUpd' error" if !$@
	}
	elsif (!$f->{-key} ||!$fs) {
	}
	elsif (($af->{-vfclear} || $f->{-vfclear}) && $s->{"${fs}-calc"}) {
		$s->vfclear($fs);
	}
	elsif (($af->{-vfrenew} || $f->{-vfrenew}) && $s->{"${fs}-store"}) {
		eval{$s->vfclear($fs); $s->vfrenew($fs)}
	}
	elsif ($af->{-vfedit} || $f->{-vfedit}) {
		my $fn =$f->{-namedb} ||$af->{-namedb};
		my $ft =defined($f->{-vftran}) ? $f->{-vftran} : $af->{-vftran};
		my $fv =cfpv($s, $f);
		my $fa =$s->vfdata($fs);
		foreach my $e (@$fa) {
			next if !defined($e->{$fn}) || ($e->{$fn} ne $fv);
			foreach my $f1 (cfpused($s)) {
				next	if &$ffc($s, $f1);
				$e->{$f1->{-namedb}} =&$fvu($s, $f1, $ft);
			}
			last;
		}
		$s->vfstore($fs);
		$s->vfclear($fs);
	}
 }
 elsif ($act eq 'entryDel') {	# -action
	my $fs =$f->{-vfname} ||$af->{-vfname};
	$r =eval{$s->connect()
		&& $s->entryDel(-form=>$frm
		, -id=>cfpvv($s,$f))}
		if $frm && cfpvv($s,$f);
	if (!$r) {
		$@ ="Unknown 'entryDel' error" if !$@
	}
	elsif (!$fs ||!$f->{-key}) {
	}
	elsif (($af->{-vfclear} || $f->{-vfclear}) && $s->{"${fs}-calc"}) {
		$s->vfclear($fs);
	}
	elsif (($af->{-vfrenew} || $f->{-vfrenew}) && $s->{"${fs}-store"}) {
		eval{$s->vfclear($fs); $s->vfrenew($fs)}
	}
	elsif ($af->{-vfedit} || $f->{-vfedit}) {
		my $fn =$f->{-namedb} ||$af->{-namedb};
		my $fv =cfpv($s, $f);
		my $fa =$s->vfdata($fs);
		my ($i,$j)  =(0, undef);
		foreach my $e (@$fa) {
			if (defined($e->{$fn}) && ($e->{$fn} eq $fv)) {
				$j =$i;				
				last;
			}
			$i++
		}
		splice(@$fa, $i, 1);
		$s->vfstore($fs);
		$s->vfclear($fs);
	}
 }
 elsif ($ae eq 'entrySave') {	# -action
	my $a =cfpvv($s,$f) ? 'entryUpd' : cfpvp($s,$f) ? 'entryDel' : 'entryIns';
	if ($a eq 'entryIns') { # $vy= 1 if cfpvv($s,$f)
		map { &$ffc($s, $_)
			? ()
			: ($_->{-namedb} => &$fvu($s, $_))
			} cfpused($s);
		$a = $vy
			? $a
			: ($a eq 'entryIns')
			? ''
			: ($a eq 'entryUpd') && cfpvp($s,$f)
			? 'entryDel'
			: $a;
	}
	$s->{-cgi}->param($f->{-namecgi}, cfpvp($s,$f))
		if ($a eq 'entryDel') && $f->{-namecgi};
	$r =!$a
	? 1
	: ref($act) eq 'HASH'
	? cfpaction($s, {%$act, -action => $a}, @_[2..$#_])
	: cfpaction($s, $a, @_[2..$#_])
 }
 if ((ref($r) eq 'HASH') && ($ord eq '-preact')) {
	foreach my $f1 (map {	&$ffc($s, $_) || !$_->{-namecgi}
				? ()
				: ($_)
				} @{$s->{-fpl}}) {
		next if !exists($r->{$f1->{-namedb}});
		my $u =$s->cfpused($f1);
		my $v =$r->{$f1->{-namedb}};
		if (defined($v)
		||  defined($s->{-cgi}->param($f1->{-namecgi}))) {
			$s->{-cgi}->param($f1->{-namecgi}, $v);
			$s->{-cgi}->param($f1->{-namecgi} .'__C_', '')
				if $u && ($f1->{-values} || $f1->{-labels});
		}
		if (defined($v)
		&& (defined($f1->{-vftran}) ? $f1->{-vftran} : defined($f->{-vftran}) ? $f->{-vftran} : $af->{-vftran})
		&& (ref($f1->{-labels}) eq 'HASH') && !exists($f1->{-labels}->{$v})) {
			foreach my $k (keys %{$f1->{-labels}}) {
				next if $v ne $f1->{-labels}->{$k};
				$v =$k;
				$s->{-cgi}->param($f1->{-namecgi}, $v);
				last;
			}
			print &{$s->{-fpmsg}}($s, 'Warning'
				, ($af->{-namelbl} ||$af->{-namecgi}) 
				.': '
				."'" .($f1->{-namelbl}||$f1->{-namedb}) 
				."' == ?\"$v\"?")
				if $u 
				&& !exists($f1->{-labels}->{$v})
				&& (defined($f1->{-lbtran}) && !$f1->{-lbtran})
		}
	}
 }
 $r
}


sub cfprun {	# Field Player: run
		# (self, msg sub{}
		# , form row sub{}, form top, form bottom) -> success
 my ($s, $cmsg, $cfld, $cfld0, $cfld1) =@_;
 $cmsg =sub{"\n<br /><font style=\"font-weight: bolder\""
		.($_[1] =~/^(?:Error|Warning)/ ? ' color="red"' : '')
		.'>'
		.(defined($_[1]) ? $_[0]->{-cgi}->escapeHTML($_[1]) : 'undef')
		.": "
		.(defined($_[2]) ? $_[0]->{-cgi}->escapeHTML($_[2]) : 'undef')
		."</font>"
		# 'Error', 'Warning',
		# 'Executing', 'Done'('Success', 'Error')
		}
	if !$cmsg;
 my $emsg =sub{	
		$CGI::Carp::CUSTOM_MSG
		? &$CGI::Carp::CUSTOM_MSG($_[1])
		: print(&$cmsg($_[0], 'Error', $_[1]))
		};
 $cfld =sub{"\n<tr><th align=\"left\" valign=\"top\">"
		. ($_[1]->{-namehtml}
			? &{$_[1]->{-namehtml}}(@_)
			: $_[0]->{-cgi}->escapeHTML($_[1]->{-namelbl}||''))
		. "</th>\n<td align=\"left\" valign=\"top\">"
		. $_[2]
		. "</td></tr>"
		}
	if !$cfld;
 $cfld0="\n<table>"	if !$cfld0;
 $cfld1="\n</table>"	if !$cfld1;
 $s->cgi();
 cfpinit($s);
 local $s->{-fpmsg} =$cmsg;
 my $err;
 my $act;
 my $aec;
 my $arv;
 my $key;
 foreach my $f (@{$s->{-fpl}}) {
	next	if (ref($f) ne 'HASH')
		|| (exists($f->{-used}) && !$f->{-used});
	if ($f->{-preact} && ($f->{-preact} !~/^\d$/) && cfpvv($s, $f)) {
		$act =[] if !$act ||$key;
		push @$act, $f
	}
	if ($f->{-action} && ($f->{-action} !~/^\d$/) && cfpvv($s, $f)) {
		$aec =cfpvv($s, $f);
	}
	if ($f->{-preact} || $f->{-action}) {
		$key =undef
	}
	elsif ($f->{-key}) {
		$key =1;
	}
	if ($f->{-key} && $act && !$err) {
		$arv =1;
		foreach my $a (@$act) {
			$arv =cfpaction($s, $a, '-preact', $arv, $f);
			next if $arv;
			$err =$@;
			last
		}
		if (!$arv) {
			&$emsg($s, $err ||"Unknown 'cfpaction' error");
			$err =1;
			last;
		}
	}
	next if !cfpused($s, $f);
	my $fn =cfpn($s, $f);
	if (!$f->{-reset}
		? undef
		: ref($f->{-reset}) eq 'CODE'
		? &{$f->{-reset}}($s, $f)
		: ref($f->{-reset}) eq 'ARRAY'
		? grep {cfpvcc($s, $_)} @{$f->{-reset}}
			# ??? read from URL interpreted as changed listbox
		: $f->{-reset}
		? cfpvcc($s, $f->{-reset})
		: undef
		) {
		$s->{-cgi}->delete($fn);
	}
	my $fv =exists($f->{-computed})
		? (ref($f->{-computed}) eq 'CODE'
			? &{$f->{-computed}}($s, $f)
			: ref($f->{-computed}) eq 'ARRAY'
			? cfpvv($s, @{$f->{-computed}})
			: $f->{-computed})
		: cfpvv($s, $f);
	local $_ =$fv;
	if (!($f->{-action} || $f->{-preact}) && $f->{-namecgi}) {
		if (defined($fv)) {
			if ((defined($f->{-lbtran}) ? $f->{-lbtran} : 0)
			&& (ref($f->{-labels}) eq 'HASH') && !exists($f->{-labels}->{$fv})) {
				foreach my $k (keys %{$f->{-labels}}) {
					next if $fv ne $f->{-labels}->{$k};
					$fv =$k;
					last;
				}
				print &$cmsg($s, 'Warning'
					, "'" .($f->{-namelbl} ||$f->{-namecgi} ||$f->{-namedb})
					."' == ?\"$fv\"?")
					if !exists($f->{-labels}->{$fv})
					&& !$f->{-lbadd}
			}			
			if ((defined($f->{-lbadd}) ? $f->{-lbadd} : 0)) {
				$f->{-values} =do{use locale;
					[sort {lc($f->{-labels}->{$a}) cmp lc($f->{-labels}->{$b})} keys %{$f->{-labels}}]}
					if (ref($f->{-labels}) eq 'HASH')
					&& !$f->{-values};
				push @{$f->{-values}}, $fv
					if (ref($f->{-values}) eq 'ARRAY')
					&& !grep /^\Q$fv\E$/, @{$f->{-values}};
			}
		}
		$f->{-labels} =&{$f->{-labels}}($s, $f, $_ =$fv)
			if ref($f->{-labels}) eq 'CODE';
		$f->{-values} =&{$f->{-values}}($s, $f, $_ =$fv)
			if ref($f->{-values}) eq 'CODE';
		$f->{-values} =do{use locale;
				[sort {lc($f->{-labels}->{$a}) cmp lc($f->{-labels}->{$b})} keys %{$f->{-labels}}]}
			if $f->{-labels}
			&& !$f->{-values};
		if ($f->{-values}
		&& (!defined($fv) || !grep /^\Q$fv\E$/, @{$f->{-values}})) {
			$fv =$f->{-values}->[0];
			$fv ='' if !defined($fv);
			$s->{-cgi}->delete("${fn}__C_")	if $f->{-change};
		}
		if (defined($fv)) {
			$s->{-cgi}->param($fn, $fv);
			$s->{-cgi}->param("${fn}__PV_", $fv)
				if !defined($s->{-cgi}->param("${fn}__PV_"));
		}
		else {
			$s->{-cgi}->delete($fn);
		}
	}
	foreach my $q ('-change', '-changelb') {
		next if !$f->{$q};
		last if !cfpvcc($s, $f);
		my $c =ref($f->{$q}) eq 'CODE' ? &{$f->{$q}}($s, $f, $_ =$fv) : $f->{$q};
		$c =ref($c) ne 'HASH' ? undef : ref($c->{$fv}) eq 'HASH' ? $c->{$fv} : $c;
		if (ref($c) eq 'HASH') {
			foreach my $k (keys %$c) {
				next if $k =~/^-/;
				defined($c->{$k})
				? $s->{-cgi}->param(cfpn($s, $k)
					, ref($c->{$k}) eq 'CODE'
					? &{$c->{$k}}($s, $f, $_ =$fv)
					: $c->{$k}
					)
				: $s->{-cgi}->delete(cfpn($s, $k))
			}
		}
	}
	if (my $ev =!$aec || !$f->{-error}
		? undef
		: ref($f->{-error}) eq 'CODE'
		? &{$f->{-error}}($s, $f, $_ =$fv, cfpvp($s, $f), $aec)
		: !ref($f->{-error}) && (!defined($fv) || ($fv eq ''))
		? $f->{-error}
		: undef
		) {
		print &$cmsg($s, 'Error', "'" .$f->{-namelbl} ."' - $ev");
		$err =1;
	}
	if (my $ev =!$f->{-warn}
		? undef
		: ref($f->{-warn}) eq 'CODE'
		? &{$f->{-warn}}($s, $f, $_ =$fv, cfpvp($s, $f), $aec)
		: !ref($f->{-warn}) && (!defined($fv) || ($fv eq ''))
		? $f->{-warn}
		: undef
		) {
		print &$cmsg($s, 'Warning', "'" .$f->{-namelbl} ."' - $ev");
	}
 }
 return(undef)
	if $err;
 $act =	$arv =$key =undef;
 foreach my $f (@{$s->{-fpl}}) {
	next	if (ref($f) ne 'HASH')
		|| (exists($f->{-used}) && !$f->{-used});
	next if !cfpused($s, $f);
	if ($f->{-action} && ($f->{-action} !~/^\d$/) && cfpvv($s, $f)) {
		$act =[] if !$act ||$key;
		push @$act, $f
	}
	if ($f->{-preact} || $f->{-action}) {
		$key =undef
	}
	elsif ($f->{-key}) {
		$key =1;
	}
	if ($f->{-key} && $act && !$err) {
		$arv =1;
		foreach my $a (@$act) {
			print &$cmsg($s, 'Executing', ($a->{-namelbl} ||$a->{-namecgi} ||'') .' ', $arv)
				if $a->{-namelbl} ||$a->{-namecgi};
			$arv =cfpaction($s, $a, '-action', $arv, $f);
			next if $arv;
			$err =$@;
			last;
		}
		if (!$arv) {
			&$emsg($s, $err ||"Unknown 'cfpaction' error");
			$err =1;
			last;
		}
	}
 }
 if ($act) {
	print &$cmsg($s, 'Done', $err ? ('Error', $@) : ('Success', $arv))
 }
 return(undef)
	if $err;
 return(1)
	if $act;
 foreach my $f (@{$s->{-fpl}}) {
	next	if (ref($f) ne 'HASH')
		|| (exists($f->{-used}) && !$f->{-used});
	next if exists($f->{-widget}) && !defined($f->{-widget});
	next if !$f->{-namecgi};
	my $u =cfpused($s, $f);
	next if $u && !($f->{-hidden} ||((ref($f->{-values}) eq 'ARRAY') && !scalar(@{$f->{-values}})));
	print 	defined(cfpvp($s, $f)) 
		? '<input type="hidden" name="' .$f->{-namecgi} .'__PV_" value="' 
			.$s->{-cgi}->escapeHTML(cfpvp($s, $f))
			.'" />' ."\n"
		: ''
		, !$u
		? (	defined($s->{-cgi}->param($f->{-namecgi}))
			? '<input type="hidden" name="' .$f->{-namecgi} .'" value="'
				.$s->{-cgi}->escapeHTML($s->{-cgi}->param($f->{-namecgi}))
				.'" />' ."\n"
			: '')
		: defined(cfpvv($s, $f))
		? '<input type="hidden" name="' .$f->{-namecgi} .'" value="'
			.$s->{-cgi}->escapeHTML(cfpvv($s, $f))
			.'" />' ."\n"
		: '';
 }
 print ref($cfld0) ? &{$cfld0}($s) : $cfld0;
 my $bb ='';
 foreach my $f (@{$s->{-fpl}}) {
	next	if (ref($f) ne 'HASH')
		|| (exists($f->{-used}) && !$f->{-used});
	next if !cfpused($s, $f);
	next if exists($f->{-widget}) && !defined($f->{-widget});
	next if $f->{-hidden} ||((ref($f->{-values}) eq 'ARRAY') && !scalar(@{$f->{-values}}));
	my $bf =$f->{-action} ||$f->{-preact};
	if ($f->{-action} ||$f->{-preact}) {
		$bb .=' ' if $bb;
		$bb .=	exists($f->{-widget}) && !$f->{-widget}
			? ''
			: !ref($f->{-widget}) && $f->{-widget}
			? $f->{-widget}
			: ref($f->{-widget}) eq 'CODE'
			? &{$f->{-widget}}($s, $f, cfpvv($s, $f), cfpvp($s, $f))
			: !$f->{-namecgi}
			? ''
			: ref($f->{-widget}) eq 'HASH'
			? $s->{-cgi}->submit(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}, -value=>$f->{-namelbl}
				, %{$f->{-widget}})
			: $s->{-cgi}->submit(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}, -value=>$f->{-namelbl}
				, map {defined($f->{$_}) ? ($_=>$f->{$_}) : ()} qw(-class -style));
		next
	}
	elsif ($bb) {
		print &$cfld($s, {}, $bb);
		$bb ='';
	}
	print &$cfld($s
	, $f->{-action} ||$f->{-preact}
		? {}
		: $f
	, (!$f->{-widget0}
		? ''
		: ref($f->{-widget0}) eq 'CODE'
		? &{$f->{-widget0}}($s, $f, cfpvv($s, $f), cfpvp($s, $f))
		: $f->{-widget0})
	. (!($f->{-action} || $f->{-preact}) && $f->{-namecgi} && defined(cfpvp($s, $f))
		? '<input type="hidden" name="' .$f->{-namecgi} .'__PV_" value="' 
			.$s->{-cgi}->escapeHTML(cfpvp($s, $f))
			.'" />'
		: ''
		)
	. (!ref($f->{-widget}) && exists($f->{-widget})
	? $f->{-widget}
	: ref($f->{-widget}) eq 'CODE'
	? &{$f->{-widget}}($s, $f, cfpvv($s, $f), cfpvp($s, $f))
	: !$f->{-namecgi}
	? ''
	: ref($f->{-widget}) eq 'HASH'
	? (	 $f->{-values}
		? $s->cgiselect(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}
			, -onchange=>1
			, map {defined($f->{$_}) ? ($_=>$f->{$_}) : ()} qw(-values -labels)
			, -id => $f->{-namecgi}
			, %{$f->{-widget}})
		: $f->{-rows}
		? $s->cgitext(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}
			, -id => $f->{-namecgi}
			, %{$f->{-widget}})
		: $f->{-action} ||$f->{-preact}
		? $s->{-cgi}->submit(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}, -value=>$f->{-namelbl}
			, -id => $f->{-namecgi}
			, %{$f->{-widget}})
		: $s->cgistring(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}
			, -id => $f->{-namecgi}
			, %{$f->{-widget}})
			)
	: (	 $f->{-values}
		? $s->cgiselect(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}
			, -id => $f->{-namecgi}
			, -onchange=>1			
			, map {defined($f->{$_}) ? ($_=>$f->{$_}) : ()} qw(-values -labels -onchange -readonly -disabled -class -style))
		: $f->{-rows}
		? $s->cgitext(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}
			, -id => $f->{-namecgi}
			, map {defined($f->{$_}) ? ($_=>$f->{$_}) : ()} qw(-rows -columns -maxlength -readonly -class -style))
		: $f->{-action} ||$f->{-preact}
		? $s->{-cgi}->submit(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}, -value=>$f->{-namelbl}
			, -id => $f->{-namecgi}
			, map {defined($f->{$_}) ? ($_=>$f->{$_}) : ()} qw(-class -style))
		: $s->cgistring(-name=>$f->{-namecgi}, -title=>$f->{-namecmt}
			, -id => $f->{-namecgi}
			, map {defined($f->{$_}) ? ($_=>$f->{$_}) : ()} qw(-size -maxlength -readonly -disabled -class -style))
			)
	)
	. (!$f->{-widget1}
		? ''
		: ref($f->{-widget1}) eq 'CODE'
		? &{$f->{-widget1}}($s, $f, cfpvv($s, $f), cfpvp($s, $f))
		: $f->{-widget1})
	);
 }
 if ($bb) {
	print &$cfld($s, {}, $bb);
	$bb ='';
 }
 print ref($cfld1) ? &{$cfld1}($s) : $cfld1;
 $err ? undef : 1
}
