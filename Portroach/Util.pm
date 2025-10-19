#------------------------------------------------------------------------------
# Copyright (C) 2011, Shaun Amott <shaun@inerd.com>
# Copyright (C) 2025 Fabien Romano <fabien@openbsd.org>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#
#------------------------------------------------------------------------------

package Portroach::Util;

use Portroach::Const;
use Portroach::Config;

use File::Path qw(make_path);
use List::MoreUtils qw(uniq);
use List::Util qw(first);

use URI;
use URI::Escape;
use HTML::Entities;

use Try::Tiny;
use Capture::Tiny 'capture';

require Exporter;

use strict;

require 5.006;

our @ISA = qw(Exporter);

our @EXPORT = qw(
	$date_regex
	$beta_regex
	$month_regex
	$ext_regex
	$verprfx_regex
	$verlike_regex
	$lang_regex

	&strchop
	&emptydir
	&isversion
	&isbeta
	&chopbeta
	&nametoregex
	&porttoregex
	&vertoregex
	&verguess
	&vercompare
	&betacompare
	&checkevenodd
	&extractfilenames
	&extractpages
	&extracthandlers
	&extractdirectories
	&extractsuffix
	&extractversion
	&extractgit
	&tobasepkgpath
	&fullpkgpathtoport
	&fullpkgpathtoleaf
	&fullpkgpathtosubcat
	&path_absolute
	&regress
	&info
	&debug
	&randstr
	&arrexists
	&wantport
	&primarycategory
	&lwp_useragent
	&uri_filename
	&uri_lastdir
	&getdbver
	&getstat
	&setstat
	&prepare_sql
	&finish_sql
	&connect_db
);


#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

our (%settings, @months, $date_regex, $beta_regex, $month_regex, $ext_regex,
    $verprfx_regex, $verlike_regex, $lang_regex);

my %beta_types;

my %want_regex = (
	port       => restrict2regex($settings{restrict_port}),
	category   => restrict2regex($settings{restrict_category}),
	maintainer => restrict2regex($settings{restrict_maintainer}),
	site       => restrict2regex('*' . $settings{restrict_site} . '*'),
);

# XXX vercompare handle long months but not date_regex
@months = (
	qr/jan(?:uary)?/,
	qr/feb(?:ruary)?/,
	qr/mar(?:ch)?/,
	qr/apr(?:il)?/,
	qr/may/,
	qr/jun(?:e)?/,
	qr/jul(?:y)?/,
	qr/aug(?:ust)?/,
	qr/sep(?:tember)?/,
	qr/oct(?:ober)?/,
	qr/nov(?:ember)?/,
	qr/dec(?:ember)?/
);

$month_regex = qr/
	jan(?:uary)?|
	feb(?:ruary)?|
	mar(?:ch)?|
	apr(?:il)?|
	may|
	jun(?:e)?|
	jul(?:y)?|
	aug(?:ust)?|
	sep(?:tember)?|
	oct(?:ober)?|
	nov(?:ember)?|
	dec(?:ember)?/x;
$date_regex  = qr/(?<!\d)(?:\d{2,4})?(?<SEP>[\.\-\_]?)
    (?:\d{2}|$month_regex)\k<SEP>\d{2,4}(?!\d)/x;

%beta_types = (
	snapshot   => { re => 'svn|cvs|snap(?:shot)?|nightly',	rank => 1 },
	unstable   => { re => 'unstable|dev|test|adhoc',	rank => 2 },
	milestone  => { re => 'milestone|m(?:\d+|$)',		rank => 3 },
	alpha      => { re => 'alpha|a(?:\d+|$)',		rank => 4 },
	beta       => { re => 'beta|b(?:\d+|$)',		rank => 5 },
	prerelease => { re => 'pre|pr?(?:\d+|$)',		rank => 6 },
	relcand    => { re => 'rc|\d[\.\-\_]?r(?:\d+|$)',	rank => 7 }
);

$beta_regex = join '|', map +($beta_types{$_}->{re}), keys %beta_types;

$ext_regex = qr/(?:l|t?b|t?g|t?x)?z(?:2|st)?|
    langpack|xpi|pl|uu|txt|bin|c|dictd|dtd|exe|F90|gem|html|ins|jar|kar|mp3|
    mmdb|otf|pdf|phar|rar|rpm|run|sfc|shar|spl|tar|tgz|ttf|txi|txt|uqm|war|
    zip/xi;

$verprfx_regex = qr/(?:v|ver|version|r|rel|release)(?:[\.\-\_]|(?=\d))/;

$verlike_regex = qr/(?:$beta_regex)?(?:
    \d+[\.\-\_]\d+[^\/]*?
    |$date_regex
    |\d{2,}([a-z]{,2}\d{,2})?
    |\d(?:\/|$))/x;

$lang_regex = qr/(?:cpp|hs|lua|mod|node|p5|perl|py\d?|ruby)/;

#------------------------------------------------------------------------------
# Func: strchop()
# Desc: Chop or pad string to $limit characters, using ellipses to contract.
#
# Args: $str   - String to manipulate.
#       $limit - Length of new string.
#
# Retn: $str   - Modified string.
#------------------------------------------------------------------------------

sub strchop
{
	my ($str, $limit) = @_;

	my $slen = int ($limit / 2) - 1;
	my $elen = ($limit - 1) - $slen;

	return '' if (!$str or !$limit);

	if (length $str > $limit)
	{
		return $str if ($str =~ s/^(.{$slen}).*(.{$elen})$/$1%$2/);
	}
	elsif (length $str < $limit)
	{
		return $str if $str .= ' ' x ($limit - length $str);
	}

	return $str;
}


#------------------------------------------------------------------------------
# Func: emptydir()
# Desc: Remove all files from a given directory, or create an empty directory
#       if it doesn't already exist.
#
# Args: $dir     - Directory to clear
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub emptydir
{
	my ($dir) = @_;

	return 0 if (!$dir);

	if (-d $dir) {
		opendir my $dh, "$dir";
		unlink "$dir/$_" foreach readdir($dh);
		closedir $dh;
	} else {
		make_path($dir);
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: isversion()
# Desc: Determine if a string looks like a valid version. If two strings are
#       provided, also check they both have the same schema.
#
# Args: $version 1 - Version to test.
#       $version 2 - Version to match.
#
# Retn: $isversion  - Looks like a version?
#------------------------------------------------------------------------------

sub isversion
{
	my $vera = shift;
	my $verb = shift;

	# XXX discard HASH for now ( new commit = new version ? )
	if ($vera =~ /[;=\?\[\]\(\)#]/ || ($vera !~ /^$date_regex$/ &&
		$vera =~ /^[0-9a-f]{10,40}$/)) {
		return 0;
	}
	if ($verb =~ /[;=\?\[\]\(\)#]/ || ($verb !~ /^$date_regex$/ &&
		$verb =~ /^[0-9a-f]{10,40}$/)) {
		return 0;
	}

	# Valid version are two digits schema, date, or single digit one letter
	if ($vera =~ /^\d+[\.\-\_]\d+/) {
		return 1 unless ($verb);
		return 1 if ($verb =~ /^\d+[\.\-\_]\d+/);
		$verb = quotemeta $verb;
		return 1 if ($vera =~ /^$verb/); # 7.0.1 ~ 7
		return 0;
	} elsif ($vera =~ /^$date_regex$/) {
		return 1 unless ($verb);
		return 1 if ($verb =~ /^$date_regex$/);
		return 0;
	} elsif ($vera =~ /^\d+(\D{,2}\d{,2})?([\.\-\_].+)?$/) {
		return 1 unless ($verb);
		return 1 if ($verb =~ /^\d+(\D{,2}\d{,2})?([\.\-\_].+)?$/);
		return 0;
	}

	return 0;
}


#------------------------------------------------------------------------------
# Func: isbeta()
# Desc: Determine if a version (or filename) looks like a beta/alpha/dev't
#       version.
#
# Args: $version - Version or full filename.
#
# Retn: $isbeta  - Looks like beta?
#------------------------------------------------------------------------------

sub isbeta
{
	my ($version) = @_;

	return (
		# XXX => \.\-\_ add ~ everywhere ?
		$version =~ /^(.*)[-_~.](?:$beta_regex).*$/gi
			or $version =~ /^(.*)(?<=\d)(?:$beta_regex).*$/gi
	);
}


#------------------------------------------------------------------------------
# Func: chopbeta()
# Desc: As above, but remove the beta extension from the string.
#
# Args: \$version - Version string.
#
# Retn: $isbeta   - Looks like beta (and therefore, $version modified)?
#------------------------------------------------------------------------------

sub chopbeta
{
	my ($version) = @_;

	$version = \$version if (!ref $version);

	return 1 if ($$version =~
	    s/^(.*)[\.\-\_](?:$beta_regex)\d*(?:\.\d+)*(.*)$/$1$2/gi);
	return 1 if ($$version =~
	    s/^(.*)(?<=\d)(?:$beta_regex)\d*(?:\.\d+)*(.*)$/$1$2/gi)
}


#------------------------------------------------------------------------------
# Func: nametoregex()
# Desc: Transform distfile/distname into a regex to matches. Escape regex
#       special chars like + ? { } ( ) and make them optional. Do lazy matching
#       against common separator - _ . and single digit to match any char or
#       none. Handle optional lang prefix (ex: py could be py- py_ or none).
#
# Args: $name  - Name string to transform into a regex
#
# Retn: $regex - A regular expression to match another name against
#------------------------------------------------------------------------------

sub nametoregex
{
	my $name = shift;
	my $regex;

	return lc $name if ($name =~ /^($lang_regex|g)$/i);

	if ($name =~ /\//) {
		foreach my $subname (split /\//, $name) {
			$regex .= '|' if ($regex);
			$regex .= nametoregex($subname);
		}
		return $regex;
	}

	$regex = lc $name;
	$regex =~ s/([\+\?\{\}])/\\$1?/g;
	$regex =~ s/[\.\-\_]/.?/g;
	$regex =~ s/^($lang_regex)/(?:$1)?\[\\-\\_\]?/i;
	$regex =~ s/^g/(?:g)?/i; # note, G for gnu ports: gtar -> tar
	# debug(__PACKAGE__, undef, "$name -> $regex");

	return $regex unless ($name =~ /\-|\_|\+|\d|[A-Z]/);

	my @names;
	@names = (@names, split(/\-/, $name))
		if (!@names && $name =~ /\-/);
	@names = (@names, split(/\_/, $name))
		if (!@names && $name =~ /\_/);
	@names = (@names, split(/\+/, $name))
		if (!@names && $name =~ /\+/);
	@names = (@names, split(/\d+/, $name))
		if (!@names && $name =~ /\d+/);
	@names = (@names, split(/(?=[A-Z])/, $name))
		if (!@names && $name =~ /[A-Z]+/);
	return $regex unless (scalar @names > 1);

	foreach my $subname (@names) {
		# debug(__PACKAGE__, undef, "subname $subname");
		# skip if empty (consecutive delimiter)
		next unless ($subname);
		if ($subname eq "p5") {
			$regex .= '|perl5?';
			next;
		}
		if ($subname eq "py") {
			$regex .= '|python';
			next;
		}
		next if ($subname =~ /^$lang_regex|g$/i);
		next if (length $subname == 1);

		my $q = nametoregex($subname);
		$regex .= "|$q";
	}

	return lc $regex;
}


#------------------------------------------------------------------------------
# Func: porttoregex()
# Desc: Transform $port into a regex to matches as a version prefix.
#
# Args: $port  - Port hash which contains name, fullpkgpath, homepage
#       $sites - Array of sites to extract names from their host part
#
# Retn: $regex - A regular expression to match another name against
#------------------------------------------------------------------------------

sub porttoregex
{
	my ($port, $sites) = @_;
	my ($basename, $pathname, $catname, @sites, $basename_q, $pathname_q,
	    @name_q, $is_build, $is_strict, @cats, $dist_q);

	# Prepare name queries to guess version later on
	$basename = $port->{name};
	$basename =~ s/(\-\D[^\-]*)+$//g;
	$basename =~ s/^(.*)-([^-]*)$/$1/g;
	$basename_q = nametoregex($basename);
	$basename_q = quotemeta($basename) unless($basename_q);
	#debug(__PACKAGE__, $port, "basename regex, "
	#    . "$basename -> $basename_q");
	push(@name_q, $basename_q);
	$pathname = fullpkgpathtoport($port->{fullpkgpath});
	if ($basename ne $pathname) {
		$pathname_q = nametoregex($pathname);
		$pathname_q = quotemeta($pathname) unless($pathname_q);
		#debug(__PACKAGE__, $port, "pathname regex, "
		#    . "$pathname -> $pathname_q");
		push(@name_q, $pathname_q);
	}

	if (!$sites) {
		# Use during check, we already have mastersites built
		@sites = split(' ', $port->{mastersites});
		push(@sites, $port->{homepage});
	} elsif (scalar @$sites) {
		# Use during build, sites isn't empty so we can add homepage
		$is_build = 1;
		@sites = @$sites;
		push(@sites, $port->{homepage});
	} else {
		# Use to extract handler during check, we do not want to pollute
		# the regex with site so we keep the list empty.
		$is_strict = 1;
	}

	# Add extra guess based on fullpkgpath
	@cats = fullpkgpathtosubcat($port->{fullpkgpath}) if (!$is_strict);
	foreach $catname (@cats) {
		my $q = nametoregex($catname);
		#debug(__PACKAGE__,$port,"catname regex, $catname -> $q");
		push(@name_q, $q);
		if ($catname =~ /^lib/) {
			$q = $catname;
			$q =~ s/^lib//;
			$q = nametoregex($q);
			#debug(__PACKAGE__, $port, "(lib)cat regex, "
			#    . "$catname -> $q");
			push(@name_q, $q);
		}
	}

	# Add extra guesses based on SITES/HOMEPAGE
	foreach my $site (@sites) {
		next unless ($site);
		my $name;
		#debug(__PACKAGE__, $port, "extract host regex from $site");
		if ($site =~ m:/([^/]+)\.git/:) {
			$name = nametoregex($1);
			#debug(__PACKAGE__, $port, "site .git regex, "
			#    . "$site -> $name");
			push(@name_q, $name);
		}
		$name = Portroach::SiteHandler->FindName($site);
		if ($name) {
			$name = nametoregex($name);
			#debug(__PACKAGE__, $port, "site handler regex, "
			#    . "$site -> $name");
			push(@name_q, $name);
			next;
		}
		try {
			$name = URI->new($site)->host;
		} catch {
			print STDERR "$port->{fullpkgpath}: "
			    . "caught error on $site\n";
			debug(__PACKAGE__, $port, "$_");
			next;
		};
		$name =~ s/\.[^\.]+$//;
		$name =~ s/^[^\.]+\.//;
		# XXX better way ?
		next if ($name =~ /(github|python)/);
		if ($name) {
			$name = nametoregex($name);
			#debug(__PACKAGE__, $port, "site host regex, "
			#    . "$site -> $name");
			push(@name_q, $name);
		}
	}

	# Add extra guess based on basename (pkgname), ONLY during build
	if ($is_build && $basename =~ /^lib/) {
		my $q = $basename;
		$q =~ s/^lib//;
		$q = nametoregex($q);
		#debug(__PACKAGE__, $port, "(lib)name regex, $basename -> $q");
		push(@name_q, $q);
	}
	if ($is_build && $basename =~ /^\d{2,}\D/) {
		my $q = $basename;
		$q =~ s/^\d{2,}//;
		$q = nametoregex($q);
		#debug(__PACKAGE__, $port, "name\$ regex, $basename -> $q");
		push(@name_q, $q);
		$q = $basename;
		$q =~ s/^(\d{2,}).*?$/$1/;
		$q = nametoregex($q);
		#debug(__PACKAGE__, $port, "^digit regex, $basename -> $q");
		push(@name_q, $q);
	} elsif ($is_build && $basename =~ /\D\d{2,}$/) {
		my $q = $basename;
		$q =~ s/\d{2,}$//;
		$q = nametoregex($q);
		#debug(__PACKAGE__, $port, "^name regex, $basename -> $q");
		push(@name_q, $q);
		$q = $basename;
		$q =~ s/^.*?(\d{2,})$/$1/;
		$q = nametoregex($q);
		#debug(__PACKAGE__, $port, "digit\$ regex, $basename -> $q");
		push(@name_q, $q);
	} elsif ($is_build && $basename =~ /\D\d$/) {
		my $q = $basename;
		$q =~ s/\d$//;
		$q = nametoregex($q);
		#debug(__PACKAGE__, $port, "^name regex, $basename -> $q");
		push(@name_q, $q);
	}

	# Finally compute the regex
	foreach my $q (@name_q) {
		next unless ($q =~ /\|/);
		my @qs = split (/\|/, $q);
		for my $i (0 .. $#qs) {
			next unless ($qs[$i]);
			push(@name_q, $qs[$i]);
			#debug(__PACKAGE__, $port, "split $q -> $qs[$i]");
		}
	}
	@name_q = sort { length $b <=> length $a } uniq @name_q;
	foreach my $q (@name_q) {
		#debug(__PACKAGE__, $port, "skip $q") if ($q =~ /\|/);
		next if ($q =~ /\|/); # already splited, skip duplicate re
		next unless ($q);
		$dist_q .= '|' if ($dist_q);
		$dist_q .= $q;
	}
	#debug(__PACKAGE__, $port, "porttoregex $port->{name} -> $dist_q");
	return $dist_q;
}


#------------------------------------------------------------------------------
# Func: vertoregex()
# Desc: Transform version into a regex to matches. Escape regex special chars
#       like + ? { } ( ) and make them optional. Do lazy matching against
#       common separator - _ . and single digit to match any char or
#       none.
#
# Args: $ver  - Version string to transform into a regex
#
# Retn: $regex - A regular expression to match another name against
#------------------------------------------------------------------------------

sub vertoregex
{
	my $ver = shift;
	my $regex = lc $ver;

	$regex =~ s/([\+\?\{\}])/\\$1?/g;
	$regex =~ s/[\.\-\_]/.?/g;
	$regex =~ s/^($lang_regex)/(?:$1)?\[\\-\\_\]?/;

	return $regex;
}


#------------------------------------------------------------------------------
# Func: verguess()
# Desc: Guess possible "next version" values from given string.
#       For example: 1.4.2 -> (2.0.0, 1.5.0, 1.4.3)
#
# Args: $ver         - Current version string
#       $evenoddpart - Incremement nth component by TWO to keep even/odd
#
# Retn: @ver         - List of possible new versions
#------------------------------------------------------------------------------

sub verguess
{
	my ($ver, $evenoddpart) = @_;
	my @ver_guesses;

	return if (!$ver);

	my @vparts = split /(\D+)/, $ver;

	my $i = 0;
	for (0 .. $#vparts) {
		my $guess;

		my $v = $vparts[$i];

		if ($v =~ /^\d+$/) {
			if (defined $evenoddpart and $evenoddpart == $i/2) {
				$v+=2;
			} else {
				$v++;
			}
		} else {
			$i++;
			next;
		}

		$guess .= $vparts[$_] for (0 .. ($i - 1));
		$guess .= $v;

		for (++$i .. $#vparts) {
			if ($vparts[$_] =~ /^\d+$/) {
				$guess .= '0' x length $vparts[$_];
			} elsif ($vparts[$_] =~ /^-?[A-Z]+-?$/i) {
				last;
			} else {
				$guess .= $vparts[$_];
			}
		}

		push @ver_guesses, $guess;
	}

	return @ver_guesses;
}


#------------------------------------------------------------------------------
# Func: vercompare()
# Desc: Compare two version strings and return true if $new is greater than
#       $old; otherwise return false.
#
# Args: $new       - New version string
#       $old       - Old version string
#       $recursive - Call recusrsively ?
#
# Retn: $result - Is $new greater than $old? Returns -1 for "Maybe"
#------------------------------------------------------------------------------

sub vercompare
{
	my ($new, $old, $recursive) = @_;

	unless ($recursive) {
		my $result = vercompare($new, $old, 1);
		debug(__PACKAGE__, undef, "=> $new > $old") if ($result == 1);
		debug(__PACKAGE__, undef, "=> $new ~> $old") if ($result == -1);
		debug(__PACKAGE__, undef, "=> $new <= $old") if ($result == 0);
		return $result;
	}

	#debug(__PACKAGE__, undef, "vercompare $new vs $old");

	# 10.2+2.0.1 Vs 10+2.0
	if ($new =~ /\+$verlike_regex/ || $old =~ /\+$verlike_regex/) {
		my @news = split('\+', $new);
		my @olds = split('\+', $old);
		if (scalar @news == scalar @olds) {
			#debug(__PACKAGE__, undef, "SPLIT $new vs $old");
			for my $i (0 .. $#news) {
				my $new  = $news[$i];
				my $old = $olds[$i];
				next if ($new eq $old);
				return vercompare($new, $old, 1);
			}
			#debug(__PACKAGE__, undef, "SPLIT => $new equal $old");
			return 0;
		}
	}

	# Check for version with a single alphabetical character as to not to
	# get entangled with rc,beta,alpha,whatnot. These are dealt with below.
	# Get the last char, check if the remaining version is equal (otherwise
	# it will be properly compared below) and return the comparison of the
	# letters.
	if (($new =~ m/.*?([a-zA-Z]{1})$/) || ($old =~ m/.*?([a-zA-Z]{1})$/)) {
		my ($new_v, $new_c, $old_v, $old_c);

		# Now save the last chars and versions.
		($new_v, $new_c) = $new =~ m/(.*?)([a-zA-Z]{1})$/;
		($old_v, $old_c) = $old =~ m/(.*?)([a-zA-Z]{1})$/;

		#debug(__PACKAGE__, undef,
		#    "CHAR $new_v($new_c) vs $old_v($old_c)");

		if ($new_v eq $old_v) {
			return (($new_c cmp $old_c) == 1) ? 1 : 0;
		}
	}

	# Attempt to stop false positives on versions that
	# look newer - e.g. 2.5 is newer than 2.5-prerelease3

	if (1) {
		my $_new = $new;
		my $_old = $old;

		my ($newbeta, $oldbeta, $newdots, $olddots);

		if (chopbeta(\$_new)) {
			#debug(__PACKAGE__, undef, "BETA new v $new -> $_new");
			# $new and $old equal except for beta bit
			# Therefore, $old (a final release) is newer
			return 0 if ($_new eq $old);

			$newbeta = 1;
		}

		if (chopbeta(\$_old)) {
			#debug(__PACKAGE__, undef, "BETA old v $old -> $_old");
			# $new and $old equal except for beta bit
			# Therefore, $new (a final release) is newer
			return 1 if ($_old eq $new);

			$oldbeta = 1;
		}

		$olddots = $_old;
		$olddots =~ s/[^.]//g;
		$olddots = length $olddots;

		$newdots = $_new;
		$newdots =~ s/[^.]//g;
		$newdots = length $newdots;

		if ($newbeta && $oldbeta && $newdots == $olddots) {
			# Both had beta bits; non-beta bits
			# have same number of components
			# Therefore, don't remove beta bits.

			# ... if just the non-beta bits
			# differ, compare them.
			return (betacompare($new, $old))
				if ($_new eq $_old);
			#debug(__PACKAGE__, undef, "no need to trim beta");
		} else {
			# Remove beta bits, as non-beta bits
			# differ and can be compared.
			#debug(__PACKAGE__, undef, "trim beta -> $new vs $old")
			#    unless ($new eq $_new && $old eq $_old);
			$new = $_new;
			$old = $_old;
		}
	}

	# If both version strings contain a date AND other
	# numbers, take care to split them and compare
	# individually.

	unless ($new =~ /^$date_regex$/ && $old =~ /^$date_regex$/)
	{
		# XXX -> [\.\-\_]
		if ($new =~ /^(.*?)[\-\.]?($date_regex)[\-\.]?(.*)$/) {
			my ($new_1, $new_2, $new_3) = ($1, $2, $4);

			# XXX -> [\.\-\_]
			if ($old =~ /^(.*?)[\-\.]?($date_regex)[\-\.]?(.*)$/) {
				my ($old_1, $old_2, $old_3) = ($1, $2, $4);
				#debug(__PACKAGE__, undef,
				#    "vercompare old $old_1, $old_2, $old_3"
				#    . " new $new_1, $new_2, $new_3");

				if ($new_1 and $old_1) {
					return vercompare($new_1, $old_1, 1)
					    unless ($new_1 eq $old_1);
				}

				if ($new_2 and $old_2) {
					return vercompare($new_2, $old_2, 1)
					    unless ($new_2 eq $old_2);
				}

				if ($new_3 and $old_3) {
					return vercompare($new_3, $old_3, 1)
					    unless ($new_3 eq $old_3);
				} elsif ($new_3) {
					return 1;
				} else {
					return 0;
				}
			}
		}
	}

	# Give month names a numerical value

	if ($new =~ /$month_regex/) {
		my $i = 1;
		foreach my $m (@months) {
			$new =~ s/$m/sprintf "%02d", $i/ge;
			$i++;
		}
	}

	if ($old =~ /$month_regex/) {
		my $i = 1;
		foreach my $m (@months) {
			$old =~ s/$m/sprintf "%02d", $i/ge;
			$i++;
		}
	}

	# Check for digit only version

	if ($new =~ /^\d+$/ && $old =~ /^\d+$/) {
		#debug(__PACKAGE__, undef, "vercompare digit $new vs $old");
		return 1 if (0+$new > 0+$old);
		return 0 if (0+$new < 0+$old);
	}

	# Split version shema then compare its components
	my @nums_new = split /[\.\-\_\+\~]+/, $new;
	my @nums_old = split /[\.\-\_\+\~]+/, $old;

	# Make sure x.y.z is newer than xxxxx but not x+1
	#debug(__PACKAGE__, undef, "vercompare "
	#    . "$new ($#nums_new) vs $old ($#nums_old)");
	return 0 if ($#nums_new == 0 && $#nums_old > 0 and
	    length $nums_new[0] != length $nums_old[0] and
	    int($nums_new[0]) != int($nums_old[0])+1);

	foreach my $n (0 .. $#nums_new) {
		my ($num_new, $num_old);
		#debug(__PACKAGE__, undef, "vercompare "
		#    . $nums_new[$n]." / ".$nums_old[$n]);
		# All preceding components are equal, so assume newer.
		return 1 if (!defined($nums_old[$n]));

		# XXX experimental, let's check & see
		# Attempt to handle cases where version component lengths vary.
		if ($n == $#nums_new && $nums_old[$n] ne '0' &&
		    length $nums_new[$n] != length $nums_old[$n] &&
		    $nums_new[$n] =~ /^\d+$/ && $nums_old[$n] =~ /^\d+$/)
		{
			my $lendiff_thresh;

			$lendiff_thresh =
			    ($nums_new[$n] =~ /^0/ && $nums_old[$n] =~ /^0/)
			    ? 1
			    : 2;

			$nums_new[$n] = $nums_new[$n] . ('0' x length $1)
			    if ($nums_old[$n] =~ /^(0+)/);
			$nums_old[$n] = $nums_old[$n] . ('0' x length $1)
			    if ($nums_new[$n] =~ /^(0+)/);

			# Experimental, catch (some) "backwards" version numbers

			my ($lendiff, $first_old, $first_new);

			$lendiff = length($nums_new[$n])-length($nums_old[$n]);
			$first_new = substr($nums_new[$n], 0, 1);
			$first_old = substr($nums_old[$n], 0, 1);

			if ($lendiff >= $lendiff_thresh) {
				#debug(__PACKAGE__, undef,
				#    "diff $lendiff >= tresh $lendiff_thresh");
				if ($first_new > $first_old) {
					#debug(__PACKAGE__, undef,
					#    "$first_new > $first_old : -1");
					return -1;
				} elsif ($first_new == $first_old) {
					$nums_old[$n] .= ('0' x $lendiff);
					#debug(__PACKAGE__, undef,
					#    "$nums_new[$n] > $nums_old[$n] ?");
					return ($nums_new[$n] > $nums_old[$n]
					    ? -1 : 0);
				} else {
					#debug(__PACKAGE__, undef,
					#    "$first_new < $first_old : 0");
					return 0;
				}
			} elsif ($lendiff <= -$lendiff_thresh) {
				#debug(__PACKAGE__, undef,
				#    "diff $lendiff <= tresh $lendiff_thresh");
				if ($first_new < $first_old) {
					#debug(__PACKAGE__, undef,
					#    "$first_new < $first_old : 0");
					return 0;
				} elsif ($first_new == $first_old) {
					$nums_new[$n] .= ('0' x abs $lendiff);
					#debug(__PACKAGE__, undef,
					#    "$nums_new[$n] < $nums_old[$n] ?");
					return ($nums_new[$n] < $nums_old[$n]
					    ? 0 : -1);
				} else {
					#debug(__PACKAGE__, undef,
					#    "$first_new > $first_old : -1");
					return -1;
				}
			}
		}

		# Compare values numerically
		$num_new = 0+$nums_new[$n];
		$num_old = 0+$nums_old[$n];
		#debug(__PACKAGE__, undef, "num compare $num_new / $num_old");
		return 1 if ($num_new > $num_old);
		return 0 if ($num_new < $num_old);

		# On equal values, compare suffix if any
		if ($nums_new[$n] =~ /[^\d]/ || $nums_old[$n] =~ /[^\d]/) {
			my $cmp;
			$nums_new[$n] =~ s/\d*$num_new//;
			$nums_old[$n] =~ s/\d*$num_old//;
			#debug(__PACKAGE__, undef, "suffix compare " .
			#    $nums_new[$n]." / ".$nums_old[$n]);
			$cmp = $nums_new[$n] cmp $nums_old[$n];
			return 0 if ($cmp < 0);
			return 1 if ($cmp > 0);
		}
	}

	# Handle versions like 1.0_suffix; strip away any hyphens or underbars.
	$new =~ s/([-_]+[a-zA-Z]+)+$//;
	$old =~ s/([-_]+[a-zA-Z]+)+$//;

	# Fall back to string compare
	return (($new cmp $old) == 1) ? 1 : 0;
}


#------------------------------------------------------------------------------
# Func: betacompare()
# Desc: Compare beta bits of two versions strings and return true if $new is
#       greater than $old; otherwise return false.
#
#       Result is undefined if either string doesn't contain a beta portion.
#
# Args: $ver    - New version string
#       $old    - Old version string
#
# Retn: $result - Is $new greater than $old? Returns -1 for "Maybe"
#------------------------------------------------------------------------------

sub betacompare
{
	my ($new, $old) = @_;

	#debug(__PACKAGE__, undef, "betacompare $new vs $old");

	my $newrank = 0;
	my $oldrank = 0;
	my $newnums = 0;
	my $oldnums = 0;

	foreach my $bt (keys %beta_types) {
		my $re   = $beta_types{$bt}->{re};
		my $rank = $beta_types{$bt}->{rank};

		if ($new =~ /[\.\-\_](($re)(\d*(?:\.\d+)*))$/ ||
		    $new =~ /(?<=\d)(($re)(\d*(?:\.\d+)*))/) {
			$newrank = $rank;
			$newnums = $1 if $1;
			$newnums =~ s/^\D+//;
		}

		if ($old =~ /[\.\-\_](($re)(\d*(?:\.\d+)*))/ ||
		    $old =~ /(?<=\d)(($re)(\d*(?:\.\d+)*))/) {
			$oldrank = $rank;
			$oldnums = $1 if $1;
			$oldnums =~ s/^\D+//;
		}
	}
	#debug(__PACKAGE__, undef,
	#    "betacompare ($newrank/$newnums) vs ($oldrank/$oldnums)");

	if ($oldrank == $newrank) {
		my @nums_new = split /\D+/, $newnums;
		my @nums_old = split /\D+/, $oldnums;

		foreach my $n (0 .. $#nums_new) {
			# New version component; all preceding
			# components are equal, so assume newer.
			return 1 if (!defined($nums_old[$n]));

			return 1 if (0+$nums_new[$n] > 0+$nums_old[$n]);
			return 0 if (0+$nums_new[$n] < 0+$nums_old[$n]);
		}

		# All numbers equal
		return 0;
	}

	return ($newrank > $oldrank ? 1 : 0);
}


#------------------------------------------------------------------------------
# Func: checkevenodd()
# Desc: Check that a version component is either even or odd.
#
# Args: $version   - Version string to check
#       $evenodd   - True = force even; false = force false
#       $component - Zero-based component number to check
#
# Retn: $result    - true/false
#------------------------------------------------------------------------------

sub checkevenodd
{
	my ($version, $evenodd, $component) = @_;

	my @bits = split /\D+/, $version;

	return 0 if $#bits < $component;

	if ($bits[$component] % 2) {
		return !$evenodd;
	} else {
		return $evenodd;
	}
}


#------------------------------------------------------------------------------
# Func: extractfilenames()
# Desc: Extract filenames (and dates, where possible) from a mastersite index
#
# Args: $data    - Data from master site request.
#       $sufx    - Distfile suffix (e.g. ".tar.gz")
#       \$files  - Where to put filenames found.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub extractfilenames
{
	my ($data, $sufx, $files) = @_;

	$sufx = quotemeta $sufx;

	foreach (split "<", $data) {
		next unless (/^a\s+href\s*=\s*('|")(.*?)\1/i);
		my $link = $2;
		next unless $link =~ /$sufx$/i;
		my $file = uri_unescape($link);
		debug(__PACKAGE__, undef, "unescape $link -> $file")
		    if ($file ne $link);
		push @$files, $file;
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: extractpages()
# Desc: Extract plausible download pages from an homepage
#
# Args: $resp    - Response from site request.
#       \$pages  - Where to put pages found.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub extractpages
{
	my ($resp, $pages) = @_;
	my ($page_re, $ext_re, $base, $html, $link, $sh, $page);

	$page_re = qr/(?<!\#)[^\#]*(developers|down|community|contributing|
		software|source)/x;
	$ext_re = qr/(\/|\.html|\.php|(^|\/)[^\.]*)/;
	$base = $resp->base;
	$base =~ s/[^\/]+$//;

	foreach $html (split "<", $resp->content) {
		next unless ($html =~ /^a\s.*(?<=\s)href\s*=\s*('|")(.*?)\1/i);
		#debug(__PACKAGE__, undef, "page ? $html");
		$link = $2;
		# debug(__PACKAGE__, undef, "$link !~ /$page_re/i")
		#     unless ($link =~ /$page_re/i);
		next unless ($link =~ /$page_re/i);
		# debug(__PACKAGE__, undef, "$link !~ /$ext_re\$/i")
		#     unless ($link =~ /$ext_re$/i);
		next unless ($link =~ /$ext_re$/i);
		$sh = Portroach::SiteHandler->FindHandler($link);
		debug(__PACKAGE__, undef, "skip page handler $link") if ($sh);
		next if ($sh);
		if ($link !~ /^(.*?:\/\/|\/)/) {
			#debug(__PACKAGE__, undef, "$link => ".$base.".$link");
			$link = $base.$link;
		}
		$page = path_absolute($link);
		#debug(__PACKAGE__, undef, "absolute $link -> $page")
		#    if ($page ne $link);
		$link = $page;
		$page = uri_unescape($link);
		#debug(__PACKAGE__, undef, "unescape $link -> $page")
		#    if ($page ne $link);
		if ( first { $page eq $_ } @$pages) {
			debug(__PACKAGE__,undef,"already record, skip $page");
			next;
		}
		#debug(__PACKAGE__, undef, "push page $page");
		push @$pages, $page;
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: extracthandlers()
# Desc: Extract site handler from a webpage
#
# Args: $data      - Data from master site request.
#       $port      - Port hash which contains name to filter unrelated handlers.
#       \$handlers - Where to put handlers found.
#
# Retn: $success   - true/false
#------------------------------------------------------------------------------

sub extracthandlers
{
	my ($data, $port, $handlers) = @_;
	my ($name_q, $html, $link, $sh, $name);

	$name_q = porttoregex($port, []); # XXX strict minimum

	debug(__PACKAGE__, undef, "extract handlers =~ $name_q");
	foreach $html (split "<", $data) {
		next unless ($html =~ /^a\s.*(?<=\s)href\s*=\s*('|")(.*?)\1/i);
		#debug(__PACKAGE__, undef, "handler ? $html");
		$link = $2;
		$link = 'https:'.$link if ($link =~ m:^//:);
		$sh = Portroach::SiteHandler->FindHandler($link);
		#debug(__PACKAGE__, undef, "!SiteHandler->FindHandler($link)")
		#    unless ($sh);
		next unless ($sh);
		$name = $sh->GetName($link);
		#debug(__PACKAGE__, undef, "!sh->GetName($link)")
		#    unless ($name);
		next unless ($name);
		debug(__PACKAGE__, undef, "$link !~ /$name_q/i")
		    unless ($link =~ /$name_q/i);
		next unless ($link =~ /$name_q/i);
		debug(__PACKAGE__, undef, "$link name $name !~ /$name_q/i")
		    unless ($name =~ /(^|\/)($name_q)(\/|$)/i);
		next unless ($name =~ /(^|\/)($name_q)(\/|$)/i);
		$sh->{site} = $link;
		debug(__PACKAGE__,undef,"push handler $sh->{name} for $link");
		push @$handlers, $sh;
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: extractdirectories()
# Desc: Extract directories from a mastersite index, going down the path only.
#
# Args: $resp    - Response from site request.
#       $site    - Actual, full path on the site to resolve/filter link.
#       \$dirs   - Where to put directories found.
#       $verlike - 0|1 return path matching or not a version schema
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub extractdirectories
{
	my ($resp, $site, $dirs, $verlike) = @_;
	my ($host_q, $path_q, $base, $html, $link);

	$host_q = $site->host;
	$host_q = s/.*\.([^\.]*?\.[^\.])$/$1/; # check only for top domain
	$host_q = quotemeta $host_q;
	$path_q = quotemeta $site->path;
	$base = $resp->base;
	$base =~ s/[^\/]+$//;

	foreach $html (split "<", $resp->content) {
		next unless ($html =~ /^a\s.*(?<=\s)href\s*=\s*('|")(.*?)\1/i);
		$link = $2;
		#debug(__PACKAGE__, undef, "$link =~ /\/\$/")
		#    unless ($link =~ /\/$/);
		next unless $link =~ /\/$/;
		# check for version
		#debug(__PACKAGE__, undef, "$link =~ /$verlike_regex/i")
		#    if (!$verlike and $link =~ /$verlike_regex/i);
		next if (!$verlike and $link =~ /$verlike_regex/i);
		#debug(__PACKAGE__, undef, "$link !~ /$verlike_regex/i")
		#    if ($verlike and $link !~ /$verlike_regex/i);
		next if ($verlike and $link !~ /$verlike_regex/i);

		if ($link !~ /^(.*?:\/\/|\/)/) {
			#debug(__PACKAGE__, undef, "$link => ".$base.".$link");
			$link = $base.$link;
		}
		if ($link =~ /^.*?:\/\//) {
			next if ($link !~ /^.*?:\/\/[^\/]*$host_q\//i);
			#debug(__PACKAGE__, undef,
			#    "skip, $link !~ s/^.*?:\/\/[^\/]*$host_q\//\//i");
			$link =~ s/^.*?:\/\/[^\/]*$host_q\//\//i;
		}
		$link = path_absolute($link);
		$link =~ s/[^\/]+$//;
		if ($link eq $site->path) {
			#debug(__PACKAGE__, undef,
			#    "skip, $link eq ".$site->path);
			next;
		}
		if ($link !~ /^$path_q[^\/]+/i) {
			#debug(__PACKAGE__, undef,
			#    "skip, $link !~ /^$path_q\[^\/]+/i");
			next;
		}

		next if (grep { $_ eq $link } @$dirs);
		#debug(__PACKAGE__, undef, "push dir $link");
		push @$dirs, $link;
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: extractsuffix()
# Desc: Extract suffixe from a filename
#
# Args: $filename - Data from master site request.
#
# Retn: $sufx     - true/false
#------------------------------------------------------------------------------

sub extractsuffix
{
	my $sufx = shift;
	return unless ($sufx =~ s/^(.*?)((\.($ext_regex))+)$/$2/i);
	return $sufx;
}


#------------------------------------------------------------------------------
# Func: extractversion()
# Desc: Extract version from a path, need port to handle names' prefix
#
# Args: $port   - Port hash which contains name, fullpkgpath, homepage
#       $path   - String to extract version from
#
# Retn: $ver    - version string or undef
#------------------------------------------------------------------------------

sub extractversion
{
	my ($port, $path) = @_;
	my ($dist_q, $chop_re, $chop_q, $re, @paths);

	$chop_re = $settings{build_chop};
	$dist_q = porttoregex($port, []); # XXX should be lazy, try being strict
	$chop_q = qr/[\.\-\_\+]?(?:$chop_re|$dist_q|$lang_regex)[\.\-\_\+]?/x;
	$re = qr:($verprfx_regex)?($chop_q)*+((?<!\d)\.|\-|\_)?
	    ($verprfx_regex)?($verlike_regex)($chop_q)*+:xi;
	#debug(__PACKAGE__, $port, "extract $path version $re");
	if ($path =~ /\//) {
		@paths = split /\//, $path;
	} else {
		push (@paths, $path);
	}
	foreach $path (@paths) {
		return $5 if ($path =~ m:^$re$:);
	}
	return undef;
}


#------------------------------------------------------------------------------
# Func: extractgit()
# Desc: Extract tags(versions) from a git url, need port to handle names' prefix
#
# Args: $port    - Port hash which contains name, fullpkgpath, homepage
#       $giturl  - Url to use in order to list tags remotly
#       $fileurl - Url to use in order to fetch matching tag archive
#       \$files  - Where to put filenames found.
#
# Retn: $ntag   - -1 on error otherwise number of tags found
#------------------------------------------------------------------------------

sub extractgit
{
	my ($port, $giturl, $fileurl, $files) = @_;
	my ($gitcmd, $out, $err, $rc, @tags);

# XXX extract date from git, for later use with better site handler api
#https://stackoverflow.com/questions/65729722/git-ls-remote-tags-how-to-get-date-information
# git init repo
# cd repo
# git config extensions.partialClone true
# git remote add origin https://github.com/shopify/sarama
# time git fetch --filter=blob:none --tags --depth=1 origin
# git tag -l | xargs -t -n1 git log --format=%cd

	$gitcmd = "GIT_TERMINAL_PROMPT=0 /usr/local/bin/git".
	    " -c 'versionsort.suffix=-' ls-remote".
	    " --tags --sort='v:refname'";
	debug(__PACKAGE__, $port, "$gitcmd $giturl");
	($out, $err, $rc) = capture {
		system("$gitcmd $giturl");
	};
	if ($rc != 0 or $err) {
		$err .= '\n' if ($err !~ /\n$/g);
		info(1, $port->{fullpkgpath}, strchop($giturl, 60)
		    . ': ' . "$rc, $err");
		return -1;
	}
	foreach my $output (reverse(split /^/m, $out)) {
		my ($commit, $tag) = split(/\s+/, $output);
		next if ($tag =~ /\^\{\}$/);
		$tag =~ s/refs\/tags\///;
		#debug(__PACKAGE__, $port, "tag $tag");
		push(@tags, $tag);
	}
	foreach my $tag (@tags) {
		my ($ver, $version);
		$ver = lc $tag;

		debug(__PACKAGE__, $port, "replace ver/ver by ver+ver -> $ver")
		    if ($ver =~ s/(?=$verlike_regex)\/(?=$verlike_regex)/\+/g);

		debug(__PACKAGE__, $port, "replace / by _ -> $ver")
		    if ($ver =~ s/\//\_/g);

		$version = extractversion($port, $ver);
		debug(__PACKAGE__, $port, "extract $ver -> '$version'")
		    if ($ver ne $version);
		$ver = $version;

		debug(__PACKAGE__, $port, "trim (v|r) marker -> $ver")
		    if ($ver =~ s/^$verprfx_regex//);

		# use hardcoded sufx instead of $port->{sufx}
		# bsd.port.mk EXTRACT_SUFX ?= .tar.gz
		# font.port.mk EXTRACT_SUFX ?=	.zip
		$ver = $fileurl . $tag . ".tar.gz%%$ver";
		push(@$files, $ver);

		# XXX lazy matching ?
		# $tag = vertoregex($port->{ver});
		debug(__PACKAGE__, $port, "stop loading tag, old found $ver")
		    if ($ver eq $port->{ver});
		last if ($ver eq $port->{ver});
	}
}


#------------------------------------------------------------------------------
# Func: path_absolute()
# Desc: Given a URL string, trim ../ ./ // and may return ""
#
# Args: $path - URL to trim
#
# Retn: $abspath - URL trimmed.
#------------------------------------------------------------------------------

sub path_absolute
{
	my $abspath = shift;
	while ($abspath =~ s:
		(?<=[^\:]|^)/+(?=/)
		| ^/?\.\.(?=/)
		| ^\.\.$
		| (?<=/)(?!\.\./)[^/]+/\.\./
		| (?<![^/])\./
		::x) {};
	return $abspath;
}

#------------------------------------------------------------------------------
# Func: regress()
# Desc: Print either 'REGRESS' on STDERR or 'FIX' using info(0,...) depending on
#       regress setting.
#
# Args: @str - Array of message parts to chop and format.
#       $msg - Message to print unformatted after other parts.
#
# Retn: n/a
#------------------------------------------------------------------------------

sub regress
{
	my @items = (@_);
	my ($str, $msg);

	$msg = pop (@items);
	if ($settings{regress}) {
		$msg = "FIX, " . $msg;
		info(0, @items, $msg);
		return;
	}

	$msg = "REGRESS, " . $msg;
	foreach (@items) {
		$str .= ' ' if ($str);
		$str .= $_ . ':';
	}
	print STDERR "$str $msg\n";
}

#------------------------------------------------------------------------------
# Func: info()
# Desc: Format arguments into message and print.
#
# Args: $verbose - 0/1 print depending on verbose setting.
#       @str - Array of message parts to chop and format.
#       $msg - Message to print unformatted after other parts.
#
# Retn: n/a
#------------------------------------------------------------------------------

sub info
{
	my $verbose = shift;
	my @items = (@_);
	my ($str, $msg);

	# use at most two: [--20--] [--20--] $msg
	my $width = 20;

	return if ($verbose && !$settings{verbose});

	$msg = pop (@items);

	foreach (@items) {
		$str .= ' ' if ($str);
		$str .= '[' . strchop($_, $width) . ']';
	}

	if ($str) {
		print "$str $msg\n";
	} else {
		print "$msg\n";
	}
}

#------------------------------------------------------------------------------
# Func: debug()
# Desc: Print a debug message.
#
# Args: $package - Prefix message with "($package), ", usually __PACKAGE__.
#       $port - Prefix message with "$port->{fullpkgpath}: " if defined.
#       $msg - Message.
#
# Retn: n/a
#------------------------------------------------------------------------------

sub debug
{
	return unless ($settings{debug});
	my $package = shift;
	my $port = shift;
	my ($msg) = @_;
	return if (!$msg);
	my $debug = "($package) ";
	$debug .= $port->{fullpkgpath} . ': ' if ($port);
	$debug .= $msg;
	print STDERR "$debug\n";
}

#------------------------------------------------------------------------------
# Func: randstr()
# Desc: Generate string of random characters
#
# Args: $len - Length of string to generate.
#
# Retn: $str - Random string.
#------------------------------------------------------------------------------

sub randstr
{
	my ($len) = @_;

	my @chars = ('a'..'z','A'..'Z','0'..'9');

	my $str;
	$str .= $chars[rand @chars] foreach (1 .. $len);

	return $str;
}


#------------------------------------------------------------------------------
# Func: arrexists()
# Desc: 'exists' for array values.
#
# Args: \@array - Array to search.
#       $value  - Value to check for.
#
# Retn: $exists - Does the value exist?
#------------------------------------------------------------------------------

sub arrexists
{
	my ($array, $value) = @_;

	foreach (@{$array}) {
		return 1 if ($_ eq $value);
	}

	return 0;
}


#------------------------------------------------------------------------------
# Func: wantport()
# Desc: Check the restriction lists are either empty or contain the specified
#       values.
#
# Args: $path       - Port path  (undef to skip)
#       $category   - Category   (undef to skip)
#       $maintainer - Maintainer (undef to skip)
#
# Retn: $result     - true = all values falls within constraints
#------------------------------------------------------------------------------

sub wantport
{
	my ($path, $category, $maintainer, $sites) = @_;

	my ($site, $needed, $matched);

	$needed = 0;
	$matched = 0;

	if ($want_regex{site} && defined $sites) {
		$needed++;

		$sites =~ $want_regex{site}
			and $matched++;

		return 0 if ($matched != $needed);
	}

	if ($want_regex{maintainer} && defined $maintainer) {
		$needed++;

		$maintainer =~ $want_regex{maintainer}
			and $matched++;

		return 0 if ($matched != $needed);
	}

	if ($want_regex{category} && defined $category) {
		$needed++;

		$category =~ $want_regex{category}
			and $matched++;

		return 0 if ($matched != $needed);
	}

	if ($want_regex{port} && defined $path) {
		$needed++;

		$path =~ $want_regex{port}
			and $matched++;

		return 0 if ($matched != $needed);
	}

	return ($matched == $needed);
}


#------------------------------------------------------------------------------
# Func: tobasepkgpath()
# Desc: Remove flavors and/or subpackages from fullpkgpath.
#
# Args: $fullpkgpath - Package path for a specific port variation.
#
# Retn: $basepkgpath - Actual path to the package.
#------------------------------------------------------------------------------

sub tobasepkgpath
{
	my $fullpkgpath = shift;
	my $basepkgpath = $fullpkgpath;
	# Remove any flavors and/or subpackages.
	$basepkgpath =~ s/,.*//g;

	return $basepkgpath;
}


#------------------------------------------------------------------------------
# Func: fullpkgpathtoport()
# Desc: Extract the port name excluding version specific subdir package.
#
# Args: $fullpkgpath - Package path for a specific port variation.
#
# Retn: $port        - Plausible name of the port based on its path.
#------------------------------------------------------------------------------

sub fullpkgpathtoport
{
	my $fullpkgpath = shift;
	my $port = $fullpkgpath;

	# Remove any versions, categories, flavors and subpackages.
	$port =~ s/,.*//g;
	$port =~ s/\/[\d\.]+(\/|$)/$1/g;
	$port =~ s/.*\///g;

	# Remove -V marker, allow either nameD or name/V or long-name-V.
	# ex: devel/libsigc++-2 -> libsigc++
	# but: x11/kde-plasma/polkit-kde-agent-1 -> polkit-kde-agent-1
	$port =~ s/^([^\-]+)\-\d+$/$1/;

	return $port;
}


#------------------------------------------------------------------------------
# Func: fullpkgpathtoleaf()
# Desc: Extract the last folder name from path.
#
# Args: $fullpkgpath - Package path for a specific port variation.
#
# Retn: $port        - Plausible name of the port based on its path.
#------------------------------------------------------------------------------

sub fullpkgpathtoleaf
{
	my $fullpkgpath = shift;
	my $port = $fullpkgpath;

	# Remove any categories, flavors and subpackages.
	$port =~ s/,.*//g;
	$port =~ s/.*\///g;

	return $port;
}


#------------------------------------------------------------------------------
# Func: fullpkgpathtosubcat()
# Desc: Extract the port sub-category from cat/sub/cat/port.
#
# Args: $fullpkgpath - Package path for a specific port variation.
#
# Retn: $cat         - undef or sub-categories of the port.
#------------------------------------------------------------------------------

sub fullpkgpathtosubcat
{
	my $fullpkgpath = shift;
	my $cat = $fullpkgpath;

	# Remove any versions, top category, flavors and subpackages.
	$cat =~ s/,.*//g;
	$cat =~ s/\/[\d\.]+(\/|$)/$1/g;
	$cat =~ s/^[^\/]*\///g;

	# There is no sub-category to extract, return undef
	return if ($cat !~ /\//);

	$cat =~ s/\/[^\/]*$//g;

	return split /\//, $cat;
}


#------------------------------------------------------------------------------
# Func: primarycategory()
# Desc: Find the longest category (most precise one) that match package path.
#
# Args: $categories - List of category separated by space.
#       $path       - Package path in the port tree.
#
# Retn: $main_cat   - undef or the category path.
#------------------------------------------------------------------------------

sub primarycategory
{
	my ($categories, $path) = @_;
	my $main_cat;
	foreach my $category (split(/ /, $categories)) {
		$main_cat = $category if ($path =~ /^\Q$category\E/ && (
		    !$main_cat || length $main_cat < length $category));
	}
	return $main_cat;
}


#------------------------------------------------------------------------------
# Func: useragent()
# Desc: Return LWP::UserAgent with default settings.
#
# Args: n/a
#
# Retn: $ua - LWP::UserAgent.
#------------------------------------------------------------------------------

sub lwp_useragent
{
	my $ua = LWP::UserAgent->new;
	$ua->agent(USER_AGENT);
	$ua->timeout($settings{http_timeout});
	$ua->ssl_opts(verify_hostname => 0, SSL_verify_mode => 0x00);
	return $ua;
}


#------------------------------------------------------------------------------
# Func: uri_filename()
# Desc: Given a URI object, set or return the filename component. We define
#       the filename to be everything after the last slash.
#
# Args: $uri      - URI object.
#       $filename - New filename (optional).
#
# Retn: $filename - Filename component.
#------------------------------------------------------------------------------

sub uri_filename
{
	my $uri = shift;
	my @segs = $uri->path_segments;
	my $curr = $segs[$#segs];

	if (scalar @_) {
		splice(@segs, -1, 1);
		$uri->path_segments(@segs, $_[0] || '');
	}

	return $curr;
}


#------------------------------------------------------------------------------
# Func: uri_lastdir()
# Desc: Given a URI object, set or return the last directory. We define this
#       to be the everything after the last slash, unless the slash is the
#       last character, in which case, return the previous component.
#
# Args: $uri     - URI object.
#       $lastdir - New directory (optional).
#
# Retn: $lastdir - Last directory component.
#------------------------------------------------------------------------------

sub uri_lastdir
{
	my $uri = shift;
	my @segs = $uri->path_segments;

	my $offs = $segs[$#segs] ? 0 : 1;
	my $curr = $segs[$#segs-$offs];

	if (scalar @_) {
		splice(@segs, -1-$offs, 1+$offs);
		if ($offs && $_[0]) {
			$uri->path_segments(@segs, $_[0], '');
		} else {
			$uri->path_segments(@segs, $_[0] || '');
		}
	}

	return $curr;
}


#------------------------------------------------------------------------------
# Func: restrict2regex()
# Desc: Convert a comma-separated list of values into a restriction regex for
#       use by wantport().
#
# Args: $csv - Comma-separated string; values may contain * and ? wildcards.
#
# Retn: $re  - Compiled regex.
#------------------------------------------------------------------------------

sub restrict2regex
{
	my ($csv) = @_;

	my @items = split /,/, $csv;

	foreach my $item (@items) {
		# Clean up
		$item =~ s/\s+$//;
		$item =~ s/^\s+//;
		$item = lc $item;

		# Quote literal stuff
		$item =~ s/([^*?%_]+)/\Q$1\E/g;

		# Transform wildcards to regex
		$item =~ s/\*+/.*/g;
		$item =~ s/\?/./g;

		# Transform SQL wildcards to regex
		$item =~ s/\%+/.*/g;
		$item =~ s/\_/./g;

	}

	if (scalar @items) {
		my $list = join '|', @items;
		return qr/^(?:$list)(\-.*)?$/i;
	} else {
		return undef;
	}
}


#------------------------------------------------------------------------------
# Func: getdbver()
# Desc: Return the current database schema version.
#
# Args: n/a
#
# Retn: $version - database version.
#------------------------------------------------------------------------------

sub getdbver
{
	my ($dbh, $sth, $ver);

	$dbh = connect_db();

	$sth = $dbh->prepare($Portroach::SQL::sql{portroach_version})
		or die DBI->errstr;
	$sth->execute;

	($ver) = $sth->fetchrow_array;

	$sth->finish;

	return $ver;
}


#------------------------------------------------------------------------------
# Func: getstat()
# Desc: Retrieve a value from the "stats" table.
#
# Args: $key  - Statistic name.
#       $type - Datum type (default: TYPE_STRING).
#
# Retn: $val  - Value from database.
#------------------------------------------------------------------------------

sub getstat
{
	my ($key, $type) = @_;

	my ($dbh, $sth, $val);

	$dbh = connect_db();

	$sth = $dbh->prepare($Portroach::SQL::sql{portroach_getstat})
		or die DBI->errstr;
	$sth->execute($key);

	($val) = $sth->fetchrow_array;

	$sth->finish;

	if ($type == TYPE_INT || $type == TYPE_BOOL) {
		$val = 0 + $val;
	}

	return $val;
}


#------------------------------------------------------------------------------
# Func: setstat()
# Desc: Set a value in the "stats" table.
#
# Args: $key - Statistic name.
#       $val - New value.
#
# Retn: n/a
#------------------------------------------------------------------------------

sub setstat
{
	my ($key, $val) = @_;

	my ($dbh, $sth);

	return if $settings{precious_data};

	$val = '' if !defined $val;

	$dbh = connect_db();

	$sth = $dbh->prepare($Portroach::SQL::sql{portroach_setstat})
		or die DBI->errstr;
	$sth->execute($val, $key);

	$sth->finish;

	return;
}


#------------------------------------------------------------------------------
# Func: prepare_sql()
# Desc: Prepare the named SQL statements.
#
# Args: $dbh     - Database handle, already connected.
#       \%sths   - Somewhere to put prepared statement handles
#       @queries - Names of queries to prepare -- from %sql hash.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub prepare_sql
{
	my ($dbh, $sths, @queries) = @_;

	foreach (@queries) {
		if (exists $Portroach::SQL::sql{$_}) {
			$$sths{$_} = $dbh->prepare($Portroach::SQL::sql{$_})
			    or die DBI->errstr . "; statement \"$_\""
			    . "; query $Portroach::SQL::sql{$_}";
		} else {
			die "Attempted to prepare non-existent SQL query ($_).\n";
		}
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: finish_sql()
# Desc: Finish specified SQL statements.
#
# Args: \$dbh    - Database handle, already connected.
#       \%sths   - The hash of prepared statement handles.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub finish_sql
{
	my ($dbh, $sths) = @_;

	$$sths{$_}->finish
		foreach (keys %$sths);

	return 1;
}


#------------------------------------------------------------------------------
# Func: connect_db()
# Desc: Connect to database.
#
# Args: $nocache - If set, force new connection.
#
# Retn: $dbh     - Database handle.
#------------------------------------------------------------------------------

my $g_dbh;

sub connect_db
{
	my ($nocache) = @_;

	my ($dbh);

	if ($nocache) {
		$dbh = DBI->connect(
			$settings{db_connstr},
			$settings{db_user},
			$settings{db_pass}
		) or die DBI->errstr;
		Portroach::SQL->RegisterHacks($dbh);
	} else {
		$dbh = DBI->connect_cached(
			$settings{db_connstr},
			$settings{db_user},
			$settings{db_pass}
		) or die DBI->errstr;
		Portroach::SQL->RegisterHacks($dbh)
		    unless ($g_dbh);
		$g_dbh = $dbh; # Keep handle alive
	}

	return $dbh;
}


1;
