#------------------------------------------------------------------------------
# Copyright (C) 2011, Shaun Amott <shaun@inerd.com>
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
use URI::Escape;

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
	&vertoregex
	&verguess
	&vercompare
	&betacompare
	&checkevenodd
	&extractfilenames
	&extractdirectories
	&extractsubdirectories
	&extractsuffix
	&tobasepkgpath
	&fullpkgpathtoport
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
	maintainer => restrict2regex($settings{restrict_maintainer})
);

# XXX vercompare handle long months but not date_regex
@months = (
	qr/Jan(?:uary)?/,
	qr/Feb(?:ruary)?/,
	qr/Mar(?:ch)?/,
	qr/Apr(?:il)?/,
	qr/May/,
	qr/Jun(?:e)?/,
	qr/Jul(?:y)?/,
	qr/Aug(?:ust)?/,
	qr/Sep(?:tember)?/,
	qr/Oct(?:ober)?/,
	qr/Nov(?:ember)?/,
	qr/Dec(?:ember)?/
);

$month_regex = qr/jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec/;
$date_regex  = qr/(?<!\d)\d{2,4}([\.\-\_]?)
    (?:\d{2}|$month_regex)\1\d{2,4}(?!\d)/x;

%beta_types = (
	snapshot   => { re => 'svn|cvs|snap(?:shot)?|nightly',	rank => 1 },
	unstable   => { re => 'unstable|dev|test',		rank => 2 },
	alpha      => { re => 'alpha|a(?=\d+|$)',		rank => 3 },
	beta       => { re => 'beta|b(?=\d+|$)',		rank => 4 },
	prerelease => { re => 'pr.*?|p(?=\d+|$)',		rank => 5 },
	relcand    => { re => 'rc|r(?=\d+|$)',			rank => 6 }
);

$beta_regex = join '|', map +($beta_types{$_}->{re}), keys %beta_types;

$ext_regex = qr/(?:l|t?b|t?g|t?x)?z(?:2|st)?|
    langpack|xpi|pl|uu|txt|bin|c|dictd|dtd|exe|F90|gem|html|ins|jar|kar|mp3|
    mmdb|otf|pdf|phar|rar|rpm|run|sfc|shar|spl|tar|tgz|ttf|txi|txt|uqm|war|
    zip/xi;

$verprfx_regex = qr/(?:v|ver|r|rel|release)[\.\-\_]?(?=\d)/;

$verlike_regex = qr/
    \d+[\.\-\_]\d+[^\/]*?
    |$date_regex
    |\d{2,}([a-z]{,2}\d{,2})?/x;

$lang_regex = qr/(?:hs|lua|mod|node|p5|perl|py\d?|ruby)/;

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

	return lc $name if ($name =~ /^$lang_regex$/i);

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

	return $regex unless ($name =~ /\-|\+|\d|[A-Z]/);

	my @names;
	@names = (@names, split(/\-/, $name))
		if (!@names && $name =~ /\-/);
	@names = (@names, split(/\+/, $name))
		if (!@names && $name =~ /\+/);
	@names = (@names, split(/\d+/, $name))
		if (!@names && $name =~ /\d+/);
	@names = (@names, split(/(?=[A-Z])/, $name))
		if (!@names && $name =~ /[A-Z]+/);
	return $regex unless (scalar @names > 1);

	foreach my $subname (@names) {
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
		next if ($subname =~ /^$lang_regex$/);
		next if (length $subname == 1);

		my $q = nametoregex($subname);
		$regex .= "|$q";
	}

	return lc $regex;
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
# Args: $new    - New version string
#       $old    - Old version string
#
# Retn: $result - Is $new greater than $old? Returns -1 for "Maybe"
#------------------------------------------------------------------------------

sub vercompare
{
	my ($new, $old) = @_;

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
			# $new and $old equal except for beta bit
			# Therefore, $old (a final release) is newer
			return 0 if ($_new eq $old);

			$newbeta = 1;
		}

		if (chopbeta(\$_old)) {
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
		} else {
			# Remove beta bits, as non-beta bits
			# differ and can be compared.
			$new = $_new;
			$old = $_old;
		}
	}

	# If both version strings contain a date AND other
	# numbers, take care to split them and compare
	# individually.

	unless ($new =~ /^$date_regex$/i && $old =~ /^$date_regex$/i)
	{
		my $date_regex = $date_regex;
		$date_regex =~ s/\\1/\\3/g; # Bump internal backreference (evil)

		# XXX -> [\.\-\_]
		if ($new =~ /^(.*?)[\-\.]?($date_regex)[\-\.]?(.*)$/) {
			my ($new_1, $new_2, $new_3) = ($1, $2, $4);

			# XXX -> [\.\-\_]
			if ($old =~ /^(.*?)[\-\.]?($date_regex)[\-\.]?(.*)$/) {
				my ($old_1, $old_2, $old_3) = ($1, $2, $4);

				if ($new_1 and $old_1) {
					return vercompare($new_1, $old_1) unless ($new_1 eq $old_1);
				}

				if ($new_2 and $old_2) {
					return vercompare($new_2, $old_2) unless ($new_2 eq $old_2);
				}

				if ($new_3 and $old_3) {
					return vercompare($new_3, $old_3) unless ($new_3 eq $old_3);
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
			$new =~ s/$m/sprintf "%02d", $i/gie;
			$i++;
		}
	}

	if ($old =~ /$month_regex/) {
		my $i = 1;
		foreach my $m (@months) {
			$old =~ s/$m/sprintf "%02d", $i/gie;
			$i++;
		}
	}

	my @nums_new = split /\D+/, $new;
	my @nums_old = split /\D+/, $old;

	foreach my $n (0 .. $#nums_new) {
		# All preceding components are equal, so assume newer.
		return 1 if (!defined($nums_old[$n]));

		# Attempt to handle cases where version component lengths vary.
		if ($n == $#nums_new &&
		    length $nums_new[$n] != length $nums_old[$n])
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
				if ($first_new > $first_old) {
					return -1;
				} elsif ($first_new == $first_old) {
					$nums_old[$n] .= ('0' x $lendiff);
					return ($nums_new[$n] > $nums_old[$n]
					    ? -1 : 0);
				} else {
					return 0;
				}
			} elsif ($lendiff <= -$lendiff_thresh) {
				if ($first_new < $first_old) {
					return 0;
				} elsif ($first_new == $first_old) {
					$nums_new[$n] .= ('0' x abs $lendiff);
					return ($nums_new[$n] < $nums_old[$n]
					    ? 0 : -1);
				} else {
					return -1;
				}
			}
		}

		# Otherwise, compare values numerically
		return 1 if (0+$nums_new[$n] > 0+$nums_old[$n]);
		return 0 if (0+$nums_new[$n] < 0+$nums_old[$n]);
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

	my $newrank = 0;
	my $oldrank = 0;
	my $newnums = 0;
	my $oldnums = 0;

	foreach my $bt (keys %beta_types) {
		my $re   = $beta_types{$bt}->{re};
		my $rank = $beta_types{$bt}->{rank};

		if ($new =~ /[\.\-\_](?:$re)(\d*(?:\.\d+)*)/ ||
		    $new =~ /(?<=\d)(?:$re)(\d*(?:\.\d+)*)/) {
			$newrank = $rank;
			$newnums = $1 if $1;
		}

		if ($old =~ /[\.\-\_](?:$re)(\d*(?:\.\d+)*)/ ||
		    $old =~ /(?<=\d)(?:$re)(\d*(?:\.\d+)*)/) {
			$oldrank = $rank;
			$oldnums = $1 if $1;
		}
	}

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
		while (/^a\s+href\s*=\s*(['"])([^<>\s]*$sufx?)\1/gi) {
			my $file = uri_unescape($2);
			debug(__PACKAGE__, undef, "unescape $2 -> $file")
			    if ($file ne $2);
			push @$files, $file;
		}
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: extractdirectories()
# Desc: Extract directories from a mastersite index
#
# Args: $data    - Data from master site request.
#       \$dirs   - Where to put directories found.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub extractdirectories
{
	my ($data, $dirs) = @_;

	foreach (split "\n", $data) {
		while (/<a href=(['"])(.*?)\/\1.*?>\2(?:\/<\/a>|<\/a>\/)(?:.*?)/gi) {
			push @$dirs, $2;
		}
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: extractsubdirectories()
# Desc: Extract directories from a mastersite index, going down the path only.
#
# Args: $data    - Data from master site request.
#       \$dirs   - Where to put directories found, only full path, ^/
#       $site    - Actual, full path on the site to resolve/filter link.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub extractsubdirectories
{
	my ($data, $dirs, $site) = @_;

	my $host_q = quotemeta $site->host;
	my $path_q = quotemeta $site->path;

	foreach (split "<", $data) {
		if (/^a\s+href\s*=\s*('|")(.*\/)\1/gi) {
			my $guess = $2;
			# check same host, extract actual path
			# XXX check type also ?
			if ($guess =~ /^.*?:\/\//) {
				next if ($guess !~ /^.*?:\/\/$host_q\//);
				$guess =~ s/^.*?:\/\/$host_q\//\//;
			}
			$guess = $site->path.$guess if ($guess !~ /^\//);
			$guess = path_absolute($guess);
			next if ($guess !~ /^$path_q[^\/]+/);
			push @$dirs, $guess;
		}
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
		/+(?=/)
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

	# XXX should fit for non verbose msg ... or not
	# width is diveded by 2 for eah @str
	# use at most two: [---30---] [-15-] $msg
	my $width = 30;

	return if ($verbose && !$settings{verbose});

	$msg = pop (@items);

	foreach (@items) {
		$str .= ' ' if ($str);
		$str .= '[' . strchop($_, $width) . ']';
		$width /= 2;
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
# Args: $port       - Port name  (undef to skip)
#       $category   - Category   (undef to skip)
#       $maintainer - Maintainer (undef to skip)
#
# Retn: $result     - true = all values falls within constraints
#------------------------------------------------------------------------------

sub wantport
{
	my ($port, $category, $maintainer) = @_;

	my ($needed, $matched);

	$needed = 0;
	$matched = 0;

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

	if ($want_regex{port} && defined $port) {
		$needed++;

		if ($port =~ $want_regex{port}) {
			$matched++;
		} elsif (defined $category and
		    "$category/$port" =~ $want_regex{port}) {
			$matched++;
		}

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
# Func: fullpkgpathtosubcat()
# Desc: Extract the port sub-category from cat/subcat/port.
#
# Args: $fullpkgpath - Package path for a specific port variation.
#
# Retn: $cat         - undef or the sub-category of the port.
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

	$cat =~ s/\/.*$//g;

	return $cat;
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
		return qr/^(?:$list)$/i;
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
				or die DBI->errstr . "; statement \"$_\"";
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
