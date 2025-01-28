#------------------------------------------------------------------------------
# Copyright (C) 2010, Shaun Amott <shaun@inerd.com>
# Copyright (C) 2015, Jasper Lievisse Adriaanse <j@jasper.la>
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

package Portroach::DataSrc::Ports;

use base qw(Portroach::DataSrc);

use File::stat;

use List::MoreUtils qw(uniq);

use URI;
use URI::Escape;

use Try::Tiny;

use Portroach::Const;
use Portroach::Config;
use Portroach::API;
use Portroach::Util;

use strict;

require 5.006;


#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

our %settings;


#------------------------------------------------------------------------------
# Func: new()
# Desc: Constructor.
#
# Args: n/a
#
# Retn: $self
#------------------------------------------------------------------------------

sub new
{
	my $class = shift;

	my $self = {};

	bless ($self, $class);

	return $self;
}


#------------------------------------------------------------------------------
# Func: Build()
# Desc: Perform a full database build.
#
# Args: n/a
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub Build
{
    my $self = shift;
    my $sdbh = shift;

    return $self->BuildDB($sdbh);
}

#------------------------------------------------------------------------------
# Func: BuildDB()
# Desc: Build database.
#
# Retn: $success     - true/false
#------------------------------------------------------------------------------

sub BuildDB
{
	my $self = shift;

	my ($sdbh) = @_;

	my (%ssths, %sths, $dbh, $ps, $num_ports, $buildtime);

	$dbh = connect_db();
	$ps = Portroach::API->new;
	$num_ports = 0;
	$buildtime = time;

	# first need to create view before any other prepare statement
	prepare_sql($sdbh, \%ssths, qw(create_view));
	$ssths{create_view}->execute or die DBI->errstr;

	# Query SQLports for all the information we need. We don't care about
	# restrictions for now as this step basically copies sqlports. Check()
	# will handle any restrictions instead.
	prepare_sql($sdbh, \%ssths, qw(sqlports_count_ports));
	$ssths{sqlports_count_ports}->execute or die DBI->errstr;
	$num_ports = $ssths{sqlports_count_ports}->fetchrow_array();

	print "\n" unless ($num_ports < 1 or !$settings{verbose});

	if ($num_ports >= 1) {
		print "Building...\n";
		setstat('buildtime', $buildtime)
		    if (BuildPort($ps, $sdbh));
	} else {
		print "None found!\n";
	}

	finish_sql(\$dbh, \%sths);

	return 1;
}

sub BuildPortRegex
{
	my ($port, $sites) = @_;
	my ($basename, $pathname, $catname, $basename_q, $pathname_q, @name_q,
	    $dist_q);

	# Prepare name queries to guess version later on
	$basename = $port->{fullpkgname};
	$basename =~ s/(\-\D[^\-]*)+$//g;
	$basename =~ s/^(.*)-([^-]*)$/$1/g;
	$pathname = fullpkgpathtoport($port->{fullpkgpath});
	$catname = fullpkgpathtosubcat($port->{fullpkgpath});
	$basename_q = nametoregex($basename);
	debug(__PACKAGE__, $port, "basename regex, "
	    . "$basename -> $basename_q");
	push(@name_q, $basename_q);
	if ($basename ne $pathname) {
		$pathname_q = nametoregex($pathname);
		debug(__PACKAGE__, $port, "pathname regex, "
		    . "$pathname -> $pathname_q");
		push(@name_q, $pathname_q);
	}

	# Add extra guess based on fullpkgpath
	if ($catname) {
		my $q = nametoregex($catname);
		debug(__PACKAGE__,$port,"catname regex, $catname -> $q");
		push(@name_q, $q);
		if ($catname =~ /^lib/) {
			$q = $catname;
			$q =~ s/^lib//;
			$q = nametoregex($q);
			debug(__PACKAGE__, $port, "(lib)cat regex, "
			    . "$catname -> $q");
			push(@name_q, $q);
		}
	}

	# Add extra guesses based on SITES/HOMEPAGE
	foreach my $site (@$sites, $port->{homepage}) {
		if ($site =~ m:/([^/]+)\.git/:) {
			push(@name_q, nametoregex($1));
		}
		my $name = Portroach::SiteHandler->FindName($site);
		next unless ($name);
		$name = nametoregex($name);
		push(@name_q, $name);
	}

	# Add extra common pattern guess based basename (pkgname)
	if ($basename =~ /^lib/) {
		my $q = $basename;
		$q =~ s/^lib//;
		$q = nametoregex($q);
		debug(__PACKAGE__, $port, "(lib)name regex, $basename -> $q");
		push(@name_q, $q);
	}
	if ($basename =~ /^\d{2,}\D/) {
		my $q = $basename;
		$q =~ s/^\d{2,}//;
		$q = nametoregex($q);
		debug(__PACKAGE__, $port, "name\$ regex, $basename -> $q");
		push(@name_q, $q);
		$q = $basename;
		$q =~ s/^(\d{2,}).*?$/$1/;
		$q = nametoregex($q);
		debug(__PACKAGE__, $port, "^digit regex, $basename -> $q");
		push(@name_q, $q);
	} elsif ($basename =~ /\D\d{2,}$/) {
		my $q = $basename;
		$q =~ s/\d{2,}$//;
		$q = nametoregex($q);
		debug(__PACKAGE__, $port, "^name regex, $basename -> $q");
		push(@name_q, $q);
		$q = $basename;
		$q =~ s/^.*?(\d{2,})$/$1/;
		$q = nametoregex($q);
		debug(__PACKAGE__, $port, "digit\$ regex, $basename -> $q");
		push(@name_q, $q);
	} elsif ($basename =~ /\D\d$/) {
		my $q = $basename;
		$q =~ s/\d$//;
		$q = nametoregex($q);
		debug(__PACKAGE__, $port, "^name regex, $basename -> $q");
		push(@name_q, $q);
	}

	# Finally compute the regex
	foreach my $q (@name_q) {
		next unless ($q =~ /\|/);
		my @qs = split (/\|/, $q);
		for my $i (1 .. $#qs) {
			push(@name_q, $qs[$i]);
		}
	}
	@name_q = sort { length $b <=> length $a } uniq @name_q;
	foreach my $q (@name_q) {
		$dist_q .= '|' if ($dist_q);
		$dist_q .= $q;
	}
	return $dist_q;
}

# Queries SQLports for:
sub BuildPort
{
	my ($ps, $sdbh) = @_;
	my ($port, $q, $tot, $n, $rej, $meta, $dup, $bump, $up, $new);
	$n = $rej = $meta = $dup = $bump = $up = $new = 0;

	my $sths = {};
	prepare_sql($sdbh, $sths, qw(
	    ports_select ports_select_count
	    ports_restrict_maintainer ports_restrict_maintainer_count
	    ports_restrict_category ports_restrict_category_count
	    ports_restrict_port ports_restrict_port_count));

	# Patterns to remove common suffix/prefix
	my $chop_regex = $settings{build_chop};
	my $chop_sufxq = qr/[\.\-\_\+]?(?:$chop_regex|$lang_regex)[\.\-\_\+]?/;
	my $chop_prfxq = qr/[\.\-\_\+]?
	    (?:$chop_regex|$lang_regex|$beta_regex)[\.\-\_\+]?/x;

	# Apply any needed restrictions.
	my $rc = 0; # report success only if not restricted

	if ($settings{restrict_maintainer}) {
		my $limit = "$settings{restrict_maintainer}%";

		$sths->{ports_restrict_maintainer}->execute($limit)
		    or die DBI->errstr;
		$sths->{ports_restrict_maintainer_count}->execute($limit)
		    or die DBI->errstr;

		$tot = $sths->{ports_restrict_maintainer_count}->fetchrow_array;
		$q = $sths->{ports_restrict_maintainer};

	} elsif ($settings{restrict_category}) {
		my $limit = "$settings{restrict_category}";

		$sths->{ports_restrict_category}->execute($limit)
		    or die DBI->errstr;
		$sths->{ports_restrict_category_count}->execute($limit)
		    or die DBI->errstr;

		$tot = $sths->{ports_restrict_category_count}->fetchrow_array;
		$q = $sths->{ports_restrict_category};

	} elsif ($settings{restrict_port}) {
		my $limit = "%$settings{restrict_port}%";

		$sths->{ports_restrict_port}->execute($limit)
		    or die DBI->errstr;
		$sths->{ports_restrict_port_count}->execute($limit)
		    or die DBI->errstr;

		$tot = $sths->{ports_restrict_port_count}->fetchrow_array;
		$q = $sths->{ports_restrict_port};

	} else {
		$sths->{ports_select}->execute()
		    or die DBI->errstr;
		$sths->{ports_select_count}->execute()
		    or die DBI->errstr;

		$tot = $sths->{ports_select_count}->fetchrow_array;
		$q = $sths->{ports_select};
		$rc = 1;
	}

	if ($tot < 1) {
		print STDERR "Nothing to build, "
		    . "invalid restrict_* settings ?\n";
		return 0;
	}

	while($port = $q->fetchrow_hashref()) {
		my (%pcfg, @sites, $category, $distfile, $dist, $path, $url,
		    $sufx, $ver, $versrc, $basepkgpath, $dist_q, $pkgver,
		    $verbose);
		$n++;

		$basepkgpath = tobasepkgpath($port->{fullpkgpath});
		$category = primarycategory($port->{categories}, $basepkgpath);

		unless ($category) {
			print STDERR "$port->{fullpkgpath}: no cat matching, "
			    . "$port->{categories} !~ $basepkgpath\n";
			$category = (split(/ /, $port->{categories}))[0];
		}

		if ($category eq 'meta') {
			info(1, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) SKIP, meta package");
			$meta++;
			next;
		}

		# Bail out early if the port has no distfile to begin with
		if (!$port->{distfiles}) {
			info(0, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) SKIP, no distfile");
			$rej++;
			next;
		}

		# ports homepage should start with gopher:// or https?://
		if ($port->{homepage} && $port->{homepage} !~
		    /^https?:\/\/|gopher:\/\//) {
			my $homepage = 'http://'.$port->{homepage};
			print STDERR "$port->{fullpkgpath}: FIX, homepage url "
			    . $port->{homepage} . " -> $homepage\n";
			$port->{homepage} = $homepage;
		}

		# sanitize portroach config
		foreach my $cfg (split /\s+/, $port->{portroach}) {
			if ($cfg =~ /^([A-Za-z]+):(.*)$/i) {
				$pcfg{lc $1} = $2;
			} else {
				print STDERR "$port->{fullpkgpath}: "
				    . "invalid portroach config '$cfg'\n";
			}
		}

		# ROACH_URL is UNIQUE ! one distfile only :)
		$distfile = $port->{distfiles};
		# XXX site group spec. ?
		$distfile =~ s/:[A-Za-z0-9][A-Za-z0-9\,]*$//g;

		# detect url file?k=v;... and remove anything after '?'
		if ($distfile =~ /\?/) {
			$url = $distfile;
			$distfile =~ s/\?.*$//;
			info(1, $port->{fullpkgpath},
			    "url detected $url -> $distfile");
		}

		# detect path in distfile, move it into SITES
		if ($distfile =~ /^(.*)\/(.*?)$/) {
			debug(__PACKAGE__, $port, "path detected, "
			    . "split $distfile -> $1 / $2");
			$path = $1 . '/';
			$distfile = $2;
		}

		# ports should not use encoded url
		if ((my $file = uri_unescape($distfile)) ne $distfile) {
			print STDERR "$port->{fullpkgpath}: FIX, encoded url "
			    . "$distfile -> $file\n";
			$distfile = $file;
		}

		# get file extention suffix
		info(1, $port->{fullpkgpath}, "SUFX? $distfile")
		    unless ($sufx = extractsuffix($distfile));
		$dist = $distfile;
		debug(__PACKAGE__, $port, "trim .ext -> $dist")
		    if ($dist =~ s/(\.($ext_regex))+$//);

		# not much todo without any version, adjust verbosity level
		$verbose = 1 if ($dist !~ /$verlike_regex$/i);
		debug(__PACKAGE__, $port, "verbose($verbose) if "
		    . "$dist !~ $verlike_regex\$");

		foreach my $site (split /\s+/, $port->{master_sites}) {
			my ($canon, $abs_path);

			# Sanitize site
			# XXX site group spec. ?
			$site =~ s/:[A-Za-z0-9][A-Za-z0-9\,]*$//g;
			$site =~ s/^\s+//;
			$site =~ s/\/+$/\//;
			if (length($site) == 0) {
				print STDERR "$port->{fullpkgpath}: "
				    . "no master sites\n";
				next;
			}
			unless ($site =~ /\/$/) {
				print STDERR "$port->{fullpkgpath}: "
				    . "FIX, missing last / in $site\n";
				$site .= '/';
			}

			# path detected in distfile, move it into SITES
			$site .= $path if ($path);

			try {
				# canonical site
				$canon = URI->new($site)->canonical;
				if (length $canon->host == 0) {
					print STDERR "$port->{fullpkgpath}: "
					    . "empty host $canon\n";
					$canon = undef;
					next;
				}
			} catch {
				$canon = undef;
				print STDERR "$port->{fullpkgpath}: "
				    . "caught error on $site\n";
				debug(__PACKAGE__, $port, "$_");
			};
			next unless($canon);

			# Check if path is absolute (../ ./ //)
			# XXX verbose, looks cosmetic
			$abs_path = path_absolute($canon->path);
			if ($canon->path && $canon->path ne $abs_path) {
				info(1, $port->{fullpkgpath},
				    "FIX, path ".$canon->path." -> $abs_path");
				$canon->path($abs_path);
			# check if site is cannonical
			} elsif ($canon ne $site) {
				info(1, $port->{fullpkgpath},
				    "FIX, site $site -> $canon");
			}
			$site = $canon;

			# mastersite to ignore ?
			my $sre = Portroach::Util::restrict2regex(
			    $settings{mastersite_ignore});
			if ($sre && $site =~ /$sre/) {
				debug(__PACKAGE__, $port, "mastersite "
				    . "ignore $site =~ $sre");
				next;
			}

			# Extract from site handler, override dist
			if ($ver = Portroach::SiteHandler->FindVersion($site)) {
				$dist = $ver;
				debug(__PACKAGE__, $port, "$site -> $dist");
			}

			push(@sites, $site);
		}

		# Ports without sites ends as not found, adjust verbosity level
		# XXX maybe drop this later, have to focus on living ports first
		if (!@sites) {
			info(1, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) no mastersite");
			$verbose = 1;
		}

		debug(__PACKAGE__, $port, "pkg $port->{fullpkgname}: "
		    . "distfile $distfile, distname $port->{distname}");
		
		# Shortcut, check pkg version against distfile
		$pkgver = $port->{fullpkgname};
		$pkgver = lc $pkgver;
		debug(__PACKAGE__, $port, "flavor -> $pkgver")
		    if ($pkgver =~ s/(\-\D[^\-]*)+$//g);
		$pkgver =~ s/^(.*)-([^-]*)$/$2/g;
		debug(__PACKAGE__, $port, "rev -> $pkgver")
		    if ($pkgver =~ s/([pv]\d+)+$//g);
		debug(__PACKAGE__, $port, "beta -> $pkgver")
		    if ($pkgver =~ s/($beta_regex)$//);
		$pkgver = vertoregex($pkgver);# lazy version separator X.Y ~ XY
		debug(__PACKAGE__, $port, "pkgver, $pkgver");

		# Compute regex to extract version (as a fallback)
		$dist_q = BuildPortRegex($port,\@sites);
		debug(__PACKAGE__, $port, "regex, $dist_q");

		# Stop on the first version match, keep track of source string
		foreach my $verdist ($dist, @sites, $port->{distname}) {
			if ($verdist !~ /\d/) {
				debug(__PACKAGE__, $port, "skip, "
				    . "no digit in $verdist");
				next;
			}

			debug(__PACKAGE__, $port, "extract ver from $verdist");
			$ver = $versrc = $verdist;

			# Alway start with lower case to ease further processing
			$ver = lc $ver;

			# Version check againt SITE, handle only one /Version/
			if ($ver =~ s:^.*/($verlike_regex)/.*$:$1:) {
				debug(__PACKAGE__,$port,"path version -> $ver");
			} elsif (index($ver,"/") != -1) {
				# Still a path, no version found, next
				debug(__PACKAGE__, $port,
				    "invalid path, skip $ver");
				$ver = undef;
				next;
			} else {
				# Extract base on pkgver
				debug(__PACKAGE__, $port,
				    "pkgver ~ $pkgver -> $ver")
				    if ($ver =~ s/^\D*?($chop_prfxq|$dist_q)?
				    \D*?($pkgver.*)$/$2/x);

				debug(__PACKAGE__, $port, "chop\$ -> $ver")
				    if ($ver =~ s/($chop_sufxq|[\.\-\_]?
				        ($dist_q))+$//xg);
			}

			# shortcut, most common cases
			if (isversion($ver)) {
				# Normalize \d [-_] \d into \d.\d
				debug(__PACKAGE__, $port, "normalize -> $ver")
				    if ($ver =~ s/(?<=\d)[\-\_](?=\d)/\./g);
				last;
			}

			# Remove names from pkgname/fullpkgpath, prefix & suffix
			# Note, allow some extra garbage \d(?!\.) | \++
			debug(__PACKAGE__,$port,"~$dist_q~ SEP -> $ver")
			    if ($ver =~ s/^(\D*?($chop_prfxq|$dist_q)
			    (?:\d(?!\.)|\++)?((?<!\d)\.|\-|\_))+//x);
			debug(__PACKAGE__,$port,"SEP ~$dist_q~ -> $ver")
			    if ($ver =~ s/([\.\-\_]($dist_q|$chop_sufxq)
			    (?:\d(?!\.)|\++)?\D*?)+$//x);

			# Remove prefix with no separator
			debug(__PACKAGE__, $port, "$dist_q v.like -> $ver")
			    if ($ver =~ s/^\D*?(?:$dist_q)(?:$chop_prfxq)?
			    (?:$verprfx_regex?)?($verlike_regex)$/$1/x);

			# Chop prefix after name's query
			debug(__PACKAGE__, $port, "^chop -> $ver")
			    if ($ver =~ s/^(\D*?$chop_prfxq)+//g);

			# Remove common prefix version marker
			debug(__PACKAGE__, $port, "trim (v|r) -> $ver")
			    if ($ver =~ s/^$verprfx_regex//);

			# Normalize \d [-_] \d into \d.\d
			debug(__PACKAGE__, $port, "normalize -> $ver")
			    if ($ver =~ s/(?<=\d)[\-\_](?=\d)/\./g);

			# Check commit id string to reduce verbosity on those
			# XXX WIP, tmp verbose adjustment to ease dev/debug
			if ($ver =~ /^[0-9a-f]{10,40}$/) {
				$verbose = 1;
				debug(__PACKAGE__, $port, 
				    "discard commitid $ver");
				$ver = undef;
				next;
			}

			# Finally, check we got something plausible
			unless (isversion($ver)) {
				debug(__PACKAGE__, $port, 
				    "discard invalid version $ver");
				$ver = undef;
				next;
			}

			last if ($ver);
		}

		unless ($ver) {
			$ver = $versrc = $port->{fullpkgname};
			$ver = lc $ver;
			$ver =~ s/(\-\D[^\-]*)+$//g;
			$ver =~ s/^(.*)-([^-]*)$/$2/g;
			$ver =~ s/([pv]\d+)+$//g;
			info($verbose, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) FULLPKGNAME, $versrc");
		} elsif ($versrc =~ /\//) {
			info($verbose, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) URL, $versrc");
		} elsif ($versrc ne $dist) {
			info($verbose, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) DISTNAME, $versrc");
		} else {
			# No report if nothing changed
			$verbose = 1;
		}

		debug(__PACKAGE__, $port, "$versrc -> $ver");

		my $rc = $ps->AddPort({
		    'name'        => $port->{fullpkgname},
		    'cat'         => $category,
		    'ver'         => $ver,
		    'maintainer'  => $port->{maintainer},
		    'comment'     => $port->{comment},
		    'distname'    => $port->{distname},
		    'sufx'        => $sufx,
		    'distfile'    => $url ? $url : $distfile,
		    'sites'       => \@sites,
		    'options'     => \%pcfg,
		    'pcfg_comment'=> $port->{portroach_comment},
		    'homepage'    => $port->{homepage},
		    'basepkgpath' => $basepkgpath,
		    'fullpkgpath' => $port->{fullpkgpath},
		});
		if (!$rc) {
			$rej++;
			info(0, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) REJ, $ver");
		} elsif ($rc == 4) {
			# XXX handle DUP earlier, before AddPort()
			$dup++;
			info(1, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) DUP, $ver");
		} elsif ($rc == 3) {
			$bump++;
			info($verbose, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) BUMP, $ver");
		} elsif ($rc == 2) {
			$up++;
			info($verbose, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) UP, $ver");
		} else { # $rc == 1
			$new++;
			info(0, $port->{fullpkgpath},
			    "(".strchop($n,5)."/$tot) NEW, $ver");
		}
	}
	print "($tot) Build done: new $new, up. $up, "
	    . "bump(ver) $bump / rej. $rej, meta $meta, dup. $dup\n";
	return $rc;
}

1;
