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

# Queries SQLports for:
sub BuildPort
{
    my ($ps, $sdbh) = @_;
    my (@ports, $q, $total_ports, $n_port, $rej, $meta, $dup, $bump, $up, $new);
    $n_port = $rej = $meta = $dup = $bump = $up = $new = 0;

    my $sths = {};
    prepare_sql($sdbh, $sths, qw(ports_select ports_select_count
                                 ports_restrict_maintainer ports_restrict_maintainer_count
                                 ports_restrict_category ports_restrict_category_count
				 ports_restrict_port  ports_restrict_port_count));

    # Apply any needed restrictions.
    my $rc = 1; # report success only if not restricted
    if ($settings{restrict_maintainer}) {
	my $limit = "$settings{restrict_maintainer}%";

	$sths->{ports_restrict_maintainer}->execute($limit) or die DBI->errstr;
	$sths->{ports_restrict_maintainer_count}->execute($limit) or die DBI->errstr;

	$total_ports = $sths->{ports_restrict_maintainer_count}->fetchrow_array();
	$q = $sths->{ports_restrict_maintainer};
	$rc = 0;
    } elsif ($settings{restrict_category}) {
	my $limit = "$settings{restrict_category}";

	$sths->{ports_restrict_category}->execute($limit) or die DBI->errstr;
	$sths->{ports_restrict_category_count}->execute($limit) or die DBI->errstr;

	$total_ports = $sths->{ports_restrict_category_count}->fetchrow_array();
	$q = $sths->{ports_restrict_category};
	$rc = 0;
    } elsif ($settings{restrict_port}) {
	my $limit = "%$settings{restrict_port}%";

	$sths->{ports_restrict_port}->execute($limit) or die DBI->errstr;
	$sths->{ports_restrict_port_count}->execute($limit) or die DBI->errstr;

	$total_ports = $sths->{ports_restrict_port_count}->fetchrow_array();
	$q = $sths->{ports_restrict_port};
	$rc = 0;
    } else {
	$sths->{ports_select}->execute() or die DBI->errstr;
	$sths->{ports_select_count}->execute() or die DBI->errstr;

	$total_ports = $sths->{ports_select_count}->fetchrow_array();
	$q = $sths->{ports_select};
    }

    if ($total_ports < 1) {
	print STDERR "Nothing to build, invalid restrict_* settings ?\n";
	return 0;
    }

    while(@ports = $q->fetchrow_array()) {
	my (%pcfg, @sites, $fullpkgpath, $pkgname, $name,
	    $category, $distname, $distfile, $path, $url, $maintainer,
	    $comment, $sufx, $ver, $versrc, $basepkgpath, $pcfg_comment,
	    $homepage, $port, $basename, $pathname, $basename_q, $pathname_q);
	$n_port++;

	$pkgname     = $ports[10];
	$fullpkgpath = $ports[0];
	$basepkgpath = tobasepkgpath($fullpkgpath);
	$category    = primarycategory($ports[1]);

	# Fake $port to ease debugging
	$port = {'fullpkgpath' => $fullpkgpath,};

	if ($category eq 'meta') {
		info(1, $fullpkgpath, "(".strchop($n_port,5)."/$total_ports) "
		    . "SKIP, meta package");
		$meta++;
		next;
	}

	# Bail out early if the port has no distfile to begin with
	if (!$ports[3]) {
		info(0, $fullpkgpath, "(".strchop($n_port,5)."/$total_ports) "
		    . "SKIP, no distfile");
		$rej++;
		next;
	}

	# Extract name from pkgname but check dirname in case it's more explicit
	$basename = $pkgname;
	$basename =~ s/^(.*)-([^-]*)$/$1/g;
	$pathname = fullpkgpathtoport($fullpkgpath);

	# Chose the most precise name to insert in database, mostly for lookup
	# XXX not that usefull afterall ?
	if (index($pathname, $basename) != -1) {
		debug(__PACKAGE__, $port, "prefer pkgpath, "
		    . "name -> $pathname");
		$name = $pathname;
	} else {
		debug(__PACKAGE__, $port, "prefer pkgname, "
		    . "name -> $basename");
		$name = $basename;
	}

	$distname = $ports[2];
	# get rid of version/epoch markers
	$distname =~ s/v[0-9]+$//;
	$distname =~ s/p[0-9]+$//;

	# ROACH_URL is UNIQUE ! one distfile only :)
	$distfile = $ports[3];
	# XXX site group spec. ?
	$distfile =~ s/:[A-Za-z0-9][A-Za-z0-9\,]*$//g;

	# detect path in distfile, move it into SITES
	if ($distfile =~ /^(.*)\/(.*?)$/) {
		debug(__PACKAGE__, $port, "path detected, "
		    . "split $distfile -> $1 / $2");
		$path = $1 . '/';
		$distfile = $2;
	}

	# ports should not use encoded url
	if ((my $file = uri_unescape($distfile)) ne $distfile) {
		print STDERR "$fullpkgpath: FIX encoded url, "
		    . "$distfile -> $file\n";
		$distfile = $file;
	}

	# detect url file?k=v;... and remove anything after '?'
	if ($distfile =~ /\?/) {
		$url = $distfile;
		$distfile =~ s/\?.*$//;
		info(1, $fullpkgpath,
		    "url detected $url -> $distfile");
	}

	$maintainer = $ports[5];
	$comment    = $ports[6];

	foreach my $cfg (split /\s+/, $ports[7]) {
		if ($cfg =~ /^([A-Za-z]+):(.*)$/i) {
			$pcfg{lc $1} = $2;
		} else {
			print STDERR "$fullpkgpath: invalid portroach '$cfg'\n";
		}
	}
	$pcfg_comment = $ports[8];
	$homepage = $ports[9];
	info(1, $fullpkgpath, "SUFX? $distfile")
	    unless ($sufx = extractsuffix($distfile));
	foreach my $site (split /\s+/, $ports[4]) {
		my $ignored = 0;

		# XXX site group spec. ?
		$site =~ s/:[A-Za-z0-9][A-Za-z0-9\,]*$//g;
		$site =~ s/^\s+//;
		$site =~ s/\/+$/\//;
		if (length($site) == 0) {
			print STDERR "$fullpkgpath: empty or no master sites\n";
			next;
		}
		try {
			# path detected in distfile, move it into SITES
			unless ($site =~ /\/$/) {
				print STDERR "$fullpkgpath: FIX site, "
				    . "missing last '/' in $site\n";
				$site .= '/';
			}
			$site .= $path if ($path);

			# cleanup site and print to STDERR, ports need fixing
			my $site_canonical = URI->new($site)->canonical;
			if (length $site_canonical->host == 0) {
				print STDERR "$fullpkgpath: empty host from "
				    . "$site_canonical\n";
				next;
			}
			$site_canonical =~ s/(?<!\:)\/+/\//g;
			if ($site_canonical ne $site) {
				# XXX too verbose ... those fix are cosmetic ?
				#print STDERR "$fullpkgpath: FIX site "
				#    . "$site -> $site_canonical\n";
				info(1, $fullpkgpath, "FIX site "
				    . "$site -> $site_canonical");
				$site = $site_canonical;
			}

			# mastersite to ignore ?
			my $mastersite_regex = Portroach::Util::restrict2regex(
			    $settings{mastersite_ignore});
			if ($mastersite_regex && $site =~ /$mastersite_regex/) {
				debug(__PACKAGE__, $port, "mastersite ignore "
				    . "$site =~ $mastersite_regex");
				next;
			}

			push(@sites, $site);
		} catch {
			print STDERR "$fullpkgpath: "
			    . "caught error on $site:\n";
			print STDERR "$fullpkgpath: $_";
		};
	}

	debug(__PACKAGE__, $port, "pkg $pkgname: ".
	    "distfile $distfile, distname $distname");

	my $lang_re = '(node|p5|mod|py|ruby|hs)';
	$basename_q = lc $basename;
	debug(__PACKAGE__, $port, "basename optional [-_.] "
	    . "$basename -> $basename_q")
	    if ($basename_q =~ s/[\-\_\.]/\.?/g);
	debug(__PACKAGE__, $port, "basename optional + "
	    . "$basename -> $basename_q")
	    if ($basename_q =~ s/\+/\\\+?/g);
	debug(__PACKAGE__, $port, "basename optional lang "
	    . "$basename -> $basename_q")
	    if ($basename_q =~ /^$lang_re.+/ &&
	    $basename_q =~ s/^$lang_re\\?[\-\_]?/($1)?\[\\-\\_\]?/);
	if ($basename ne $pathname) {
		$pathname_q = lc $pathname;
		debug(__PACKAGE__, $port, "pathname optional [-_.] "
		    . "$pathname -> $pathname_q")
		    if ($pathname_q =~ s/[\-\_\.]/\.?/g);
		debug(__PACKAGE__, $port, "pathname optional + "
		    . "$pathname -> $pathname_q")
		    if ($pathname_q =~ s/\+/\\\+?/g);
		debug(__PACKAGE__, $port, "pathname optional lang "
		    . "$pathname -> $pathname_q")
		    if ($pathname_q =~ /^$lang_re.+/ &&
		    $pathname_q =~ s/^$lang_re\\?[\-\_]?/($1)?\[\\-\\_\]?/);
	}

	foreach my $verdist ($distfile, $distname) {
		if ($verdist !~ /\d/) {
			debug(__PACKAGE__, $port, "skip, no digit in $verdist");
			next;
		}

		debug(__PACKAGE__, $port, "extract version from $verdist");
		$ver = $versrc = $verdist;

		# Alway start with lower case to ease further processing
		$ver = lc $ver;

		debug(__PACKAGE__, $port, "trim .ext -> $ver")
		    if ($ver =~ s/($ext_regex?)+$//);

		debug(__PACKAGE__, $port, "trim path -> $ver")
		    if ($ver =~ s/.*\///g);

		# Remove names from pkgname/fullpkgpath, prefix & suffix
		my @name_q;
		if ($basename ne $pathname) {
			# On conflict, try longest matche first
			if (index($pathname, $basename) != -1) {
				@name_q = ($pathname_q, $basename_q);
			} else {
				@name_q = ($basename_q, $pathname_q);
			}
		} else {
			@name_q = ($basename_q,);
		}
		foreach my $q (@name_q) {
			# Remove prefix / suffix
			debug(__PACKAGE__, $port, ".*$q\[-_.] * -> $ver")
			    if ($ver =~ s/^.*($q[-_\.])//);
			debug(__PACKAGE__, $port, "* [-_.]$q.* -> $ver")
			    if ($ver =~ s/([-_\.]$q).*$//);

			# Try harder, remove prefix with no separator
			# XXX could be shorter
			debug(__PACKAGE__, $port, ".*$q \\d.\\d... -> $ver")
			    if ($ver =~ s/^.*($q)(\d+\.\d+[\w\d\.]*)$/$2/);
			debug(__PACKAGE__, $port, ".*$q \\d_\\d... -> $ver")
			    if ($ver =~ s/^.*($q)(\d+\_\d+[\w\d\_]*)$/$2/);
			debug(__PACKAGE__, $port, ".*$q \\d-\\d... -> $ver")
			    if ($ver =~ s/^.*($q)(\d+\-\d+[\w\d\-]*)$/$2/);
		}

		# Remove common suffix
		my $chop_regex = qr/addons|all|bin|darwin|build|builtpkgs|dist|
		    gh|image|languages|linux|noarch|openbsd|orig|plugin|release|
		    sources?|src|stable|standalone|rpm|unix|utf-8|with|
		    x86(_64)?|x64?/x;
		debug(__PACKAGE__, $port, "chop -> $ver")
		    if ($ver =~ s/([\.\-\_]?($chop_regex))+$//g);

		# Remove all '-' prefix, most common cases
		if ($ver =~ /^(\D[^-]*-)+(.*)$/) {
			$ver = $2;
			debug(__PACKAGE__, $port,
			    "trim \\D...- -> $ver");
		}
		# Try harder, match digit-nodigit-version
		if ($ver =~ /^(\d[^-]*-)(\D[^-]*-)+(.*)$/) {
			$ver = $3;
			debug(__PACKAGE__, $port,
			    "trim \\d...-\\D...- -> $ver");
		}

		# Remove everything up to the first version-like digits,
		# only if name does not contain any digit.
		# XXX FIXME comments / code differ a bit -> code adjustment
		debug(__PACKAGE__, $port, "no digit, trim \\D -> $ver")
		    if ($name !~ /\d/ && $ver =~ /\d+[\.\-\_]\d/ &&
		    $ver =~ s/^\D+(\d.*)$/$1/);

		# Remove common prefix version marker
		debug(__PACKAGE__, $port, "trim (v...|r...) -> $ver")
		    if ($ver =~ s/^$verprfx_regex//);

		# Bruteforce, remove uncommon separator prefix
		# XXX maybe merge this with '-' and use a foreeach $sep
		debug(__PACKAGE__, $port, "trim \\D...(.|_) -> $ver")
		    if ($ver =~ s/^(\D[^\._]*(\.|_))+(.*)$/$3/);

		# Finally, check we got something plausible
		unless (isversion($ver)) {
			debug(__PACKAGE__, $port, 
			    "discard invalid version $ver");
			$ver = undef;
		}

		last if ($ver);
	}
	unless ($ver) {
		$ver = $versrc = $pkgname;
		# Alway start with lower case to ease further processing
		$ver = lc $ver;
		$ver =~ s/^(.*)-([^-]*)$/$2/g;
		debug(__PACKAGE__, $port, 
		    "fallback on pkgname version $ver");
	}

	debug(__PACKAGE__, $port, "$versrc -> $ver");

	my $rc = $ps->AddPort({
	    'name'        => $name,
	    'cat'         => $category,
	    'ver'         => $ver,
	    'maintainer'  => $maintainer,
	    'comment'     => $comment,
	    'distname'    => $distname,
	    'sufx'        => $sufx,
	    'distfile'    => $url ? $url : $distfile,
	    'sites'       => \@sites,
	    'options'     => \%pcfg,
	    'pcfg_comment'=> $pcfg_comment,
	    'homepage'    => $homepage,
	    'basepkgpath' => $basepkgpath,
	    'fullpkgpath' => $fullpkgpath,
	});
	if (!$rc) {
		$rej++;
		info(0, $fullpkgpath, "(".strchop($n_port,5)."/$total_ports) "
		    . " REJ, $ver");
	} elsif ($rc == 4) {
		$dup++;
		info(1, $fullpkgpath, "(".strchop($n_port,5)."/$total_ports) "
		    . " DUP, $ver");
	} elsif ($rc == 3) {
		$bump++;
		info(0, $fullpkgpath, "(".strchop($n_port,5)."/$total_ports) "
		    . "BUMP, $ver");
	} elsif ($rc == 2) {
		$up++;
		info(1, $fullpkgpath, "(".strchop($n_port,5)."/$total_ports) "
		    . "  UP, $ver");
	} else {
		$new++;
		info(0, $fullpkgpath, "(".strchop($n_port,5)."/$total_ports) "
		    . " NEW, $ver");
	}
    }
    print "($total_ports) Build done: new $new, up. $up, "
        . "bump(ver) $bump / rej. $rej, meta $meta, dup. $dup\n";
    return $rc;
}

1;
