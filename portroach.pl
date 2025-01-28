#!/usr/bin/perl
#
# Copyright (C) 2005-2011, Shaun Amott. All rights reserved.
# Copyright (C) 2014-2015, Jasper Lievisse Adriaanse. All rights reserved.
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
#------------------------------------------------------------------------------

use IO::Handle;
use File::Basename;
use File::Copy;
use Socket;
use POSIX;
use Fcntl;

use Proc::Queue;
use Sys::Hostname;
use LWP::UserAgent;
use MIME::Lite;
use Net::FTP;
use URI;
use JSON qw(decode_json);
use List::Util qw(first);

use DBI;

use Portroach;
use Portroach::Const;
use Portroach::Util;
use Portroach::Config;

use Portroach::SiteHandler::FTP;
use Portroach::SiteHandler::HTTP;
use Portroach::SiteHandler::HttpGuess;

use feature qw(switch);
no if $] >= 5.018, warnings => "experimental::smartmatch";

use strict;
#use warnings;

require v5.10.0;

#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

my @bad_versions;

my $datasrc;

@bad_versions =
	qw(win32 cygwin linux osx hpux irix hp-ux hp_ux solaris
	   hp-ux irix mac-?os darwin aix macintosh manual docs?
	   examples sunos tru64 rh\d-rpm suse sun4[a-z]? cvs snap
	   win jdk i[3-6]86 theme nolib dyn lin(?:ux)?(?:\d\d)?
	   \.exe$ pkg elf debian html mingw);


#------------------------------------------------------------------------------
# Signal Handlers
#------------------------------------------------------------------------------

sub terminate
{
	local $SIG{INT} = 'IGNORE';
	kill('TERM', -$$);

	info(1, "PID #$$", "Terminating...");
	exit 1;
}

sub reaper
{
	my $child;
	(1) while (($child = waitpid(-1, WNOHANG)) > 0);
	$SIG{CHLD} = \&reaper;
}

$SIG{INT}  = \&terminate;
$SIG{TERM} = \&terminate;
#$SIG{CHLD} = \&reaper;
$SIG{PIPE} = 'IGNORE';


#------------------------------------------------------------------------------
# Begin Code
#------------------------------------------------------------------------------

main();


#------------------------------------------------------------------------------
# Func: main()
# Desc: Pseudo script entry-point.
#
# Args: n/a
#
# Retn: n/a
#------------------------------------------------------------------------------

sub main
{
	my ($dbengine);

	Usage() if (!defined $ARGV[0]);

	if ($ARGV[0] eq 'debug')
	{
		debug(__PACKAGE__, 0, '-' x 72);
		debug(__PACKAGE__, 0, "Using settings:");
		debug(__PACKAGE__, 0, "  Variable: $_ -> $settings{$_}")
		    foreach (keys %settings);
		debug(__PACKAGE__, 0, '-' x 72);
		if ($#ARGV == 3 and $ARGV[1] eq 'vercompare')
		{
			my $res;
			if ($ARGV[2] eq $ARGV[3]) {
				$res = '=';
			} elsif (vercompare($ARGV[2], $ARGV[3])) {
				$res = '>';
			} else {
				$res = '<';
			}
			print "vercompare: $ARGV[2] $res $ARGV[3]\n";
			exit 0;
		} else {
			Usage();
		}
	}

	info(1, APPNAME . ' v'. APPVER . 'by ' . AUTHOR);

	SwitchUser();

	# Load stuff specific to the database engine we're using

	$dbengine = $settings{db_connstr};
	$dbengine =~ s/^\s*DBI:([A-Za-z0-9]+):?.*$/$1/;

	Portroach::SQL->Load($dbengine)
		or die 'Failed to load queries for DBI engine "'.$dbengine.'"';

	# Check DB schema version
	if (getdbver() != DB_VERSION) {
		print STDERR "Database schema mismatch; upgrade ?\n";
		exit 1;
	}

	if ($dbengine eq 'SQLite' && $settings{num_children} > 0) {
		print STDERR "SQLite is currently only supported in non-forking"
			. " mode!\n--> Forcing num_children => 0...\n";
		$settings{num_children} = 0;
		sleep 2;
	}

	$datasrc = Portroach::DataSrc->new(
		$settings{datasrc},
		$settings{datasrc_opts}
	);

	# Handle for the Sqlports database so we can close it at the right time.
	my $sdbh = Portroach::SQL::connect_sqlports($settings{sqlports});

	my $rc = (ExecArgs($ARGV[0], $sdbh) ? 0 : 1);

	$sdbh->disconnect();

	exit $rc;
}


#------------------------------------------------------------------------------
# Func: ExecArgs()
# Desc: Initiate primary operation requested by user.
#
# Args: $cmd     - Command to execute
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub ExecArgs
{
	my ($cmd, $sdbh) = @_;

	my $res;

	if ($cmd eq 'build')
	{
		print "-- [ Building ports database ] -----------------------------------------\n";
		$res = $datasrc->Build($sdbh);
	}
	elsif ($cmd eq 'check')
	{
		print "-- [ Checking ports distfiles ] ----------------------------------------\n";

		Proc::Queue::size($settings{num_children})
			unless($settings{num_children} == 0);
		$res = Check($sdbh);
	}
	elsif ($cmd eq 'generate')
	{
		Portroach::Template->templatedir($settings{templates_dir} .
		    '/dynamic');
		Portroach::Template->outputdir($settings{html_data_dir});

		$res = GenerateHTML();
	}
	elsif ($cmd eq 'rebuild')
	{
	    my $time = time;
	    $res = $datasrc->Build($sdbh);
	    if ($res) {
		$res = Prune($sdbh, $time);
	    }
	}
	elsif ($cmd eq 'mail')
	{
		Portroach::Template->templatedir($settings{templates_dir});

		if ($settings{mail_method} ne 'sendmail') {
			MIME::Lite->send($settings{mail_method}, $settings{mail_host});
		}

		$res = MailMaintainers();
	}
	elsif ($cmd eq 'showupdates')
	{
		$res = ShowUpdates();
	}
	elsif ($cmd eq 'add-mail' or $cmd eq 'remove-mail')
	{
		my (@addrs) = @ARGV; # Should be a list of addrs
		shift @addrs;        # Remove $cmd

		Usage() if (!@addrs);

		$res = ($cmd eq 'add-mail')
			? AddMailAddrs(@addrs)
			: RemoveMailAddrs(@addrs);
	}
	elsif ($cmd eq 'show-mail')
	{
		$res = ShowMailAddrs();
	}
	elsif ($cmd eq 'uncheck')
	{
		$res = Uncheck();
	}
	elsif ($cmd eq 'prune')
	{
		$res = Prune($sdbh);
	}
	else
	{
		Usage();
	}

	return $res;
}


#------------------------------------------------------------------------------
# Func: Check()
# Desc: Using the information found from a run of Build(), attempt to
#       identify ports with possible updated distfile.
#
# Args: n/a
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub Check
{
	my $sdbh = shift;
	my (%sths, @workblock, $dbh, $nofork, $num_rows, $i);

	$nofork = ($settings{num_children} == 0);

	$dbh = connect_db();

	prepare_sql($dbh, \%sths, qw(
	    portdata_count portdata_select sitedata_reset));

	STDOUT->autoflush(1);

	$sths{portdata_count}->execute(lc hostname());
	($num_rows) = $sths{portdata_count}->fetchrow_array;

	$sths{portdata_select}->execute(lc hostname());

	# Reset sitedata counter on full check (with no restriction)
	if (!$settings{restrict_port} &&
	    !$settings{restrict_category} &&
	    !$settings{restrict_maintainer}) {
		$sths{sitedata_reset}->execute;
		info(0, "Doing full check, reset site data.");
	} else {
		info(0, "Restricted check, do not reset site data.");
	}

	if ($nofork) {
		prepare_sql($dbh, \%sths, qw(
			portdata_setchecked
			portdata_setnewver
			portdata_fixnewver
			portdata_resetnewver
			portdata_setmethod
			sitedata_select
			sitedata_failure
			sitedata_success
			sitedata_initliecount
			sitedata_decliecount)
		);
	}

	$i = 0;

	while (my $port = $sths{portdata_select}->fetchrow_hashref)
	{
		my $want = 0;

		$i++;

		$want = wantport($port->{name}, $port->{cat},
		    $port->{maintainer});

		if ($nofork) {
			# This is all we need if we're not forking.
			VersionCheck($dbh, \%sths, $port) if $want;
			next;
		}

		push @workblock, $port if ($port and $want);

		next if (!$want and $i < $num_rows);

		# Got enough work?
		if ($#workblock > $settings{workqueue_size} or $i == $num_rows)
		{
			my $pid = fork;

			die "Cannot fork: $!" unless (defined $pid);

			if ($pid) {
				# Parent
				my $progress = $num_rows - $i;
				info(1, "PID #$$",
				    "Spawned ($progress ports unallocated)");
				undef @workblock;
			} else {
				# Child

				my (%sths, $dbh, $time);

				$time = time;

				$dbh = connect_db(1);

				prepare_sql($dbh, \%sths, qw(
					portdata_setchecked
					portdata_setnewver
					portdata_fixnewver
					portdata_resetnewver
					portdata_setmethod
					sitedata_select
					sitedata_failure
					sitedata_success
					sitedata_initliecount
					sitedata_decliecount)
				);

				while (my $port = pop @workblock) {
					VersionCheck($dbh, \%sths, $port);
				}

				finish_sql($dbh, \%sths);
				$dbh->disconnect;

				$time = (time - $time);
				info(1, "PID #$$",
				    "finished work block (took $time seconds)");

				exit;
			}

			(1) while (waitpid(-1, WNOHANG) > 0);
		}
	}

	(1) while (wait != -1);

	# XXX clean outdated robots_nextcheck, only HTTP handler use it

	if ($sths{portdata_select}->rows == 0) {
		print "No ports found.\n";
	} elsif (!$nofork) {
		print "Master process finished. "
		    . "All work has been distributed.\n"
	} else {
		print "Finished.\n";
	}

	finish_sql($dbh, \%sths);

	$dbh->disconnect;

	return 1;
}


#------------------------------------------------------------------------------
# Func: Uncheck()
# Desc: Reset all newver, status, and checked fields in database - equivalent
#       to doing a fresh build.
#
# Args: n/a
#
# Retn: n/a
#------------------------------------------------------------------------------

sub Uncheck
{
	my ($dbh, $sth);

	$dbh = connect_db();

	$sth = $dbh->prepare($Portroach::SQL::sql{portdata_uncheck})
		or die DBI->errstr;

	print "Resetting 'check' data...\n";

	$sth->execute;

	$sth->finish;
	$dbh->disconnect;
}


#------------------------------------------------------------------------------
# Func: VersionCheck()
# Desc: Check for an updated version of one particular port.
#
# Args: $dbh   - Database handle
#       \%sths - Prepared database statements
#       \$port - Port data extracted from database
#
# Retn: n/a
#------------------------------------------------------------------------------

sub VersionCheck
{
	my ($dbh, $sths, $port) = @_;

	my (@sites, @handlers, $found, $k, $i);

	$found = 0;
	$k = $port->{fullpkgpath};
	$i = 0;

	# XXX s/distfiles/distfile/
	return if (!$port->{distfiles} || !$port->{mastersites});

	info(1, $k, 'VersionCheck()');

	# Prepare sites to inspect: indexsite, homepage(*), mastersites
	# (*) inspect homepage only if handled by a SiteHandler
	@sites = split(' ', $port->{mastersites});
	if (Portroach::SiteHandler->FindHandler($port->{homepage})) {
		unshift @sites, $port->{homepage};
	}
	if ($port->{indexsite}) {
		info(1, $k,"indexsite, $port->{indexsite} before mastersite");
		unshift @sites, $port->{indexsite};
	}

	# Loop through master sites
	foreach my $site (@sites)
	{
		my (@files, @dates, $host, $sitedata, $method, $path_ver, $sh,
		    $new_found, $old_found, $file);

		$site .= '/' unless $site =~ /\/$/;

		$host = URI->new($site)->host;
		$sths->{sitedata_select}->execute($host);
		while (my $data = $sths->{sitedata_select}->fetchrow_hashref) {
			print STDERR "$port->{fullpkgpath}: multiple sitedata "
			    . "for $host, $sitedata->{host} already defined.\n"
			    if ($sitedata);
			debug(__PACKAGE__, $port, "sitedata $data->{host}");
			$sitedata = $data;
		}
		$sths->{sitedata_select}->finish;
		if (!$sitedata) {
			print STDERR "$port->{fullpkgpath}: "
			    . "sitedata $host not found, ignore ?.\n";
			next;
		}

		$method = 0;
		$old_found = 0;
		$new_found = 0;

		$site = URI->new($site)->canonical;

		last if ($i >= $settings{mastersite_limit});

		$i++;

		info(1, $k, 'Checking site: ' . strchop($site, 60));

		# Look to see if the URL contains the distfile version.
		# This will affect our checks and guesses later on.
		if ($site =~ m:^.*/($verlike_regex)/.*$:) {
			my $dir_ver = $1;
			my $dir_ver_q = vertoregex($dir_ver);
			debug(__PACKAGE__, $port, "port_ver $port->{ver}, "
			    . "dir_ver $dir_ver, regex $dir_ver_q");
			if ($dir_ver eq $port->{ver}) {
				$path_ver = $dir_ver;
			} elsif ($port->{ver} =~ /^$dir_ver_q/) {
				$path_ver = $dir_ver;
			}
		}
		debug(__PACKAGE__, $port, "path version $path_ver")
		    if ($path_ver);

		# Check for special handler for this site first
		if ($sh = Portroach::SiteHandler->FindHandler($site)) {
			if ( first { $sh->{name} eq $_ } @handlers) {
				info(1, $k, "Already tried site handler "
				    . "$sh->{name}, skip $site");
				next;
			}
			push(@handlers, $sh->{name});
			info(1, $k, $host, "Using site handler $sh->{name}.");

			if (!$sh->GetFiles($site, $port, \@files)) {
				info(1, $k, $host, "$sh->{name} GetFiles() "
				    . "failed for $site");
				$sths->{sitedata_failure}->execute($site->host);
				next;
			} else {
				$sths->{sitedata_success}->execute($site->host);
				$method = METHOD_HANDLER if (@files);
			}

		# Get files list from FTP
		} elsif ($site->scheme eq 'ftp') {
			$sh = Portroach::SiteHandler::FTP->new;
			if (!$sh->GetFiles($site, $port, \@files, $path_ver)) {
				info(1, $k, $host, "$sh->{name} GetFiles() "
				    . "failed for $site");
				$sths->{sitedata_failure}->execute($site->host);
				next;
			} else {
				$sths->{sitedata_success}->execute($site->host);
				$method = METHOD_LIST if (@files);
			}

		# Get files list from HTTP
		} else {
			unless (robotsallowed($dbh, $site, $sitedata)) {
				info(1, $k, $host, 'Ignoring site as per '
				    . 'rules in robots.txt.');

				# Don't count 'robots' bans as a failure.
				# (We fetch them from the database so that
				# they can be re-checked every so often.)
				$i--;

				next;
			}

			$sh = Portroach::SiteHandler::HTTP->new;
			if (!$sh->GetFiles($site, $port, \@files, $path_ver)) {
				info(1, $k, $host, "$sh->{name} GetFiles() "
				    . "failed for $site");
				$sths->{sitedata_failure}->execute($site->host);
				# not next; give a chance to HttpGuess
			} else {
				$sths->{sitedata_success}->execute($site->host);
				$method = METHOD_LIST if (@files);
			}
		}

		# No files found - try some guesses ... over http/s only
		if (!@files && !$port->{indexsite} &&
		    $site->scheme =~ /^https?/) {
			my ($ua, $resp);

			# Check distfile still exists
			$ua = lwp_useragent();
			$resp = $ua->head($site.$port->{distfiles});

			if (!$resp->is_success || $resp->status_line !~ /^2/) {
				info(1, $k, $host,
				    'Not doing any guess, distfile not found.');
				$sths->{sitedata_failure}->execute($site->host);
				next;
			}

			# We keep a counter of "lies" from each site, and only
			# re-check every so often.
			if ($sitedata->{liecount} > 0) {
				info(1, $k, $host,
				    'Not doing any guessing; '
				    . 'site has previously lied.');
				$sths->{sitedata_decliecount}->execute(
				    $sitedata->{host})
				    unless ($settings{precious_data});
				next;
			}

			# Verify site gives an error for bad filenames
			$resp = $ua->head($site.randstr(8).'_shldntexist.tgz');
			# Got a response which wasn't HTTP 4xx -> bail out
			if ($resp->is_success && $resp->status_line !~ /^4/) {
				info(1, $k, $host,
				    'Not doing any guessing; '
				    . 'site is lieing to us.');
				$sths->{sitedata_initliecount}->execute(
				    $sitedata->{host})
				    unless($settings{precious_data});
				next;
			}

			# New we are ready to guess ...
			$sh = Portroach::SiteHandler::HttpGuess->new;
			my $ver = $port->{ver}; # XXX search new version again
			if (!$sh->GetFiles($site,$port,\@files,$path_ver,$ver)) {
				info(1, $k, $host, "$sh->{name} GetFiles() "
				    . "failed for $site");
				$sths->{sitedata_failure}->execute($site->host);
				next;
			} else {
				$sths->{sitedata_success}->execute($site->host);
				$method = METHOD_GUESS if (@files);
			}
		}

		debug(__PACKAGE__, $port, "Files from $site:");
		debug(__PACKAGE__, $port, " -> $_") foreach @files;

		next if (!@files);

		info(1, $k, $host, 'Found ' . scalar @files . ' files');

		debug(__PACKAGE__, $port, "port newver '$port->{newver}'");

		$file = FindNewestFile($port, $site, \@files);

		$old_found = 1 if $file->{oldfound};
		$new_found = 1 if $file->{newfound};

		if ($new_found && $file->{version} ne $port->{newver}) {
			info(0, $k, $host,
			    "UPDATE $port->{ver} -> $file->{version}");
			$sths->{portdata_setnewver}->execute(
				$file->{version},
				$method,
				$file->{url},
				$port->{id}
			) unless ($settings{precious_data});

		} elsif ($new_found && (
		    $file->{url} ne $port->{newurl} ||
		    $method != $port->{method})) {
			regress($k, $host,
			    "url $port->{newurl} -> $file->{url}")
			    if ($file->{url} ne $port->{newurl});
			regress($k, $host,
			    "method $port->{method} -> $method")
			    if ($method != $port->{method});
			$sths->{portdata_fixnewver}->execute(
				$method,
				$file->{url},
				$port->{id}
			) unless ($settings{precious_data} ||
			    !$settings{regress});

		} elsif ($new_found) {
			info(1, $k, $host, "STILL new, $file->{version}");

		} elsif ($old_found && $port->{method} &&
		    $method != $port->{method}) {
			regress($k, $host,
			    "method $port->{method} -> $method");
			$sths->{portdata_setmethod}->execute(
				$method,
				$port->{id}
			) unless ($settings{precious_data} ||
			    !$settings{regress});

		} elsif ($old_found && $port->{method}) {
			info(1, $k, $host, "STILL old, $port->{ver}");

		} elsif ($old_found) {
			info(0, $k, $host, "FOUND old, $port->{ver}");
			$sths->{portdata_setmethod}->execute(
				$method,
				$port->{id}
			) unless ($settings{precious_data});
		}

		$found = 1 if ($old_found && !$found);
		$found = 2 if ($new_found);
		last if ($new_found && $settings{newfound_enable});
		last if ($old_found && $settings{oldfound_enable});
	}

	# Update checked timestamp
	$sths->{portdata_setchecked}->execute($port->{id})
		unless ($settings{precious_data});

	if ($found != 2 && $port->{newver}) {
		regress($k, "newver not found($found): "
		    . "old $port->{ver}, new $port->{newver}, "
		    . "method $port->{method}");
		$sths->{portdata_resetnewver}->execute(
			$port->{id}
		) unless ($settings{precious_data} ||
		    !$settings{regress});
		return;
	} elsif (!$found && $port->{method}) {
		regress($k, "not found: old $port->{ver}, "
		    . "method $port->{method}");
		$sths->{portdata_setmethod}->execute(
		    0, $port->{id}
		) unless ($settings{precious_data} ||
		    !$settings{regress});
		return;
	}

	if (!$found) {
		info(0, $k, "*** not found: $port->{ver}");
	} else {
		info(1, $k, 'Done');
	}
}


#------------------------------------------------------------------------------
# Func: FindNewestFile()
# Desc: Given an array of files, try to determine if any are newer than our
#       current version, and return the newest, if any.
#
# Args: \%port  - Port hash from database.
#       $site   - Site URL.
#       \@files - Files returned from spidering (+ absolute path or no path).
#
# Retn: \%res   - Hash containing file info:
#                   newfound - True if we found a suitable file.
#                   oldfound - True if we found the "current" file.
#                   version  - Version of file found.
#                   url      - URL of file.
#------------------------------------------------------------------------------

sub FindNewestFile
{
	my ($port, $site, $files) = @_;

	my ($poss_match, $poss_url, $old_found, $new_found);

	my $distfile = $port->{distfiles};
	my $distfile_q = lc $distfile;
	my $old_vq = vertoregex($port->{ver});# lazy version separator X.Y ~ XY
	if ($distfile_q =~ s/^(.*)(?:$verprfx_regex?)*$old_vq(.*)$/$1##v##$2/) {
		$distfile_q = vertoregex($distfile_q);
		$distfile_q =~ s/##v##/(?:$verprfx_regex)?(\\d.*?)/;
		debug(__PACKAGE__, $port, "distfile regex query, "
		    . "$distfile -> $distfile_q");
	} else {
		debug(__PACKAGE__, $port, "invalid distfile $distfile, "
		    . "version ($verprfx_regex)?$old_vq not found.");
		$distfile_q = undef;
	}

	foreach my $file (@$files)
	{
		my ($old_v, $new_v, $poss_path);
		$old_v = $port->{ver};

		if ($file =~ /^(.*)\/(.*?)$/) {
			# Files from SiteHandlers can come with url/paths;
			# we're only handling absolute paths and url (later on)
			$poss_path = $1;
			$file = $2;
			debug(__PACKAGE__, $port, "path detected, split: "
			    . "path $poss_path, file $file");
		} else {
			$poss_path = '';
		}

		# Skip beta versions if requested
		if ($port->{skipbeta} && isbeta($file) && !isbeta($distfile)) {
			debug(__PACKAGE__, $port, "skip beta $file");
			next;
		}

		# Weed out some bad matches
		# XXX the following distfile_q filter should prevent such wrong
		# filename going through ... maybe drop this
		my $skip = 0;
		foreach (@bad_versions) {
			last if (!$settings{freebsdhacks_enable});
			next unless ($file =~ /$_/ && $distfile !~ /$_/);
			debug(__PACKAGE__, $port, "skip $file, bad $_");
			$skip = 1;
			last;
		}
		next if ($skip);

		# Version already know from <url>%%<ver> string
		if ($file =~ /%%/) {
			$new_v = lc $file;
			$new_v =~ s/^.*%%//;
			$file =~ s/%%.*$//;
			debug(__PACKAGE__, $port, "new version ? $new_v");

			if ($new_v eq $port->{ver}) {
				debug(__PACKAGE__, $port, "old found: "
				    . "file $file, old $port->{ver}");
				$old_found = 1;
				next;
			}

			# Fallback on file if tag was too complex for handler
			unless (isversion($new_v, $old_v)) {
				debug(__PACKAGE__, $port, "fallback on "
				    . "file $file, isversion $old_v");
				undef $new_v;
			}
		}

		# Version extraction from filename
		if ($distfile_q && !$new_v &&
		    $file =~ /^($distfile_q)$/i && $2) {
			my $ver = $2;
			debug(__PACKAGE__, $port, "new file ? $file");
			$new_v = lc $ver;

			# Version is much longer than original - skip it
			if (length $new_v > (12 + length $old_v)) {
				debug(__PACKAGE__, $port,
				    "skip, $new_v > 12 + $old_v");
				next;
			}

			debug(__PACKAGE__, $port,
			    "file $file =~ $distfile && $ver -> $new_v");
		} elsif (!$new_v) {
			debug(__PACKAGE__, $port,
			    "discard, file $file !~ $distfile ($distfile_q)");
			next;
		}

		# XXX same distfile / version extraction from path ?

		# Normalize \d [-_] \d into \d.\d
		debug(__PACKAGE__, $port, "normalize new_v -> $new_v")
		    if ($new_v =~ s/(?<=\d)[\-\_](?=\d)/\./g);

		# Check both version looks the same and are valid
		unless (isversion($new_v, $old_v)) {

			# Detect if version was collapsed into a shorter format
			my $short_v = $old_v;
			$short_v =~ s/[\.\-\_]//g;
			unless (isversion($new_v, $short_v)) {
				debug(__PACKAGE__, $port,
				    "skip, invalid version $new_v !~ $old_v");
				next;
			}
			unless (length($short_v) <= length($new_v)) {
				debug(__PACKAGE__, $port,
				    "skip, length missmatch $new_v < $short_v");
				next;
			}
			debug(__PACKAGE__, $port, "shorter version "
			    . "$old_v -> $short_v ~ $new_v");

			# Rebuild proper version from the short string
			my $long_v;
			$short_v = $old_v;
			while ($short_v =~ s/([^\.\-\_]+)([\.\-\_]|$)//) {
				$long_v .= substr($new_v, 0, length($1)).$2;
				$new_v = substr($new_v, length($1));
			}
			$long_v .= $new_v;
			debug(__PACKAGE__, $port, "rebuilt version -> $long_v");
			$new_v = $long_v;
		}

		debug(__PACKAGE__, $port, "new_v ${new_v}, "
		    . "old_v ${old_v}, file ${file}");

		# Skip any specific versions if requested
		$skip = 0;
		foreach (split (/\s+/, $port->{skipversions})) {
			next unless ($new_v eq $_);
			$skip = 1;
			debug(__PACKAGE__, $port, "skip $new_v eq $_");
			last;
		}
		next if ($skip);

		# Skip silly str (x.y-sillyv) if oldv !~ sillyv (except beta)
		# XXX not that usefull afterall ?
		unless ($settings{sillystrings_enable}) {
			if ($new_v =~ /[\.\-\_]([A-Za-z]+[A-Za-z_-]{2,})$/) {
				my $str = $1;
				debug(__PACKAGE__,$port,"sillystrings $new_v");
				if ($old_v !~ /[\.\-\_]$str$/ && (
				    $str !~ /^($beta_regex)$/ ||
				    length $1 < length $str) # short RE match
				) {
					debug(__PACKAGE__, $port,
					    "$old_v !~ [\.\-\_]str");
					debug(__PACKAGE__, $port,
					    "$str !~ beta_regex")
					    if ($str !~ /^($beta_regex)$/);
					debug(__PACKAGE__, $port,
					    "length $1 < length $str")
					    if (length $1 < length $str);
					next;
				}
			}
		}

		# Only allow new major version if port isn't version-specific

		if ($port->{limitver}) {
			unless ($new_v =~ /$port->{limitver}/) {
				debug(__PACKAGE__, $port,
				   "skip $new_v =~ /$port->{limitver}/");
				next;
			}

		} elsif (
		    (fullpkgpathtoport($port->{basepkgpath}) =~
		    /^(.*\D)(\d{1,3})(?:[\-\_]\D+)?$/) ||
		    (fullpkgpathtosubcat($port->{basepkgpath}) =~
		    /^(.*\D)(\d{1,3})(?:[\-\_]\D+)?$/)) {
			my $nm_nums = $2;
			my $vr_nums = $new_v;
			my $vo_nums = $old_v;

			debug(__PACKAGE__, $port, "prefix $1 nm_nums $nm_nums");

			unless (($1.$2) =~
			    /(?:md5|bz2|bzip2|rc4|rc5|ipv6|mp3|utf8)$/) {
				my $fullver = '';
				while ($vo_nums =~ s/^(\d+?)[\.\-\_]?//) {
					$fullver .= $1;
					last if ($fullver eq $nm_nums);
				}

				if ($fullver eq $nm_nums) {
					debug(__PACKAGE__, $port,
					   "fullver $fullver nm_nums $nm_nums");
					$vr_nums =~ s/[\.\-\_]//g;
					unless ($vr_nums =~ /^$nm_nums/) {
						debug(__PACKAGE__, $port,
						    "skip $vr_nums ~ $nm_nums");
						next;
					}
				}
			}
		}

		if (defined $port->{limiteven} and $port->{limitwhich} >= 0) {
			unless (checkevenodd($new_v,
			    $port->{limiteven}, $port->{limitwhich})) {
				debug(__PACKAGE__, $port,
				    "skip $new_v: "
				    . "limiteven $port->{limiteven}, "
				    . "limitwhich $port->{limitwhich}");
				next;
			}
		}

		# Test our new version string
		debug(__PACKAGE__, $port, "check new_v $new_v <> old_v $old_v");

		if ($new_v eq $old_v) {
			debug(__PACKAGE__, $port, "old found");
			$old_found = 1;

		} elsif (vercompare($new_v, $old_v)) {
			debug(__PACKAGE__, $port, "new found");
			$new_found = 1;

			# Keep going until we find the newest version
			if (!defined($poss_match) or
			    vercompare($new_v, $poss_match)) {
				$poss_match = $new_v;

				# If custom url or path, use it
				if ($poss_path =~ /^https?:\/\/[^\/]+\//) {
					$poss_url = URI->new($poss_path);
				} else {
					$poss_url = $site->clone;
					$poss_url->path($poss_path)
					    if ($poss_path);
				}

				# No url if there is no file (%%version)
				if ($file) {
					$poss_url->path($poss_url->path . '/')
					    if ($poss_url !~ /\/$/);
					uri_filename($poss_url, $file);
				} else {
					$poss_url = "";
				}

				debug(__PACKAGE__, $port, "last found "
				    . "poss $poss_match '$poss_url'");
				next;
			}
		} else {
			debug(__PACKAGE__, $port, "skip $new_v < $old_v");
		}
	}

	return {
		'newfound' => $new_found,
		'oldfound' => $old_found,
		'version'  => $poss_match,
		'url'      => $poss_url
	};
}

#------------------------------------------------------------------------------
# Func: robotsallowed()
# Desc: Determine if a given site blocks robots (or us, specifically).
#
# Args: $dbh     - Database handle, connected.
#       $url     - URL we intend to fetch.
#       $site    - Relevant record (hash ref.) from sitedata table.
#
# Retn: $allowed - Are we permitted to spider site?
#------------------------------------------------------------------------------

sub robotsallowed
{
	my ($dbh, $url, $site) = @_;

	my (@paths, $allowed, $sitepath, $pathmatch);

	# Checks enabled?
	if (!$settings{robots_enable}) {
		return 1;
	}

	# Do our records need updating?
	if ($site->{robots_outofdate} || $site->{robots} == ROBOTS_UNKNOWN) {
		my ($ua, $resp);

		info(1, "(R) $site->{host}", "processing robots.txt");

		$ua = lwp_useragent();

		$resp = $ua->get("$site->{type}://$site->{host}/robots.txt");

		if ($resp->is_success) {
			if ($resp->status_line =~ /^4/) {
				# HTTP 404 = no blocks. We can roam free.
				$allowed = ROBOTS_ALLOW;
				info(1, "(R) $site->{host}", "no robots.txt");
			} else {
				# Process rules
				my ($data, $agentmatch);

				$allowed = ROBOTS_ALLOW;

				$data = $resp->content;

				foreach (split /[\r\n]+/, $data) {
					my $rule = $_;
					$rule =~ s/^\s*//;
					$rule =~ s/#.*$//;
					$rule =~ s/\s*$//;

					if ($rule =~ s/^User-Agent:\s*//i) {
						my $agent_regex;

						# Build a regex from the wildcard
						# expression. Ignores the possibility
						# of escaped asterisks.
						$agent_regex = '^.*';
						foreach (split /(\*)/, $rule) {
							if ($_ eq '*') {
								$agent_regex .= '.*';
							} else {
								$agent_regex .= quotemeta $_
									unless $_ eq '';
							}
						}
						$agent_regex .= '.*$';

						if (USER_AGENT =~ /$agent_regex/i) {
							my $app_regex = '.*' . quotemeta(APPNAME) . '.*';

							if ($rule =~ /$app_regex/i) {
								$allowed = ROBOTS_SPECIFIC;
							} elsif ($allowed != ROBOTS_SPECIFIC) {
								$allowed = ROBOTS_BLANKET;
							}

							$agentmatch = 1;
						} else {
							$agentmatch = 0;
						}

						info(1, "(R) $site->{host}", "-> $rule "
						    . "(matched: $agentmatch; type: $allowed)");
						next;
					}

					if ($rule =~ /^(?:Allow|Disallow):/i && !defined $agentmatch) {
						# No User-Agent was specified, so
						# assume '*' is implied.
						$allowed = ROBOTS_BLANKET;
						$agentmatch = 1;
					}

					if ($agentmatch && $rule =~ s/^Disallow:\s*//i) {
						$rule = '/' if ($rule eq '');
						push @paths, $rule;
					}
				}
			}
		} else {
			# Couldn't access server for some reason.
			# Assume we're allowed for now, but it's
			# probable that the site will fail later
			# on anyway.
			return 1;
		}

		if (!$settings{precious_data}) {
			my %sths;
			prepare_sql($dbh, \%sths, 'sitedata_setrobots');
			$sths{sitedata_setrobots}->execute($allowed, join("\n", @paths), $site->{host});
			finish_sql($dbh, \%sths);
		}
	} else {
		$allowed = $site->{robots};
		@paths = split(/\n+/, $site->{robots_paths});
	}

	# See if we're trying to access a banned path.

	$sitepath = $url;
	$sitepath =~ s/^[A-Z0-9]+:\/\///i;
	$sitepath =~ s/^[^\/]*//;

	$pathmatch = 0;

	foreach (@paths) {
		my $pathstart = substr($sitepath, 0, length $_);
		if ($pathstart eq $_) {
			$pathmatch = 1;
			info(1, "(R) $site->{host}", "path matched ($_)");
			last;
		}
	}

	return 1 if !$pathmatch;

	if ($settings{robots_checking} eq 'strict') {
		# Explicit 'allow' only
		info(1, "(R) strict $site->{host}", ($allowed == ROBOTS_ALLOW ?
		    "Allow " : "Block ") . strchop($url, 60));
		return ($allowed == ROBOTS_ALLOW);
	} else {
		# Ignore blanket bans
		info(1, "(R) $site->{host}", ($allowed == ROBOTS_ALLOW ?
		    "Allow " : "Block ") . strchop($url, 60));
		return ($allowed != ROBOTS_SPECIFIC);
	}
}


#------------------------------------------------------------------------------
# Func: GenerateHTMLPorts()
# Desc: Build ports list web pages based on database data.
#
# Args: $sth      - Statement to fetch port data, given a specific page
#       $page     - Specific page to generate
#       \%outdata - Hash containing global template data
#       $prefix   - Type of output, output into $prefix_$page.html
#
# Retn: n/a
#------------------------------------------------------------------------------

sub GenerateHTMLPorts
{
	my ($sth, $page, $template, $outdata, $prefix) = @_;
	my ($row, @results, $fh);

	$outdata->{index} = $page;
	if ($prefix eq "maintainer") {
		# cleanup maintainer email
		$outdata->{index} =~ s/\>[\s\,]+([^\s])/\>& $1/g;
		$outdata->{index} =~ s/\<.*?\>//g;
		$outdata->{index} =~ s/\s+/ /g;
		$outdata->{index} =~ s/\s$//g;
	}
	info(1, "$prefix\_$outdata->{index}");

	if ($prefix eq "state") {
		$sth->execute();
	} else {
		$sth->execute($page);
	}

	while ($row = $sth->fetchrow_hashref) {
		if ($row->{ignore}) {
			$row->{method} = 'X';
			$row->{newver} = '';
			$row->{newurl} = '';
		} else {
			if ($row->{method} eq METHOD_LIST) {
				$row->{method} = 'L';
			} elsif ($row->{method} eq METHOD_GUESS) {
				$row->{method} = 'G';
			} elsif ($row->{method} eq METHOD_HANDLER) {
				$row->{method} = 'S';
			} else {
				$row->{method} = '?';
			}
		}

		if ($row->{newver} && ($row->{ver} ne $row->{newver})) {
			$row->{newdistfile} = 'updated';
		} else {
			next if ($settings{hide_unchanged});
			$row->{newdistfile} = '';
		}
		$row->{updated} =~
			s/:\d\d(?:\.\d+)?$/ $settings{local_timezone}/;
		$row->{checked} =~
			s/:\d\d(?:\.\d+)?$/ $settings{local_timezone}/;

		if ($row->{homepage}) {
			$row->{head} = "<a href=\"$row->{homepage}\">"
				. "$row->{basepkgpath}</a>";
		} else {
			$row->{head} = $row->{basepkgpath};
		}
		if ($row->{newurl}) {
			$row->{new} = "<a href=\"$row->{newurl}\">"
				. "$row->{newver}</a>";
		} else {
			$row->{new} = $row->{newver};
		}

		$template->pushrow($row);
		push(@results, $row);
	}

	$template->applyglobal(\%$outdata);
	$outdata->{index} =~ tr|/|_|;
	$outdata->{index} =~ tr| |_|;
	$template->output("$prefix\_$outdata->{index}.html");
	$template->reset;

	open($fh, '>', "$settings{html_data_dir}/json/"
		. "$prefix\_$outdata->{index}.json")
		or die "$prefix\_$outdata->{index}.json: $!";
	print $fh JSON::encode_json(\@results);
	close($fh);

	undef @results;
}


#------------------------------------------------------------------------------
# Func: GenerateHTML()
# Desc: Build web pages based on database data.
#
# Args: n/a
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub GenerateHTML
{
	my (%sths, %outdata, %totals, @assets, @results, $dbh, $sth, $template,
	    $page, $row, $fh, $total, $outdated, $unknow, $maintainers);

	$dbh = connect_db();

	prepare_sql($dbh, \%sths, qw(
	    portdata_gencategories
	    portdata_genmaintainers
	    portdata_gensites
	    portdata_selectall_cat
	    portdata_selectall_limited
	    portdata_selectall_maintainer
	    portdata_selectall_outdated
	    portdata_selectall_site
	    portdata_selectall_unknow)
	);

	print "Organising results...\n";
	$sths{portdata_genmaintainers}->execute;
	$sths{portdata_gencategories}->execute;
	$sths{portdata_gensites}->execute;

	emptydir($settings{html_data_dir});
	emptydir("$settings{html_data_dir}/json/");

	# Put together some output data for the templates

	my @gmt = ($settings{local_timezone} eq 'GMT') ? gmtime : localtime;
	$outdata{date} = sprintf('%d-%02d-%02d',$gmt[5]+1900,++$gmt[4],$gmt[3]);
	$outdata{time} = sprintf('%02d:%02d', $gmt[2], $gmt[1]);
	$outdata{appname} = APPNAME;
	$outdata{appver}  = APPVER;
	$outdata{author}  = AUTHOR;

	# Produce indexes pages
	
	print "Generating dynamic categories.html\n";

	$template = Portroach::Template->new('categories.html')
	    or die "categories.html template not found!\n";

	$sth = $dbh->prepare("SELECT * FROM categories") or die DBI->errstr;
	$sth->execute;

	$total = $outdated = $unknow = 0;
	while ($row = $sth->fetchrow_hashref) {
		$row->{link} = "cat_".$row->{cat};
		$row->{link} =~ tr|/|_|;
		$row->{percentage} = 0 + sprintf('%.2f', $row->{percentage})
		    if ($row->{percentage});
		$row->{total} += 0 if ($row->{total});
		$row->{withnewdistfile} += 0 if ($row->{withnewdistfile});
		$row->{unknow} += 0 if ($row->{unknow});

		$template->pushrow($row);
		push(@results, $row);

		$total += $row->{total};
		$unknow += $row->{unknow};
		$outdated += $row->{withnewdistfile};
	}

	$sth->finish;

	die("No categories found; database have been built previously?")
	    unless ($total);

	$outdata{total_ports} = $total;
	$outdata{total_unknow} = $unknow;
	$outdata{total_outdated} = $outdated;
	$outdata{outdated_percentage} = sprintf('%.2f',($outdated/$total)*100);
	$outdata{unknow_percentage} = sprintf('%.2f',($unknow/$total)*100);

	$template->applyglobal(\%outdata);
	# XXX output categories as index.html
	$template->output("index.html");
	$template->reset;

	$totals{results} = \@results;
	$totals{summary} = {
	    'total_ports'         => $total,
	    'total_unknow'        => $unknow,
	    'total_outdated'      => $outdated,
	    'outdated_percentage' => sprintf('%.2f',($outdated/$total)*100),
	    'unknow_percentage' => sprintf('%.2f',($unknow/$total)*100),
	};

	open($fh, '>>', "$settings{html_data_dir}/json/categories.json")
	    or die "categories.json: $!";
	print $fh JSON::encode_json(\%totals);
	close($fh);

	undef $totals{results};
	undef @results;

	print "Generating dynamic maintainers.html\n";

	$template = Portroach::Template->new('maintainers.html')
	    or die "maintainers.html template not found!\n";

	$sth = $dbh->prepare("SELECT * FROM maintainers") or die DBI->errstr;
	$sth->execute;

	$total = $outdated = 0;
	while ($row = $sth->fetchrow_hashref) {
		if ($row->{maintainer}) {
			$row->{maintainer} =~ s/\>[\s\,]+([^\s])/\>& $1/g;
			$row->{maintainer} =~ s/\<.*?\>//g;
			$row->{maintainer} =~ s/\s+/ /g;
			$row->{maintainer} =~ s/\s$//g;
		}
		$row->{link} = "maintainer_".$row->{maintainer};
		$row->{link} =~ tr| |_|;
		$row->{percentage} = 0 + sprintf('%.2f', $row->{percentage})
		    if ($row->{percentage});
		$row->{total} += 0 if ($row->{total});
		$row->{unknow} += 0 if ($row->{unknow});
		$row->{withnewdistfile} += 0 if ($row->{withnewdistfile});

		$template->pushrow($row);
		push(@results, $row);

		$maintainers++;
		$total += $row->{total};
		$unknow += $row->{unknow};
		$outdated += $row->{withnewdistfile};
	}
	
	$sth->finish;

	die("No maintainers found; database have been built previously?")
	    unless ($total);
	die("total ports mismatch between maintainers & results")
	    if ($outdata{total_ports} != $total);
	die("total outdated mismatch between maintainers & results")
	    if ($outdata{total_outdated} != $outdated);

	$outdata{total_maintainers} = $maintainers;

	$template->applyglobal(\%outdata);
	$template->output("maintainers.html");
	$template->reset;

	$totals{results} = \@results;
	$totals{summary}{total_maintainers} = $maintainers;

	open($fh, '>>', "$settings{html_data_dir}/json/maintainers.json")
	    or die "maintainers.json: $!";
	print $fh JSON::encode_json(\%totals);
	close($fh);

	undef $totals{results};
	undef @results;

	print "Generating dynamic sites.html\n";

	$template = Portroach::Template->new('sites.html')
	    or die "sites.html template not found!\n";

	$sth = $dbh->prepare("SELECT * FROM sites") or die DBI->errstr;
	$sth->execute;

	$total = 0;
	while ($row = $sth->fetchrow_hashref) {
		$row->{link} = "site_".$row->{host};
		$row->{percentage} = 0 + sprintf('%.2f', $row->{percentage})
		    if ($row->{percentage});
		$row->{total} += 0 if ($row->{total});
		$row->{withnewdistfile} += 0 if ($row->{withnewdistfile});
		$row->{unknow} += 0 if ($row->{unknow});
		$row->{failures} += 0 if ($row->{failures});
		$row->{successes} += 0 if ($row->{successes});
		$row->{liecount} += 0 if ($row->{liecount});
		$row->{robots_nextcheck} =~
			    s/:\d\d(?:\.\d+)?$/ $settings{local_timezone}/;

		$template->pushrow($row);
		push(@results, $row);

		$total++;
	}

	$sth->finish;

	$outdata{total_hosts} = $total;

	$template->applyglobal(\%outdata);
	$template->output("sites.html");
	$template->reset;

	die("No sites found; database have been built previously?")
	    if ($total == 0);

	$totals{results} = \@results;
	$totals{summary}{total_hosts} = $total;

	open($fh, '>>', "$settings{html_data_dir}/json/sites.json")
	    or die "sites.json: $!";
	print $fh JSON::encode_json(\%totals);
	close($fh);

	undef $totals{results};
	undef @results;
	undef %totals;

	print "Creating restricted ports (portconfig) page...\n";

	$template = Portroach::Template->new('restricted-ports.html')
	    or die "restricted-ports.html template not found!\n";

	$sths{portdata_selectall_limited}->execute;

	while (my $row = $sths{portdata_selectall_limited}->fetchrow_hashref) {
		$row->{limiteven}      = $row->{limiteven}  ? 'EVEN' : 'ODD';
		$row->{limitevenwhich} = $row->{limitwhich} ? (
		    $row->{limitwhich}.':'.$row->{limiteven}) : '';

		$template->pushrow($row);
		push(@results, $row);
	}

	$template->applyglobal(\%outdata);
	$template->output('restricted-ports.html');

	open($fh, '>', "$settings{html_data_dir}/json/restricted.json")
	    or die "restricted.json: $!";
	print $fh JSON::encode_json(\@results);
	close($fh);

	undef @results;

	# Produce ports pages

	$template = Portroach::Template->new('ports.html')
	    or die "ports.html template not found!\n";

	print "Creating state pages...\n";

	GenerateHTMLPorts($sths{portdata_selectall_outdated}, "outdated",
	    $template, \%outdata, "state");
	GenerateHTMLPorts($sths{portdata_selectall_unknow}, "unknow",
	    $template, \%outdata, "state");

	print "Creating maintainer pages...\n";

	$sth = $dbh->prepare('SELECT DISTINCT maintainer FROM maintainers')
	    or die DBI->errstr;
	$sth->execute;
	GenerateHTMLPorts($sths{portdata_selectall_maintainer}, $page,
	    $template, \%outdata, "maintainer")
	    while ($page = $sth->fetchrow_array);

	print "Creating category pages...\n";

	$sth = $dbh->prepare('SELECT DISTINCT cat FROM categories')
	    or die DBI->errstr;
	$sth->execute;
	GenerateHTMLPorts($sths{portdata_selectall_cat}, $page,
	    $template, \%outdata, "cat")
	    while ($page = $sth->fetchrow_array);

	print "Creating site pages...\n";

	$sth = $dbh->prepare('SELECT DISTINCT host FROM sites')
	    or die DBI->errstr;
	$sth->execute;
	GenerateHTMLPorts($sths{portdata_selectall_site}, $page,
	    $template, \%outdata, "site")
	    while ($page = $sth->fetchrow_array);

	# Cleanup & copy assets

	finish_sql($dbh, \%sths);
	$dbh->disconnect;

	my $_dir = "$settings{templates_dir}/assets/";
	return 1 unless (-d $_dir);

	print "Copying assets...\n";

	emptydir("$settings{html_data_dir}/assets/");
	@assets = glob("$_dir/*");
	foreach my $asset (glob("$_dir/*")) {
		info(1, "copy:$asset");
		copy($asset, "$settings{html_data_dir}/assets") or die $!;
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: MailMaintainers()
# Desc: Send a reminder e-mail to interested parties, about their ports.
#
# Args: n/a
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub MailMaintainers
{
	my (%sths, $dbh, $template);

	if (!$settings{mail_enable}) {
		print "Reminder mails are disabled; taking no action.\n";
		return 1;
	}

	print "Mailing maintainers of out-of-date ports...\n";

	$dbh = connect_db();

	prepare_sql($dbh, \%sths,
		qw(maildata_select portdata_findnewnew portdata_setmailed)
	);

	$sths{maildata_select}->execute;

	$template = Portroach::Template->new('reminder.mail')
		or die "reminder.mail template not found!\n";

	while (my ($addr) = $sths{maildata_select}->fetchrow_array) {
		my $msg;
		my $ports = 0;
		$sths{portdata_findnewnew}->execute("%<${addr}>%");
		$template->applyglobal({maintainer => lc $addr});

		while (my $port = $sths{portdata_findnewnew}->fetchrow_hashref) {
			$template->pushrow($port);
			$ports++;
		}

		if ($ports == 0) {
			$template->reset;
			next;
		}

		info(0, $addr, "$ports new port(s) out of date");

		$msg = MIME::Lite->new(
			From     => $settings{mail_from} =~ /@/
			                ? $settings{mail_from}
			                : $settings{mail_from}.'@'.hostname(),
			To       => $addr,
			Subject  => $settings{mail_subject},
			Data     => $template->string
		);

		$msg->replace('X-Mailer' => USER_AGENT);

		$msg->send;

		$template->reset;

		# Second pass to mark port newvers as mailed

		if (!$settings{precious_data}) {
			$sths{portdata_findnewnew}->finish;
			$sths{portdata_findnewnew}->execute("%<${addr}>%");

			while (my $port = $sths{portdata_findnewnew}->fetchrow_hashref) {
				$sths{portdata_setmailed}->execute(
				    $port->{newver}, $port->{fullpkgpath});
			}
		}
	}

	finish_sql($dbh, \%sths);
	$dbh->disconnect;

	return 1;
}


#------------------------------------------------------------------------------
# Func: ShowUpdates()
# Desc: Produce a simple report showing ports with updates.
#
# Args: n/a
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub ShowUpdates
{
	my (%sths, $dbh, $maintainer, $count);

	$dbh = connect_db();

	prepare_sql($dbh, \%sths, 'portdata_selectupdated');

	$sths{portdata_selectupdated}->execute();

	while (my $port = $sths{portdata_selectupdated}->fetchrow_hashref) {
		if (!$maintainer || lc $maintainer ne lc $port->{maintainer}) {
			info(0, "$maintainer, $count outdated port(s).")
			    if ($maintainer);
			$maintainer = $port->{maintainer};
			$count = 0;
			info(1, "\n${maintainer}'s ports:");
		}
		$count++;
		info(1, "$port->{fullpkgpath}: "
		    . "$port->{ver} -> $port->{newver}");
	}
	info(0, "$maintainer, $count outdated port(s).") if ($maintainer);

	finish_sql($dbh, \%sths);
	$dbh->disconnect;

	return 1;
}


#------------------------------------------------------------------------------
# Func: AddMailAddrs()
# Desc: Add e-mail address(es) to the opt-in results mail database.
#
# Args: @addrs   - List of addresses.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub AddMailAddrs
{
	my (%sths, $dbh);
	my (@addrs) = @_;

	$dbh = connect_db();

	prepare_sql($dbh, \%sths, 'maildata_exists', 'maildata_insert');

	while (my $addr = shift @addrs) {
		my ($exists);

		$addr =~ s/\s+//g;

		print "Adding $addr... ";

		$sths{maildata_exists}->execute($addr);
		($exists) = $sths{maildata_exists}->fetchrow_array;

		$sths{maildata_insert}->execute($addr)
			if (!$exists && !$settings{precious_data});

		print !$exists ? 'OK.\n' : 'already in database.\n';

		$sths{maildata_exists}->finish;
	}

	$sths{maildata_insert}->finish;
	$dbh->disconnect;

	return 1;
}


#------------------------------------------------------------------------------
# Func: RemoveMailAddrs()
# Desc: Remove e-mail address(es) from the opt-in results mail database.
#
# Args: @addrs   - List of addresses.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub RemoveMailAddrs
{
	my (%sths, $dbh);
	my (@addrs) = @_;

	$dbh = connect_db();

	prepare_sql($dbh, \%sths, 'maildata_delete');

	while (my $addr = shift @addrs) {
		my $rows;

		$addr =~ s/\s+//g;

		print "Removing $addr... ";

		if (!$settings{precious_data}) {
			$sths{maildata_delete}->execute($addr);

			$rows = $sths{maildata_delete}->rows;
			print $rows ? 'OK.\n' : 'not in database.\n';
		} else {
			print 'not allowed (precious_data).\n';
		}
	}

	$sths{maildata_delete}->finish;
	$dbh->disconnect;

	return 1;
}


#------------------------------------------------------------------------------
# Func: ShowMailAddrs()
# Desc: List e-mail address(es) currently in the results mail database.
#
# Args: n/a
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub ShowMailAddrs
{
	my (%sths, $dbh);

	$dbh = connect_db();

	prepare_sql($dbh, \%sths, 'maildata_select');
	$sths{maildata_select}->execute();

	print "Currently subscribed addresses:\n";

	while (my ($addr) = $sths{maildata_select}->fetchrow_array) {
		print "  $addr\n";
	}

	$sths{maildata_select}->finish;
	$dbh->disconnect;

	return 1;
}

#------------------------------------------------------------------------------
# Func: Prune()
# Desc: Prune the database from removed ports
#
# Args: $time - also remove port older than ($time - 1 day)
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub Prune
{
    my $sdbh = shift;
    my $time = shift;
    my (%sths, $dbh, %ssths, $ssth, $sth, $prune);

    $dbh = connect_db();
    prepare_sql($dbh,  \%sths,  qw( portdata_fullpkgpaths delete_removed portdata_outdate ));
    prepare_sql($sdbh, \%ssths, qw( sqlports_check_fullpkgpath ));

    print "-- [ Pruning removed ports ] -------------------------------------------\n";

    $sths{portdata_fullpkgpaths}->execute() or die $DBI::errstr;

    # Go through all our pkgpaths, and remove anything which cannot be found in SQLports
    $prune = 0;
    while (my $port = $sths{portdata_fullpkgpaths}->fetchrow_hashref) {
	$ssths{sqlports_check_fullpkgpath}->execute($port->{fullpkgpath});
        unless (my ($match) = $ssths{sqlports_check_fullpkgpath}->fetchrow_array) {
            if ($settings{precious_data}) {
                info(0, $port->{fullpkgpath}, 'not removed (precious_data)');
                next;
            }
            $prune++;
            $sths{delete_removed}->execute($port->{id});
            info(0, $port->{fullpkgpath}, 'removed');
        }
    }
    print "Prune done, $prune removed.\n";
    unless ($time) {
        $dbh->disconnect;
        return 1;
    }

    # NOTE: this feature is intended to clean garbage from past build bugs
    $sths{portdata_outdate}->execute($time) or die $DBI::errstr;
    my $port = $sths{portdata_outdate}->fetchrow_hashref;
    unless ($port) {
        $dbh->disconnect;
        return 1;
    }

    print "-- [ Pruning outdated ports ] ------------------------------------------\n";

    $prune = 0;
    while (1) {
        if ($settings{precious_data}) {
            info(0, $port->{fullpkgpath}, 'not removed (precious_data)');
            next;
        }
        regress($port->{fullpkgpath}, "outdated, updated " . $port->{updated});
        next if (!$settings{regress});
        $prune++;
        $sths{delete_removed}->execute($port->{id});
    } continue { last unless $port = $sths{portdata_outdate}->fetchrow_hashref};

    print "Prune done, $prune removed.\n";
    $dbh->disconnect;
    return 1;
}

#------------------------------------------------------------------------------
# Func: SwitchUser()
# Desc: Drop root privileges, switching to another user (if configured to).
#
# Args: n/a
#
# Retn: n/a
#------------------------------------------------------------------------------

sub SwitchUser
{
	if ($settings{group} && getgid() == 0) {
		my $gid = getgrnam($settings{group})
			or die "Couldn't determine GID from name $settings{group}\n";
		setgid($gid)
			or die "Couldn't switch to group $settings{group}";
	}

	if ($settings{user} && getuid() == 0) {
		my $uid = getpwnam($settings{user})
			or die "Couldn't determine UID from name $settings{user}\n";
		setuid($uid)
			or die "Couldn't switch to user $settings{user}";
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: Usage()
# Desc: Print usage message and exit.
#
# Args: n/a
#
# Retn: n/a
#------------------------------------------------------------------------------

sub Usage
{
	my $s = basename($0);

	print STDERR "Usage: \n";
	print STDERR "       $s build\n";
	print STDERR "       $s rebuild\n";
	print STDERR "       $s check\n";
	print STDERR "       $s uncheck\n";
	print STDERR "       $s prune\n";
	print STDERR "\n";
	print STDERR "       $s mail\n";
	print STDERR "       $s generate\n";
	print STDERR "       $s showupdates\n";
	print STDERR "\n";
	print STDERR "       $s add-mail user\@host ...\n";
	print STDERR "       $s remove-mail user\@host ...\n";
	print STDERR "       $s show-mail\n";
	exit 1;
}
