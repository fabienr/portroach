#------------------------------------------------------------------------------
# Copyright (C) 2015,2020 Jasper Lievisse Adriaanse <jasper@openbsd.org>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
#------------------------------------------------------------------------------

package Portroach::SiteHandler::HTTP;
use base Portroach::SiteHandler;

use Portroach::Util;
use Portroach::Config;

use strict;

require 5.006;


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
	my $self      = {};
	my $class     = shift;

	$self->{name} = 'HTTP';

	bless ($self, $class);
	return $self;
}


#------------------------------------------------------------------------------
# Func: CanHandle()
# Desc: Ask if this handler (package) can handle the given site.
#
# Args: $url - URL of site.
#
# Retn: $res - true/false.
#------------------------------------------------------------------------------

sub CanHandle
{
	my $self = shift;

	my ($url) = @_;

	return ($url =~ /^https?:\/\//i);
}


#------------------------------------------------------------------------------
# Func: GetFiles()
# Desc: Extract a list of files from the given URL. Simply query the API.
#
# Args: $url     - URL we would normally fetch from.
#       \%port   - Port hash fetched from database.
#       \@files  - Array to put files into.
#
# Retn: $success - False if file list could not be constructed; else, true.
#------------------------------------------------------------------------------

sub GetFiles
{
	my $self = shift;

	my ($url, $port, $files, $path_ver) = @_;

	my ($ua, $resp, $host, $rc, $link, @dirs, $dir, @tmp, $site, $path,
	    $path_ver_q, $root_resp, $depth_limit);

	$ua = lwp_useragent();
	$resp = $ua->get($url);
	$host = $url->host;
	$host = s/.*\.([^\.]*?\.[^\.])$/$1/; # check only for top domain
	$rc = 1; # return success by default

	if ($resp->is_success) {
		my @tmp;
		extractfilenames($resp->content, $port->{sufx}, \@tmp);
		foreach $link (@tmp) {
			if ($link =~ /^https?:\/\// && $link !~ /$host/) {
				debug(__PACKAGE__,$port,"skip $link !~ $host");
				next;
			}
			debug(__PACKAGE__, $port, "push $link");
			push @$files, $link;
		}
		info(1, $port->{fullpkgpath}, $url->host,
		    "no link mathing $port->{sufx}")
		    if (!@$files);
	} else {
		# A 404 here ought to imply that the distfile
		# is unavailable, since we expect it to be
		# inside this directory. However, some sites
		# use scripts or rewrite rules disguised as
		# directories.
		info(1, $port->{fullpkgpath}, strchop($url, 60)
		    . ': ' . $resp->status_line);
		$rc = 0;
	}

	# XXX indexsite from Makefile may prevent path_ver detection
	# return 1 unless($path_ver);

	# Directory listing is a success: investigate $path_ver variations...

	$site = $url->clone;
	$path = $site->path;
	if ($path_ver) {
		$path_ver_q = quotemeta $path_ver;
		$path =~ s/($verprfx_regex)?$path_ver_q\/.*//;
		$site->path($path);
		debug(__PACKAGE__, $port, "switch to $path");
	}

	$resp = $ua->get($site);

	if ($resp->is_success) {
		$root_resp = $resp;
		extractdirectories($resp->content, \@dirs);
		debug(__PACKAGE__, $port, "no dirs, $site") if (!@dirs);
	} else {
		info(1, $port->{fullpkgpath}, strchop($site, 60)
		    . ': ' . $resp->status_line);
		return $rc;
	}

	# Investigate sibling version matches
	my $found = 0;
	foreach $dir (@dirs) {
		my (@tmp, $dir_v, $dir_maj, $new_path);

		last unless $path_ver_q; # skip without version
		debug(__PACKAGE__, $port, "sibling, $dir");

		# Check version, filter bad matches early
		$dir_v = $dir;
		$dir_v =~ s:^(.*/)?($verprfx_regex)?($verlike_regex)/.*$:$3:i;
		if (!isversion($dir_v, $port->{ver})) {
			debug(__PACKAGE__, $port, "$dir_v !~ $port->{ver}, "
			    . "skip dir $dir");
			next;
		}
		# Check version pattern for major number. If found and the new
		# version isn't greater, check if major number match.
		# Otherwise, skip the path.
		$dir_maj = $1 if ($dir_v =~ /(\d+)\..*/);
		if ( length $dir_maj && !vercompare($dir_v, $port->{ver}) &&
		    $port->{ver} !~ /^$dir_maj/) {
			debug(__PACKAGE__, $port,
			    "$dir_v < $port->{ver} !~ /^$dir_maj/, "
			    . "skip version $dir");
			next;
		}

		# Check same site with alternative path
		$new_path = $url->path;
		$new_path =~ s/$path_ver_q/$dir_v/;
		$site->path($new_path);

		$resp = $ua->get($site);
		if ($resp->is_success) {
			extractfilenames($resp->content, $port->{sufx}, \@tmp);
			debug(__PACKAGE__, $port, "no files, $site") if (!@tmp);
		} else {
			# As we previously got an answer, this is not a failure
			info(1, $port->{fullpkgpath}, strchop($site, 60)
			    . ': ' . $resp->status_line);
		}
		foreach my $link (@tmp) {
			if ($link =~ /^https?:\/\// && $link !~ /$host/) {
				debug(__PACKAGE__,$port,"skip $link !~ $host");
				next;
			} elsif ($link !~ /^(https?:\/\/|\/)/) {
				$link = $site->path . "$link";
			}
			debug(__PACKAGE__, $port, "push $link");
			$found = 1;
			push @$files, "$link";
		}
	}

	return 1 if($found);

	# No files found, crawl down the path
	undef @dirs;

	$site->path($path);
	$depth_limit = $path =~ tr/\///;
	$depth_limit += 5; # XXX random guess, five should be enough
	extractsubdirectories($root_resp, \@dirs, $site);
	debug(__PACKAGE__, $port, "no subdir, $site") if (!@dirs);

	while ($dir = shift(@dirs)) {
		my @tmp;

		if ($dir =~ tr/\/// > $depth_limit) {
			debug(__PACKAGE__, $port,
			    "skip $dir, depth > $depth_limit");
			next;
		}
		debug(__PACKAGE__, $port, "subdir, $dir");

		$site->path($dir);
		$resp = $ua->get($site);
		if ($resp->is_success) {
			extractfilenames($resp->content, $port->{sufx}, \@tmp);
			debug(__PACKAGE__, $port, "no files, $site") if (!@tmp);
		} else {
			# As we previously got an answer, this is not a failure
			info(1, $port->{fullpkgpath}, strchop($site, 60)
			    . ': ' . $resp->status_line);
			next;
		}

		foreach my $link (@tmp) {
			if ($link =~ /^https?:\/\// && $link !~ /$host/) {
				debug(__PACKAGE__,$port,"skip $link !~ $host");
				next;
			} elsif ($link !~ /^(https?:\/\/|\/)/) {
				$link = $site->path . "$link";
			}
			debug(__PACKAGE__, $port, "push $link");
			push @$files, "$link";
		}

		# ... continue crawling ...
		undef @tmp;
		extractsubdirectories($resp, \@tmp, $site);
		debug(__PACKAGE__, $port, "no subdir, $site") if (!@tmp);
		foreach my $link (@tmp) {
			next if (grep { $_ eq $link } @dirs);
			debug(__PACKAGE__, $port, "DIR $link");
			push @dirs, "$link";
		}
	}

	return $rc;
}

1;
