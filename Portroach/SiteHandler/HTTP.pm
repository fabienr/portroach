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

	my ($ua, $resp, @dirs, $path, $dir, @tmp);

	# A 404 here ought to imply that the distfile
	# is unavailable, since we expect it to be
	# inside this directory. However, some sites
	# use scripts or rewrite rules disguised as
	# directories.

	$ua = lwp_useragent();
	$resp = $ua->get($url);

	if ($resp->is_success) {
		extractfilenames($resp->content, $port->{sufx}, \@$files);
		info(1, $port->{fullpkgpath}, $url->host,
		    "no link mathing $port->{sufx}")
		    if (!@$files);
	} else {
		info(1, $port->{fullpkgpath}, strchop($url, 60)
		    . ': ' . $resp->status_line);
		return 0;
	}

	return 1 unless($path_ver);

	# Directory listing a success: we can
	# investigate $path_ver variations...

	# Visit parent directory

	uri_lastdir($url, undef);
	$path = $url->path;

	$resp = $ua->get($url);

	if ($resp->is_success) {
		extractdirectories($resp->content, \@dirs);
	} else {
		# As we previously got an answer, this is not a failure
		info(1, $port->{fullpkgpath}, strchop($url, 60)
		    . ': ' . $resp->status_line);
		# XXX maybe return code depends on !@$files ?
		return 1; # success
	}

	# Investigate sibling version dirs
	foreach $dir (@dirs) {
		next unless ($dir =~ /^(?:\d+\.)+\d+$/ ||
		    $dir =~ /$date_regex/i);

		$url->path("$path$dir");
		$resp = $ua->get($url);
		if (!$resp->is_success) {
			# As we previously got an answer, this is not a failure
			info(1, $port->{fullpkgpath}, strchop($url, 60)
			    . ': ' . $resp->status_line);
			next;
		}

		extractfilenames($resp->content, $port->{sufx}, \@tmp);
		foreach (@tmp) {
			debug(__PACKAGE__, $port, "push $path$dir/$_");
			push @$files, "$path$dir/$_";
		}
	}
	return 1;
}

1;
