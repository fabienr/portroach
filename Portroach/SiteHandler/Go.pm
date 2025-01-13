#------------------------------------------------------------------------------
# Copyright (C) 2021, Aaron Bieber <abieber@openbsd.org>
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

package Portroach::SiteHandler::Go;

use JSON qw(decode_json);

use Portroach::Util;

use strict;

require 5.006;


#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

push @Portroach::SiteHandler::sitehandlers, __PACKAGE__;


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

	$self->{name} = 'Go';

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

	return ($url =~ /proxy\.golang\.org/);
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

	my ($url, $port, $files) = @_;

	my ($registry, $dist, $resp, $query, $ua);
	$registry = 'https://proxy.golang.org/';

	if ($port->{distname} =~ /^(.*)-(v)?\d+\.\d+.\d+(.*)?$/) {
	    $dist = $1;
	}

	$query = $url;
	$query =~ s/\@v\//\@latest/;

	debug(__PACKAGE__, $port, "GET $query");
	$ua = lwp_useragent();
	$resp = $ua->request(HTTP::Request->new(GET => $query));

	if ($resp->is_success) {
	    my ($json, $version);

	    $json = decode_json($resp->decoded_content);
	    $version = $json->{Version};
	    unless ($version) {
	        print STDERR "$port->{fullpkgpath}: $query, "
		    . "invalid json, no Version\n";
	        return 0;
	    }

	    $port->{newver} = $version;
	    $version =~ s/v//;

	    push(@$files, "$dist-${version}.zip");
	} else {
	    info(1, $port->{fullpkgpath}, strchop($query, 60)
	        . ': ' . $resp->status_line);
	    return 0;
	}

	return 1;
}

1;
