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

package Portroach::SiteHandler::Bitbucket;

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

	$self->{name} = 'Bitbucket';

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

	# The following two URLs will be handled:
	# http://bitbucket.org/{accountname}/{repo_slug}/get/
	# https://bitbucket.org/{accountname}/{repo_slug}/downloads/
	return ($url =~ /https?:\/\/bitbucket\.org\/.*?(get|downloads)\//);
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

	my ($api, $accountname, $repo_slug, $resp, $query, $ua);
	$api = 'https://api.bitbucket.org/2.0/repositories/';

	if ($url =~ /http(?:s?):\/\/bitbucket\.org\/(.*?)\/(.*?)\/(?:get|downloads)\//) {
	    $accountname = $1;
	    $repo_slug = $2;
	} else {
	    print STDERR "$port->{fullpkgpath}: $url, "
	        . "failed to match accountname/repo_slug\n";
	    return 0;
	}

	$query = $api . $accountname . '/' . $repo_slug . '/downloads';

	debug(__PACKAGE__, $port, "GET $query");
	$ua = $ua = lwp_useragent();
	$resp = $ua->request(HTTP::Request->new(GET => $query));

	if ($resp->is_success) {
	    my $downloads = decode_json($resp->decoded_content);

	    foreach my $dl ($downloads->{values}[0]) {
		    push(@$files, $dl->{name});
	    }
	} else {
	    debug(__PACKAGE__, $port, strchop($query, $60)
	        . ": $resp->status_line");
	    return 0;
	}

	return 1;
}

1;
