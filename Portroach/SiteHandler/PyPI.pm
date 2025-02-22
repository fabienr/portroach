#------------------------------------------------------------------------------
# Copyright (C) 2015, Jasper Lievisse Adriaanse <jasper@openbsd.org>
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

package Portroach::SiteHandler::PyPI;
use base Portroach::SiteHandler;

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

	$self->{name} = 'PyPI';

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

	return ($url =~ /https:\/\/(files\.pythonhosted\.org|pypi\.io)\//);
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

	my ($pypi, $package, $resp, $query, $ua);
	$pypi = 'https://pypi.python.org/pypi/';

	# Strip all the digits at the end to keep the stem of the module.
	if ($port->{distname} =~ /(.*?)-(\d+)/) {
		$package = $1;
	}

	$query = $pypi . $package . '/json';

	debug(__PACKAGE__, $port, "GET $query");
	$ua = lwp_useragent();
	$resp = $ua->request(HTTP::Request->new(GET => $query));
	if ($resp->is_success) {
		my ($json, $urls);

		$json = decode_json($resp->decoded_content);
		$urls = $json->{urls};
		foreach my $url (@$urls) {
			push(@$files, $url->{filename});
		}
		my @releases = ( keys %{$json->{releases}} );
		foreach my $version (@releases) {
			foreach my $file (@{$json->{releases}{$version}}) {
				push(@$files, $file->{url});
			}
		}
	} else {
		info(1, $port->{fullpkgpath}, strchop($query, 60)
		    . ': ' . $resp->status_line);
		return 0;
	}

	return 1;
}

1;
