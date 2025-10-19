#------------------------------------------------------------------------------
# Copyright (C) 2015,2020 Jasper Lievisse Adriaanse <jasper@openbsd.org>
# Copyright (C) 2025 Fabien Romano <fabien@openbsd.org>
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
use base Portroach::SiteHandler;

use JSON qw(decode_json);

use Portroach::Util;

use strict;

require 5.006;


#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

push @Portroach::SiteHandler::sitehandlers, __PACKAGE__;

my $bitbucket_re = qr/https?\:\/\/bitbucket\.org/;

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

	return ($url =~ /^$bitbucket_re/);
}


#------------------------------------------------------------------------------
# Func: GetName()
# Desc: Return name or undef.
#
# Args: $url - A URL we want to extract name from.
#
# Retn: undef
#------------------------------------------------------------------------------

sub GetName
{
	my $self = shift;

	my ($url) = @_;

	if ($url =~ /^$bitbucket_re\/([^\/]+)\/([^\/]+)(\/|$)/) {
		return "$1/$2";
	} else {
		return undef;
	}
}


#------------------------------------------------------------------------------
# Func: GetFiles()
# Desc: Extract a list of files from the given URL. Query the bitbucket API
#       for available downloads of a given module.
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

	my ($api, $projname, $account, $repo, $resp, $query, $ua);
	$api = 'https://api.bitbucket.org/2.0/repositories/';

	$projname = $self->GetName($url);
	unless ($projname) {
		print STDERR "$port->{fullpkgpath}: $url, "
		    . "no projname found in url\n";
		return 0;
	}
	($account, $repo) = split('/', lc $projname);

	$query = $api . $account . '/' . $repo . '/downloads';

	debug(__PACKAGE__, $port, "GET $query");
	$ua = $ua = lwp_useragent();
	$resp = $ua->request(HTTP::Request->new(GET => $query));

	if ($resp->is_success) {
		my $downloads = decode_json($resp->decoded_content);

		foreach my $dl ($downloads->{values}[0]) {
			push(@$files, $dl->{name});
		}
	} else {
		info(1, $port->{fullpkgpath}, strchop($query, 60)
		    . ': ' . $resp->status_line);
	}

	# Extract versions from git tags
	$query = "https://bitbucket.org/$projname.git";
	$url = "https://bitbucket.org/$projname/get/";
	return 0 if (extractgit($port, $query, $url, $files) < 0);

	return 1;
}

1;
