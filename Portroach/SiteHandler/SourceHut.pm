#------------------------------------------------------------------------------
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

package Portroach::SiteHandler::SourceHut;
use base Portroach::SiteHandler;

use Capture::Tiny 'capture';

use JSON qw(decode_json);
use URI;

use Portroach::Util;
use Portroach::Config;

use strict;

require 5.006;

#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

push @Portroach::SiteHandler::sitehandlers, __PACKAGE__;

our %settings;

my $srht_re = qr/https?\:\/\/(?:git\.)?sr\.ht/;

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

	$self->{name} = 'SourceHut';

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

	return ($url =~ /^$srht_re/);
}


#------------------------------------------------------------------------------
# Func: GetName()
# Desc: Return name or undef.
#
# Args: $url - A URL we want to extract name from.
#
# Retn: $name or undef
#------------------------------------------------------------------------------

sub GetName
{
	my $self = shift;
	my ($url) = @_;

	if ($url =~ /^$srht_re\/([^\/]+)\/([^\/]+)(\/|$)/) {
		return "$1/$2";
	} else {
		return undef;
	}
}


#------------------------------------------------------------------------------
# Func: GetVersion()
# Desc: Return version or undef.
#
# Args: $url - A URL we want to extract name from.
#
# Retn: $ver or undef
#------------------------------------------------------------------------------

sub GetVersion
{
	my $self = shift;
	my ($ver) = @_;

	return undef if (
	    $ver !~ s:^$srht_re/.*?/archive/(.*)$:$2:
	    && $ver !~ s:^$srht_re/.*?/refs/download/(.*?)/.*$:$1:);
	return $ver;
}


#------------------------------------------------------------------------------
# Func: GetFiles()
# Desc: Extract a list of files from the given URL. Pull tags from git command.
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
	my ($projname, $query);

	$projname = $self->GetName($url);
	unless ($projname) {
		print STDERR "$port->{fullpkgpath}: $url, "
		    . "no projname found in url\n";
		return 0;
	}

	# Extract versions from git tags
	$query = "https://git.sr.ht/$projname";
	$url = "https://git.sr.ht/$projname/archive/";
	return 0 if (extractgit($port, $query, $url, $files) < 0);

	return 1;
}

1;
