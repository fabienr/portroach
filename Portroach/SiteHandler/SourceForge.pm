#------------------------------------------------------------------------------
# Copyright (C) 2010, Shaun Amott <shaun@inerd.com>
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

package Portroach::SiteHandler::SourceForge;
use base Portroach::SiteHandler;

use XML::Feed;

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

	$self->{name} = 'SourceForge';

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

	return ($url =~ /^https?:\/\/.*\.sourceforge\.net/);
}


#------------------------------------------------------------------------------
# Func: GetName()
# Desc: By default return undef, sub for inheritance.
#
# Args: $url - A URL we want to extract version from.
#
# Retn: undef
#------------------------------------------------------------------------------

sub GetName
{
	my $self = shift;

	my ($ver) = @_;

	if ($ver =~ m:^https?\://.*\.sourceforge\.net/sourceforge/(.*?)\/:) {
		return $1;
	} else {
		return undef;
	}
}


#------------------------------------------------------------------------------
# Func: GetFiles()
# Desc: Extract a list of files from the given URL. In the case of SourceForge,
#       we are actually pulling the files from an RSS feed helpfully provided
#       for each "project".
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

	my ($query, $projname, $ua, $resp, $xpath, $items);

	my $q = 'downloads\.sourceforge\.net\/(sourceforge|project)\/';
	if ($url =~ /$q([^\/]*)\//) {
		$projname = $2;
	} elsif ($url =~ /^https?:\/\/(.*)\.sourceforge\.net\//) {
		$projname = $1;
	} else {
		print STDERR "$port->{fullpkgpath}: $url, "
		    . "no projname found in url\n";
		return 0;
	}

	# Find the RSS feed for this project.
	$query = "http://sourceforge.net/projects/$projname/rss?limit=1000";

	debug(__PACKAGE__, $port, "GET $query");
	$ua = lwp_useragent();
	$resp = $ua->get($query);

	if (!$resp->is_success || $resp->status_line !~ /^2/) {
		info(1, $port->{fullpkgpath}, strchop($query, 60)
		    . ': ' . $resp->status_line);
		return 0;
	}

	my $feed = XML::Feed->parse(\$resp->content);
	unless ($feed) {
		info(1, $port->{fullpkgpath}, strchop($query, 60)
		    . ': invalid feed, ' . XML::Feed->errstr);
		return 0;
	}
	unless ($feed->entries) {
		info(1, $port->{fullpkgpath}, strchop($query, 60)
		    . ': invalid feed, no entries');
		return 0;
	}

	foreach my $item ($feed->entries) {
		my ($file, $url);

		$file = "/project/$projname" . $item->title;
		$url = $item->link;

		next if ($url =~ /\/$/);

		# Note this file.
		push @$files, $file;
	}

	return 1;
}

1;
