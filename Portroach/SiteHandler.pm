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

package Portroach::SiteHandler;

use Portroach::SiteHandler::Bitbucket;
use Portroach::SiteHandler::CPAN;
use Portroach::SiteHandler::GitHub;
use Portroach::SiteHandler::Go;
use Portroach::SiteHandler::Hackage;
use Portroach::SiteHandler::Launchpad;
use Portroach::SiteHandler::Mozilla;
use Portroach::SiteHandler::NPM;
use Portroach::SiteHandler::Pear;
use Portroach::SiteHandler::Pecl;
use Portroach::SiteHandler::PyPI;
use Portroach::SiteHandler::RubyGems;
use Portroach::SiteHandler::SourceForge;

use strict;

require 5.006;


#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

our @sitehandlers;


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

	bless ($self, $class);
	return $self;
}


#------------------------------------------------------------------------------
# Func: FindHandler()
# Desc: Iterate over known handlers to find one, if any, that can handle the
#       given site.
#
# Args: $url - A URL we want to "handle".
#
# Retn: $sitehandler or undef
#------------------------------------------------------------------------------

sub FindHandler
{
	my $self = shift;

	my ($url) = @_;

	foreach my $class (@sitehandlers) {
		return $class->new if $class->CanHandle($url);
	}

	return undef;
}


#------------------------------------------------------------------------------
# Func: GetName()
# Desc: By default return undef, sub for inheritance.
#
# Args: $url - A URL we want to extract name from.
#
# Retn: undef
#------------------------------------------------------------------------------

sub GetName
{
	return undef;
}


#------------------------------------------------------------------------------
# Func: FindName()
# Desc: Iterate over known handlers to find one, if any, that handle the given
#       site and return GetName()
#
# Args: $url - A URL we want to extract name from.
#
# Retn: $version or undef
#------------------------------------------------------------------------------

sub FindName
{
	my $self = shift;

	my ($url) = @_;

	my $name;

	foreach my $class (@sitehandlers) {
		return $name if ($name = $class->GetName($url));
	}

	return undef;
}


#------------------------------------------------------------------------------
# Func: GetVersion()
# Desc: By default return undef, sub for inheritance.
#
# Args: $url - A URL we want to extract version from.
#
# Retn: undef
#------------------------------------------------------------------------------

sub GetVersion
{
	return undef;
}


#------------------------------------------------------------------------------
# Func: FindVersion()
# Desc: Iterate over known handlers to find one, if any, that handle the given
#       site and return GetVersion()
#
# Args: $url - A URL we want to extract version from.
#
# Retn: $version or undef
#------------------------------------------------------------------------------

sub FindVersion
{
	my $self = shift;

	my ($url) = @_;

	my $ver;

	foreach my $class (@sitehandlers) {
		return $ver if ($ver = $class->GetVersion($url));
	}

	return undef;
}

1;
