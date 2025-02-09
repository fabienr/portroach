#------------------------------------------------------------------------------
# Copyright (C) 2014, 2020 Jasper Lievisse Adriaanse <jasper@openbsd.org>
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

package Portroach::SiteHandler::GitHub;
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

my $github_re = qr/https?\:\/\/github\.com/;

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

	$self->{name} = 'GitHub';

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

	return ($url =~ /^$github_re/);
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

	if ($url =~ /^$github_re\/downloads\/(.*)\//) {
		return $1;
	} elsif ($url =~ /^$github_re\/([^\/]+)\/([^\/]+)\//) {
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
# Retn: undef
#------------------------------------------------------------------------------

sub GetVersion
{
	my $self = shift;

	my ($ver) = @_;

	my $projname = $self->GetName($ver);
	return undef unless ($projname);

	debug(__PACKAGE__, undef, "find $projname");
	my ($account, $repo) = split('/', $projname);

	return undef if (
	    $ver !~ s:^$github_re/.*?/archive/refs/tags/(release/)?(.*)/.*?$:$2:
	    && $ver !~ s:^$github_re/.*?/releases/download/(.*?)/.*$:$1:);
	$ver =~ s/^($account|$repo)[\-\_]?//g;
	$ver =~ s/\//\+/g;
	return $ver;
}

#------------------------------------------------------------------------------
# Func: GetFiles()
# Desc: Extract a list of files from the given URL. In the case of GitHub,
#       we are actually pulling the files from the project's Atom feed and
#       extract the release url, containing the tag it was based on.
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
	my (@tags, $projname, $query, $ua, $req, $resp, $json, $ver);

	# XXX check if $projname =~ /.*?\/(.*)/ ?
	$projname = $self->GetName($url);
	unless ($projname) {
		print STDERR "$port->{fullpkgpath}: $url, "
		    . "no projname found in url\n";
		return 0;
	}

	# First check if there's a latest releases endpoint
	$query = 'https://api.github.com/repos/'.$projname.'/releases/latest';
	debug(__PACKAGE__, $port, "GET $query");
	$ua = lwp_useragent();

	if ($settings{github_token}) {
		my $auth_header = HTTP::Headers->new (
		    'Authorization' => "Token $settings{github_token}");
		$req = HTTP::Request->new(GET => $query, $auth_header);
	} else {
		$req = HTTP::Request->new(GET => $query);
	}

	$resp = $ua->request($req);

	if ($resp->is_success) {
		$json = decode_json($resp->decoded_content);

		# Obtain the assets associated with the latest release
		# XXX maybe drop this: what matter is version detection thus
		#     we don't care if the new url isn't pointing to an asset
		foreach my $asset (@{$json->{assets}}) {
			if ($asset->{name} !~ m/$port->{sufx}$/) {
				debug(__PACKAGE__, $port,
				    "$asset->{name} !~ $port->{sufx}");
				next;
			}
			push(@$files, $asset->{browser_download_url});
		}

		# Anyway push the tag_name as version
		# XXX tmp fix, in case tags got messy and older pop first
		push(@tags, $json->{tag_name});

	} else {
		if ($resp->header('x-ratelimit-remaining') == 0) {
			print STDERR "$port->{fullpkgpath}: "
			    . "API rate limit exceeded, "
			    . "please set 'github token' in portroach.conf\n";
			return 0;
		}
		info(1, $port->{fullpkgpath}, strchop($query, 60)
		    . ': ' . $resp->status_line);
	}

	# Project may not do releases (any more), so let's load tags anyway.
	# API /tags are sorted based on git output (alphabetical) and returns
	# only top X tags. GraphQL looks harder to use. Then, use git directly.
	my $gitcmd = "GIT_TERMINAL_PROMPT=0 /usr/local/bin/git".
	    " -c 'versionsort.suffix=-' ls-remote".
	    " --tags --sort='v:refname'";
	my $giturl = "https://github.com/$projname.git";
	debug(__PACKAGE__, $port, "$gitcmd $giturl");
	my ($out, $err, $rc) = capture {
		system("$gitcmd $giturl");
	};
	if ($rc != 0 or $err) {
		$err .= '\n' if ($err !~ /\n$/g);
		info(1, $port->{fullpkgpath}, strchop($giturl, 60)
		    . ': ' . "$rc, $err");
		return 0;
	}
	foreach my $output (reverse(split /^/m, $out)) {
		my ($commit, $tag) = split(/\s+/, $output);
		next if ($tag =~ /\^\{\}$/);
		$tag =~ s/refs\/tags\///;
		debug(__PACKAGE__, $port, "$tag");
		push(@tags, $tag);
	}

	# In some cases the project name (read: repo) is part of the tagname.
	# For example: 'heimdal-7.3.0' is the full tagname. Therefore remove the
	# repository name from the filename just in case.
	my ($account, $repo) = split('/', lc $projname);
	$url = "https://github.com/$projname/archive/refs/tags/";

	foreach my $tag (@tags) {
		my $ver = lc $tag;

		debug(__PACKAGE__, $port, "trim projname -> $ver")
		    if ($ver =~ s/($account|$repo)[\-\_]?//);
		debug(__PACKAGE__, $port, "trim (v|r) marker -> $ver")
		    if ($ver =~ s/^$verprfx_regex//);
		debug(__PACKAGE__, $port, "trim \\D SEP -> $ver")
		    if ($ver =~ s/^\D*[\.\-\_]//);
		debug(__PACKAGE__, $port, "replace / by + -> $ver")
		    if ($ver =~ s/\//\+/g);

		debug(__PACKAGE__, $port, "tag $tag -> $ver")
		    if ($tag ne $ver);
		debug(__PACKAGE__, $port, "stop loading tag, old found $ver")
		    if ($ver eq $port->{ver});

		$ver = "$url$tag$port->{sufx}%%$ver";
		push(@$files, $ver);

		# XXX tmp fix, need graphql to order tag correclty
		$tag = quotemeta $port->{ver};
		last if ($ver =~ /$tag/);
	}

	return 1;
}

1;
