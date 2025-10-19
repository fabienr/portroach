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

package Portroach::SiteHandler::HttpHomepage;
use base Portroach::SiteHandler;

use Portroach::Util;
use Portroach::Config;
use Portroach::Const;

use List::Util qw(first);

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

	$self->{name} = 'Homepage';

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
# Desc: Extract a list of files from the given URL.
#
# Args: $url      - URL we would normally fetch from.
#       \%port    - Port hash fetched from database.
#       \@files   - Array to put files into.
#       \@checked - Array of site/handler already processed.
#
# Retn: $success - False if file list could not be constructed; else, true.
#------------------------------------------------------------------------------

sub GetFiles
{
	my $self = shift;

	my ($url, $port, $files, $checked) = @_;

	my ($ua, $resp, $host, $link, $file, $rc, @pages, @handlers, $name_q);

	$ua = lwp_useragent();
	$resp = $ua->get($url);
	$host = $url->host;
	$host = s/.*\.([^\.]*?\.[^\.])$/$1/; # check only for top domain
	$rc = 1; # return success by default

	if ($resp->is_success) {
		my (@tmp, $sh);
		# check if homepage redirect to a site handler
		$sh = Portroach::SiteHandler->FindHandler($resp->base);
		if ($sh) {
			$sh->{site} = $resp->base;
			push @handlers, $sh;
		} else {
			$resp->content($resp->content =~ s/\R/ /gr);
			$resp->content($resp->content =~ s/\s+/ /gr);
			extractfilenames($resp, $port->{sufx}, \@tmp);
			info(1, $port->{fullpkgpath}, $url,
			    "no $port->{sufx} file") if (!@tmp);
			extractpages($resp, \@pages);
			info(1, $port->{fullpkgpath}, $url,
			    "no pages found") if (!@pages);
			extracthandlers($resp->content, $port, \@handlers);
			info(1, $port->{fullpkgpath}, $url,
			    "no handlers found") if (!@handlers);
		}

		foreach $file (@tmp) {
			if ($file =~ /^https?:\/\// && $file !~ /$host/) {
				debug(__PACKAGE__,$port,"skip $file !~ $host");
				next;
			}
			debug(__PACKAGE__, $port, "push $file");
			push @$files, $file;
		}

	} else {
		# Dead homepage
		info(1, $port->{fullpkgpath}, strchop($url, 60)
		    . ': ' . $resp->status_line);
		return 0;
	}

	foreach $link (@pages) {
		my $site;
		if ($link =~ /^.*?:\/\//) {
			$site = $link;
		} else {
			$site = $url->clone;
			$site->path($link);
		}
		debug(__PACKAGE__,$port,"extract from $site");
		$resp = $ua->get($site);
		if ($resp->is_success) {
			my @tmp;
			$resp->content($resp->content =~ s/\R/ /gr);
			$resp->content($resp->content =~ s/\s+/ /gr);
			extractfilenames($resp, $port->{sufx}, \@tmp);
			info(1, $port->{fullpkgpath}, $site,
			    "no $port->{sufx} file") if (!@tmp);
			foreach $file (@tmp) {
				if ($file =~ /^https?:\/\// &&
				    $file !~ /$host/) {
					debug(__PACKAGE__, $port,
					    "skip $file !~ $host");
					next;
				}
				debug(__PACKAGE__, $port, "push $file");
				push @$files, $file;
			}

			extracthandlers($resp->content, $port, \@handlers);
			info(1, $port->{fullpkgpath}, $site,
			    "no handlers found") if (!@handlers);
		} else {
			info(1, $port->{fullpkgpath}, strchop($site, 60)
			    . ': ' . $resp->status_line);
		}
	}

	# Sort @handlers by longest match, skip sibling repo
	$name_q = porttoregex($port, []); # XXX strict minimum
	@handlers = sort {
		my ($aa, $bb);
		while($a->{site} =~ m/($name_q)/ig ) {
			$aa = length $1 if (length $1 > $aa);
		}
		while($b->{site} =~ m/($name_q)/ig ) {
			$bb = length $1 if (length $1 > $bb);
		}
		$bb <=> $aa;
	    } @handlers;
	foreach my $sh (@handlers) {
		my ($lsh);
		while($sh->{site} =~ m/($name_q)/ig ) {
			$lsh = $1 if (length $1 > length $lsh);
		}
		debug(__PACKAGE__, undef, "match $lsh from $sh->{site}");
		if ( first { $sh->{name} eq $_ } @$checked) {
			debug(__PACKAGE__,undef,"already tried site handler "
			    . "$sh->{name}, skip $sh->{site}");
			next;
		}
		push(@$checked, $sh->{site});
		push(@$checked, $sh->{name});
		if (!$sh->GetFiles($sh->{site}, $port, \@$files)) {
			info(1, $port->{fullpkgpath}, "$sh->{name} GetFiles() "
			    . "failed for $sh->{site}");
			next;
		} else {
			$rc = METHOD_HANDLER if (@$files);
		}
	}

	return $rc;
}

1;
