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

package Portroach::SiteHandler::HttpGuess;

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

	$self->{name} = 'Guess';

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

	my ($url, $port, $files, $path_ver, $curr_v) = @_;

	my ($ua, $resp, $old_v, $sufx, $guess_v, $distfile, %headers);
	my $bad_mimetypes = 'html|text|css|pdf|jpeg|gif|png|image|mpeg|bitmap';

	$ua = lwp_useragent();

	$old_v = quotemeta $port->{ver};
	$sufx = quotemeta $port->{sufx};

	# XXX $port->{newver} ? $port->{newver} : $port->{ver}
	foreach (verguess($curr_v, $port->{limitwhich})) {
		$guess_v = $_;

		# Only change major version if port isn't
		# version-specific

		if ($port->{limitver}) {
			next unless ($guess_v =~ /$port->{limitver}/);
		} elsif ($port->{name} =~ /^(.*\D)(\d{1,3})(?:[-_]\D+)?$/) {
			my $nm_nums = $2;
			my $vr_nums = $guess_v;
			my $vo_nums = $old_v;

			unless (($1.$2) =~
			    /(?:md5|bz2|bzip2|rc4|rc5|ipv6|mp3|utf8)$/i) {
				my $fullver = "";
				while ($vo_nums =~ s/^(\d+?)[-_\.]?//) {
					$fullver .= $1;
					last if ($fullver eq $nm_nums);
				}

				if ($fullver eq $nm_nums) {
					$vr_nums =~ s/[-_\.]//g;
					next unless ($vr_nums =~ /^$nm_nums/);
				}
			}
		}

		if ($port->{skipversions}) {
			my @skipvers = split /\s+/, $port->{skipversions};
			next if (arrexists(\@skipvers, $guess_v));
		}

		info(1, $port->{fullpkgpath}, $url->host,
		    "Guessing version $port->{ver} -> $guess_v");

		my $site = $url->clone;
		$distfile = $port->{distfiles};
		next unless ($distfile =~ s/$old_v/$guess_v/gi);

		if ($path_ver) {
			my ($path);
			uri_lastdir($site, undef);
			$path = $site->path;
			if ($path_ver ne $port->{ver}) {
				# Major ver in site path
				my $guess_maj = $guess_v;
				$guess_maj =~ s/\.\d+$//;
				$site->path("$path$guess_maj/");
			} else {
				# Full ver in site path
				$site->path("$path$guess_v/");
			}
		}

		$resp = $ua->head($site.$distfile);
		%headers  = %{$resp->headers};

		if ($resp->is_success && $resp->status_line =~ /^2/ &&
		    $headers{'content-type'} !~ /($bad_mimetypes)/i) {
			debug(__PACKAGE__, $port, "push $site$distfile");
			push @$files, "$site$distfile";
			last;
		} elsif (!$resp->is_success) {
			info(1, $port->{fullpkgpath},
			    strchop($site.$distfile, 60)
			    . ': ' . $resp->status_line);
		} else {
			info(1, $port->{fullpkgpath}, $site->host,
			    "Guess failed $port->{ver} -> $guess_v");
		}
	}
	return 1;
}

1;
