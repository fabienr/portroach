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

package Portroach::SiteHandler::FTP;

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

	$self->{name} = 'FTP';

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

	return ($url =~ /^ftp:\/\//i);
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

	my ($url, $port, $files, $path_ver) = @_;

	my $ftp = Net::FTP->new(
		$url->host,
		Port    => $url->port,
		Timeout => $settings{ftp_timeout},
		Debug   => $settings{debug},
		Passive => $settings{ftp_passive}
	);

	if (!$ftp) {
		info(1, $port->{fullpkgpath}, $url->host,
		    "FTP connect problem: ".$@);
		return 0;
	}

	my $ftp_failures = 0;
	while ($ftp_failures <= $settings{ftp_retries}) {
		if (!$ftp->login('anonymous')) {
			info(1, $port->{fullpkgpath}, $url->host,
			    'FTP login error: ' . $ftp->message);

			$ftp_failures++;

			if ($ftp->message =~
			    /\b(?:IP|connections|too many|connected)\b/i) {
				my $rest = 2+(int rand 15);
				info(1, $port->{fullpkgpath}, $url->host,
				    "Retrying FTP site in $rest seconds "
				    . "(attempt $ftp_failures of "
				    . "$settings{ftp_retries})"
				);
				sleep $rest;
				next;
			} else {
				last;
			}
		}

		$ftp_failures = 0;
		last;
	}
	if ($ftp_failures) {
		info(1, $port->{fullpkgpath}, $url->host,
		    "FTP error: too many failure($ftp_failures)");
		return 0;
	}

	# This acts as an error check, so we'll cwd to our original directory
	# even if we're not going to look there.
	if (!$ftp->cwd($url->path || '/')) {
		$ftp->quit;
		info(1, $port->{fullpkgpath}, $url->host,
		    'FTP cwd error: ' . $ftp->message);
		return 0;
	}

	@$files = $ftp->ls;

	if (!@$files) {
		info(1, $port->{fullpkgpath}, $url->host,
		    'FTP ls error (or no files found): ' . $ftp->message);
		$ftp->quit;
		return 0;
	}

	# Did we find a version in site path earlier? If so, we'll check the
	# parent directory for other version directories.
	if ($path_ver) {
		my ($path);
		my $site = $url->clone;
		uri_lastdir($site, undef);
		$path = $site->path;

		# Parent directory
		if ($ftp->cwd($site->path)) {
			foreach my $dir ($ftp->ls) {
				# Potential sibling version dirs
				if ($dir =~ /^(?:\d+\.)+\d+$/ ||
				    $dir =~ /$date_regex/i) {
					$site->path("$path$dir");
					if ($ftp->cwd($site->path)) {
						# Potential version files
						push @$files, "$path$dir/$_"
						    foreach ($ftp->ls);
					}
				}
			}
		}
	}

	$ftp->quit;

	return 1;
}

1;
