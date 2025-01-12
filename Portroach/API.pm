#------------------------------------------------------------------------------
# Copyright (C) 2011, Shaun Amott <shaun@inerd.com>
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

package Portroach::API;

use Portroach::Const;
use Portroach::Util;
use Portroach::Config;

require Exporter;

    use Data::Dumper;

use strict;

require 5.006;

our @ISA = qw(Exporter);


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
	my $self  = {};
	my $class = shift;

	$self->{dbh} = connect_db();
	$self->{sths} = {};

	prepare_sql(
		$self->{dbh},
		$self->{sths},
		qw(portdata_exists portdata_update portdata_insert sitedata_exists
		   sitedata_insert portdata_getver portdata_getnewver
		   portdata_clearnewver portconfig_update portconfig_isstatic)
	);

	bless ($self, $class);
	return $self;
}


#------------------------------------------------------------------------------
# Func: AddPort()
# Desc: Add an item of software (port) to the database.
#
# Args: \%port   - Hash containing data:
#                    name        - Port name                         (required)
#                    category    - Category                          (required)
#                    version     - Current port version              (required)
#                    maintainer  - Port maintainer e-mail            (required)
#                    distfiles   - Array of filenames.               (required)
#                    sites       - Array of sites to find files      (required)
#                    distname    - "distname" (as in ports)
#                    suffix      - Distfile suffix (e.g. ".tar.gz")
#                    comment     - Description of port
#                    options     - Hash of port options, from "PORTROACH" var.
#                    pcfg_comment  - Explanation for PORTROACH(PORTROACH_COMMENT)
#                    homepage    - Homepage
#                    basepkgpath - BASE_PKGPATH (calculated with tobasepkgpath) (required)
#                    fullpkgpath - FULLPKGPATH (required)
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub AddPort
{
	my ($self) = shift;
	my ($port) = @_;

	my ($exists, $iss, $_distfiles, $_sites);

	my $dbh  = $self->{dbh};
	my $sths = $self->{sths};

	my $nvcleared = 0;

	# Check for required fields

	foreach my $key (qw(
		name category version maintainer distfiles sites basepkgpath fullpkgpath
	)) {
		if (!exists $port->{$key} || !$port->{$key}) {
			print STDERR "$port->{fullpkgpath}: missing $key\n";
			debug(__PACKAGE__, $port, Dumper($port));
			return 0;
		}
	}

	foreach my $key (qw(sites distfiles)) {
		if (ref $port->{$key} ne 'ARRAY') {
			if ($port->{$key} =~ /\s/) {
				print STDERR "Wrong format for $key: should be an arrayref or single item.\n";
				return 0;
			}
			$port->{$key} = [ $port->{$key} ];
		}
	}

	# Optional fields

	$port->{distname}     ||= '';
	$port->{suffix}       ||= '';
	$port->{comment}      ||= '';
	$port->{options}      ||= {};
	$port->{pcfg_comment} ||= '';
	$port->{homepage}     ||= '';

	# Sanity checks

	if ($port->{name} =~ /[\s\/]/) {
		print STDERR "$port->{fullpkgpath}: bad port name, "
		    . "space or / not allowed, $port->{name}\n";
		debug(__PACKAGE__, $port, Dumper($port));
		return 0;
	}

	# Add port to database

	$sths->{portdata_exists}->execute($port->{name}, $port->{basepkgpath});
	($exists) = $sths->{portdata_exists}->fetchrow_array;

	$_sites = join(' ', @{$port->{sites}});
	$_distfiles = join(' ', @{$port->{distfiles}});

	if ($exists)
	{
		my $oldver;

		# Clear newver if current version changed.
		$sths->{portdata_getver}->execute($port->{basepkgpath});
		($oldver) = $sths->{portdata_getver}->fetchrow_array;
		if ($oldver ne $port->{version}) {
			$sths->{portdata_clearnewver}->execute($port->{basepkgpath})
				unless ($settings{precious_data});
			$nvcleared = 1;
		}

		unless ($settings{precious_data}) {
			$sths->{portdata_update}->execute(
				$port->{version},
				$port->{comment},
				$port->{category},
				$_distfiles,
				$port->{distname},
				$port->{suffix},
				$_sites,
				$port->{maintainer},
			        $port->{pcfg_comment},
			        $port->{homepage},
 	  	  	        $port->{basepkgpath},
 	  	  	        $port->{fullpkgpath},
 	  	  	        $port->{basepkgpath}
			) or die "Failed to execute: $DBI::errstr";
		}
	}
	else
	{
		unless ($settings{precious_data}) {
			$sths->{portdata_insert}->execute(
				$port->{name},
				$port->{category},
				$port->{distname},
				$port->{version},
				$port->{comment},
				$_distfiles,
				$port->{suffix},
				$_sites,
			    	$port->{maintainer},
			    	$port->{pcfg_comment},
			    	$port->{homepage},
     	  	  	        $port->{basepkgpath},
 	  	  	        $port->{fullpkgpath}
			) or die "Failed to execute: $DBI::errstr";
		}
	}

	# Portconfig stuff

	$sths->{portconfig_isstatic}->execute($port->{name}, $port->{category});
	($iss) = $sths->{portconfig_isstatic}->fetchrow_array;

	if (!$iss) {
		my (%pcfg);

		foreach my $var (keys %{$port->{options}}) {
			my ($val, $fullport);

			$val = $port->{options}->{$var};
			$fullport = "$port->{category}/$port->{name}";

			if ($var !~ /^[A-Za-z]+$/i) {
				print STDERR "$port->{fullpkgpath}: "
				    . "invalid portconfig tuple ($var)\n";
				next;
			}

			if ($var eq 'site') {
				if ($val =~ /^(?:ftp|https?):\/\/[^\/]+/i) {
					$pcfg{indexsite} = $val;
					next;
				}
				print STDERR "$port->{fullpkgpath}: "
				    . "invalid portconfig site ($val)\n";
				next;
			}

			if ($var eq 'limit') {
				# Check regex isn't going to explode
				eval {
					no warnings 'all';
					my $re = '';
					$re =~ /$val/;
					1;
				};

				if ($@) {
					print STDERR "$port->{fullpkgpath}: "
					    . "bad portconfig regex ($val)\n";
					next;
				};

				$pcfg{limitver} = $val;
				next;
			}

			if ($var eq 'ignore') {
				if ($val == 1 or lc $val eq 'yes') {
					$pcfg{ignore} = 1;
				} else {
					$pcfg{ignore} = 0;
				}
				next;
			}

			if ($var eq 'skipb') {
				if ($val == 1 or lc $val eq 'yes') {
					$pcfg{skipbeta} = 1;
				} else {
					$pcfg{skipbeta} = 0;
				}
				next;
			}

			if ($var eq 'skipv') {
				$val =~ s/,+/ /g;
				$pcfg{skipversions} = $val;
				next;
			}

			if ($var eq 'limitw') {
				$val = lc $val;
				if ($val =~ /^(\d{1,2}),(even|odd)$/i) {
					$pcfg{limitwhich} = $1;
					$pcfg{limiteven}  = $2 eq 'even' ? 1 : 0;
				} else {
					print STDERR "$port->{fullpkgpath}: "
					    . "bad portconfig limitw ($val)\n";
				}
				next;
			}

			# We've checked for all the variables we support

			print STDERR "$port->{fullpkgpath}: "
			    . "unknown portconfig key ($var)\n";
		}

		# Nullify any variables we haven't accumulated
		foreach ('indexsite', 'limitver', 'skipversions', 'limiteven', 'limitwhich') {
			$pcfg{$_} = undef if (!exists $pcfg{$_});
		}

		# ...except these, which shouldn't be NULL
		$pcfg{skipbeta} = 1 if !exists($pcfg{skipbeta});
		$pcfg{ignore} = 0 if !exists($pcfg{ignore});

		$sths->{portconfig_update}->execute(
			$pcfg{indexsite}, $pcfg{limitver}, $pcfg{limiteven},
			$pcfg{skipbeta}, $pcfg{skipversions}, $pcfg{limitwhich},
		        $pcfg{ignore}, $port->{basepkgpath}
		) if (!$settings{precious_data});

		# Ensure indexsite is added to sitedata
		push @{$port->{sites}}, $pcfg{indexsite} if ($pcfg{indexsite});

		my $newver;

		$sths->{portdata_getnewver}->execute($port->{basepkgpath});
		($newver) = $sths->{portdata_getnewver}->fetchrow_array;

		# Determine if the portconfig constraints
		# invalidate the current new version.
		if ($newver and !$nvcleared) {
			my $invalid = 0;

			$pcfg{ignore} and $invalid = 1;

			if (defined $pcfg{limiteven} and $pcfg{limitwhich} >= 0) {
				checkevenodd($newver, $pcfg{limiteven}, $pcfg{limitwhich})
					or $invalid = 1;
			}

			if ($pcfg{skipversions}) {
				my @sv = split /\s+/, $pcfg{skipversions};
				foreach (@sv) {
					if ($newver eq $_) {
						$invalid = 1;
						last;
					}
				}
			}

			if ($pcfg{limitver}) {
				$newver =~ /$pcfg{limitver}/
					or $invalid = 1;
			}

			if ($pcfg{skipbeta} && isbeta($port->{version})) {
				isbeta($newver)
					and $invalid = 1;
			}

			if ($invalid and !$settings{precious_data}) {
				$sths->{portdata_clearnewver}->execute($port->{basepkgpath});
			}
		}
	}

	# Sites

	if (@{$port->{sites}}) {
		# Add master site hosts to database
		$self->AddSite($_) foreach (@{$port->{sites}});
	}

	return 1;
}


#------------------------------------------------------------------------------
# Func: AddSite()
# Desc: Register a site to the database.
#
# Args: $site    - Site to add, either a string or a URI object.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub AddSite
{
	my ($self) = shift;
	my ($site) = @_;

	my $dbh  = $self->{dbh};
	my $sths = $self->{sths};

	my $exists;

	$site = URI->new($site) if (!ref $site);

	$sths->{sitedata_exists}->execute($site->host);
	($exists) = $sths->{sitedata_exists}->fetchrow_array;

	if (!$exists && !$settings{precious_data}) {
		$sths->{sitedata_insert}->execute($site->scheme, $site->host)
		    or die "Failed to add new site: $DBI::errstr";
	}
}


1;
