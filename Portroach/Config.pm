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

package Portroach::Config;

require Exporter;

use Getopt::Long;

use Portroach::Const;

use strict;

require 5.006;

our @ISA = qw(Exporter);
our @EXPORT = qw(%settings);


#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

our %settings;

my (@paths, %settings_types, $bool_opts);


#------------------------------------------------------------------------------
# Default Configuration Options
#------------------------------------------------------------------------------

# Useful settings

%settings = (
	templates_dir		=> 'templates',
	html_data_dir		=> '_html',
	sqlports		=> '/usr/local/share/sqlports',

	datasrc			=> 'Portroach::DataSrc::Ports',
	datasrc_opts		=> '',

	build_chop		=> '',
	build_nolimit		=> '',

	regress			=> 0,	# Don't override database on regression
	precious_data		=> 0,	# Don't write anything to database
	num_children		=> 15,	# Number of child processes to spawn
	workqueue_size		=> 20,	# Size of work queue per child
	ftp_retries		=> 3,	# Retry on "connections-per-IP" failures
	ftp_passive		=> 1,	# Use passive FTP where possible
	ftp_timeout		=> 120,	# FTP timeout, in seconds
	http_timeout		=> 120,	# HTTP timeout, in seconds

	mastersite_limit	=> 4,
	mastersite_ignore	=> '',
	oldfound_enable		=> 1,
	newfound_enable		=> 1,

	restrict_maintainer	=> '',
	restrict_category	=> '',
	restrict_port		=> '',

	robots_enable		=> 1,
	robots_checking		=> 'strict',

	local_timezone		=> 'GMT',

	freebsdhacks_enable	=> 1,
	sillystrings_enable	=> 0,

	default_html_sort	=> 'maintainer',

	db_user			=> APPNAME,
	db_name			=> APPNAME,
	db_connstr		=> 'DBI:Pg:dbname='.APPNAME,

	mail_enable		=> 1,
	mail_from		=> APPNAME,
	mail_subject		=> '',
	mail_method		=> 'sendmail',
	mail_host		=> 'localhost',

	user			=> '',
	group			=> '',

	debug			=> 0,
	verbose			=> 0,

	hide_unchanged		=> 0,
);


#------------------------------------------------------------------------------
# Process, parse and store
#------------------------------------------------------------------------------

# Roughly work out variable types

$bool_opts = 'ftp_passive|hide_unchanged|debug|verbose|regress';

foreach (keys %settings) {
	if (/^(?:.+_enable|.+_data|$bool_opts)$/) {
		$settings_types{$_} = TYPE_BOOL;
	} elsif ($settings{$_} =~ /^\d+$/) {
		$settings_types{$_} = TYPE_INT;
	} else {
		$settings_types{$_} = TYPE_STRING;
	}
}

@paths = ('.', '/etc', PREFIX.'/etc');

# Override defaults with config file

ParseConfigFile(CONFIG_FILE, \@paths, \%settings)
    or die 'Unable to parse config file';

# Finally, take note of command line options

GetOptions(
	%{&{sub {
		my %s;
		foreach (keys %settings) {
			my ($t, $c);
			$t = $settings_types{$_} || TYPE_STRING;

			if ($t == TYPE_BOOL) {
				$c = '!';
			} elsif ($t == TYPE_INT) {
				$c = '=i';
			} else {
				$c = '=s';
			}

			$s{$_.$c} = \$settings{$_};
		}
		return \%s;
	}}}
) or exit 1;


#------------------------------------------------------------------------------
# Func: ParseConfigFile()
# Desc: Search for and parse specified configuration file.
#
# Args: $file     - Name of config file
#       \@paths   - List of paths to search for config file
#       \%varlist - Where to put configuration options. Possibly
#                   populated with default values.
#
# Retn: $success  - true/false (false = BAD file, not missing file)
#------------------------------------------------------------------------------

sub ParseConfigFile
{
	my ($file, $paths, $varlist) = @_;

	my $filename;
	my $lineno;

	while ($_ = shift @$paths) {
		if ( -f $_.'/'.$file ) {
			$filename = $_.'/'.$file;
			last;
		}
	}

	# Return true: we can fall back to defaults
	return 1 unless ($filename);

	$lineno = 0;

	open my $cf, "<$filename" or die "Can't open config file";

	while (my $line = <$cf>)
	{
		my ($op_add, $var, $val, $quoted);

		$lineno++;

		# Remove comments
		$line =~ s/#.*$//;

		# Skip empty lines
		next unless ($line);

		if ($line =~ /^\s*(.*?)\s*(\+?=)\s*(.*?)\s*$/) {
			($var, $val) = ($1, $3);
			$op_add = 1 if ($2 eq "+=");
		} else {
			next;
		}

		$var =~ s/\s+/_/g;

		if ($val =~ s/^(["'])//) {
			$val =~ s/$1$//;
			$quoted = 1;
		}

		$var = lc $var;

		if ($var !~ /^[a-z\_]+$/) {
			print STDERR "Invalid variable name in config file "
			    . "$filename, line $lineno\n";
			return 0;
		}

		# Substitute special values

		if (!$quoted) {
			$val = 1 if (lc $val eq 'true');
			$val = 0 if (lc $val eq 'false');
		}

		(1) while (
			$val =~ s/%\((.*?)\)/
				exists $$varlist{$1} ? $$varlist{$1} : ''
			/ge
		);

		if ($op_add) {
			$$varlist{$var} = $$varlist{$var}.$val;
		} else {
			$$varlist{$var} = $val;
		}
	}

	close $cf;

	return 1;
}


1;
