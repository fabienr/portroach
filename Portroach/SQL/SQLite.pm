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

package Portroach::SQL::SQLite;

require Exporter;

use strict;

require 5.006;

our @ISA = qw(Exporter);
our @EXPORT_OK = qw(RegisterHacks);


#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

my $sql = \%Portroach::SQL::sql;


#------------------------------------------------------------------------------
# SQL that is different for this database engine.
#------------------------------------------------------------------------------

# CheckPortsDB

$$sql{sitedata_setrobots} =
	q(UPDATE sitedata
	     SET robots = ?,
	         robots_paths = ?,
	         robots_nextcheck = datetime(CURRENT_TIMESTAMP, '+14 days')
	   WHERE host = ?);

# GenerateHTML

$$sql{portdata_genmaintainers} =
	q(CREATE TEMP TABLE maintainers AS
	  SELECT lower(maintainer) AS maintainer,
	         total,
	         COALESCE(withnewdistfile, 0) AS withnewdistfile,
	         CAST (100*(COALESCE(withnewdistfile, 0)*1.0/total*1.0)
	           AS FLOAT) AS percentage

	    FROM (
	  SELECT maintainer,
	         COUNT(maintainer) AS total,
	         COUNT(newver != ver) AS withnewdistfile
	    FROM portdata
	GROUP BY maintainer
	ORDER BY withnewdistfile DESC
	));

$$sql{portdata_gencategories} =
	q(CREATE TEMP TABLE categories AS
	  SELECT lower(cat) AS cat,
	         total,
	         COALESCE(withnewdistfile, 0) AS withnewdistfile,
	         CAST (100*(COALESCE(withnewdistfile, 0)*1.0/total*1.0)
	           AS FLOAT) AS percentage,
	         unknow, guessed, indexed, handled, ignored

	    FROM (
	  SELECT cat,
	         COUNT(maintainer) AS total,
	         COUNT(newver != ver) AS withnewdistfile,
	         COUNT(CASE WHEN (method=0 AND ignore=False) THEN 1 END) AS unknow,
	         COUNT(CASE WHEN method=1 THEN 1 END) AS guessed,
	         COUNT(CASE WHEN method=2 THEN 1 END) AS indexed,
	         COUNT(CASE WHEN method=3 THEN 1 END) AS handled,
	         COUNT(CASE WHEN ignore=True THEN 1 END) AS ignored
	    FROM portdata
	GROUP BY cat
	ORDER BY cat
	));

$$sql{portdata_gensites} =
	q(CREATE TEMP TABLE sites AS
	  SELECT host,
	         type,
	         total,
	         homepages,
	         COALESCE(withnewdistfile, 0) AS withnewdistfile,
	         CAST (100*(COALESCE(withnewdistfile, 0)*1.0/total*1.0)
	           AS FLOAT) AS percentage,
	         unknow, failures, successes, liecount, robots_nextcheck

	    FROM (
	  SELECT sitedata.host,
	         sitedata.type,
	         COUNT(maintainer) AS total,
	         COUNT(CASE WHEN (homepage ILIKE '%' || sitedata.host || '%') THEN 1 END) AS homepages,
	         COUNT(newver != ver) AS withnewdistfile,
	         COUNT(CASE WHEN (method=0 AND portdata.ignore=False) THEN 1 END) AS unknow,
	         MAX(sitedata.failures) AS failures,
	         MAX(sitedata.successes) AS successes,
	         MAX(sitedata.liecount) AS liecount,
	         MIN(sitedata.robots_nextcheck) AS robots_nextcheck
	    FROM portdata
      INNER JOIN sitedata
	      ON lower(mastersites) LIKE '%' || sitedata.host || '%'
	      OR lower(homepage) LIKE '%' || sitedata.host || '%'
	GROUP BY sitedata.host, sitedata.type
	ORDER BY unknow DESC
	));

# Prune

$$sql{portdata_outdate} =
	q(SELECT id, fullpkgpath, updated
	    FROM portdata
	    WHERE unixepoch(updated) < CAST(? as timestamp)
	);

_transformsql();


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
# Func: RegisterHacks()
# Desc: Implement any missing database functions. This minimises the number of
#       different versions of queries we have to maintain. Needs to be called
#       after each new database connection.
#
# Args: \$dbh - Database handle, already connected.
#
# Retn: n/a
#------------------------------------------------------------------------------

sub RegisterHacks
{
	my ($self) = shift;

	my ($dbh) = @_;

	# Stolen from DBD::PgLite
	$dbh->func(
		'split_part',
		3,
		sub {
			my ($str, $delim, $i) = @_;
			$i ||= 1;
			return (split(/\Q$delim\E/, $str))[$i-1];
		},
		'create_function'
	);

	$dbh->func(
		'position',
		2,
		sub {
			my ($part, $whole) = @_;
			return index($whole, $part) + 1;
		},
		'create_function'
	);

	return;
}


#------------------------------------------------------------------------------
# Func: _transformsql()
# Desc: Transform the SQL queries into a form that works with this database.
#       This is so we can share as many of the SQL queries as possible, rather
#       than duplicating them for minor changes.
#
# Args: n/a
#
# Retn: n/a
#------------------------------------------------------------------------------

sub _transformsql
{
	# A bit over-engineered...
	foreach my $k (keys %$sql) {
		my ($from, $to);

		$$sql{$k} =~ s/true/1/g;
		$$sql{$k} =~ s/false/0/g;

		# Try to implement age()
		if ($$sql{$k} =~ s/age\((.*?)\)\s*([<>=])\s*'(\d+ hours?|minutes?|seconds?)'/datetime($1) _EQU_ datetime('now', '_SIG_$3')/g) {
			my ($sig) = $2;
			if ($sig eq '>') { $$sql{$k} =~ s/_EQU_/</g; $$sql{$k} =~ s/_SIG_/-/; }
			if ($sig eq '<') { $$sql{$k} =~ s/_EQU_/>/g; $$sql{$k} =~ s/_SIG_/-/; }
			if ($sig eq '=') { $$sql{$k} =~ s/_EQU_/=/g; $$sql{$k} =~ s/_SIG_/-/; }
		}

		# Convert position(X in Y) to position(X, Y) for
		# our function implemented above.
		$$sql{$k} =~ s/position\((.*?)\s*[Ii][Nn]\s*(.*?)\)/position($1, $2)/g;

		# Use case-insensitive maintainer INDEX when required
		#$$sql{$k} =~ s/lower\(maintainer\)\s*=\s*lower\(\?\)/maintainer COLLATE NOCASE = ?/gi
		$$sql{$k} =~ s/lower\(maintainer\)\s*=\s*lower\(\?\)/maintainer = ?/gi;
		$$sql{$k} =~ s/lower\(address\)\s*=\s*lower\(\?\)/address = ?/gi;
		$$sql{$k} =~ s/ORDER\s*BY\s*lower\(maintainer\)/ORDER BY maintainer/gi;
	}

	return;
}


1;
