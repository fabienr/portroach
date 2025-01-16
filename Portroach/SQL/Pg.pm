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

package Portroach::SQL::Pg;

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
	         robots_nextcheck = CURRENT_TIMESTAMP + INTERVAL '2 weeks'
	   WHERE host = ?);

# GenerateHTML

$$sql{portdata_genmaintainers} =
	q(SELECT maintainer,
	         total,
	         COALESCE(withnewdistfile, 0) AS withnewdistfile,
	         CAST (100*(COALESCE(withnewdistfile, 0)*1.0/total*1.0) AS FLOAT)
	           AS percentage
	    INTO TEMP maintainers
	
	    FROM (
	  SELECT lower(maintainer) AS maintainer,
	         COUNT(maintainer) AS total,
	         COUNT(newver != ver) AS withnewdistfile
	    FROM portdata
	GROUP BY lower(maintainer)
	)
	      AS pd1
	);

$$sql{portdata_gencategories} =
	q(SELECT cat,
	         total,
	         COALESCE(withnewdistfile, 0) AS withnewdistfile,
	         CAST (100*(COALESCE(withnewdistfile, 0)*1.0/total*1.0) AS FLOAT)
	           AS percentage,
	         unknow, guessed, indexed, handled, ignored
	    INTO TEMP categories
	
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
	)
	      AS pd1
	);


$$sql{portdata_gensites} =
	q(SELECT host,
	         type,
	         total,
	         COALESCE(withnewdistfile, 0) AS withnewdistfile,
	         CAST (100*(COALESCE(withnewdistfile, 0)*1.0/total*1.0) AS FLOAT)
	           AS percentage,
	         unknow, failures, successes, liecount, robots_nextcheck
	    INTO TEMP sites
	
	    FROM (
	  SELECT sitedata.host,
	         sitedata.type,
	         COUNT(maintainer) AS total,
	         COUNT(newver != ver) AS withnewdistfile,
	         COUNT(CASE WHEN (method=0 AND portdata.ignore=False) THEN 1 END) AS unknow,
	         SUM(sitedata.failures) AS failures,
	         SUM(sitedata.successes) AS successes,
	         SUM(sitedata.liecount) AS liecount,
	         MIN(sitedata.robots_nextcheck) AS robots_nextcheck
	    FROM portdata
	    INNER JOIN sitedata ON mastersites ILIKE '%' || sitedata.host || '%'
	    GROUP BY sitedata.host, sitedata.type
	)
	      AS pd1
	);

# Prune

$$sql{portdata_outdate} =
	q(SELECT id, fullpkgpath, updated
	    FROM portdata
	    WHERE EXTRACT(EPOCH FROM updated) < CAST(? AS bigint));

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

	$dbh->do("set timezone TO 'UTC';");

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
	return;
}


1;
