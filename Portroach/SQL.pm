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

package Portroach::SQL;

require Exporter;

use DBI;
use strict;

require 5.006;

our @ISA = qw(Exporter);


#------------------------------------------------------------------------------
# Globals
#------------------------------------------------------------------------------

our %sql;


#------------------------------------------------------------------------------
# SQL that is common to all supported database engines.
#------------------------------------------------------------------------------

$sql{portdata_get} =
	q(SELECT *
	    FROM portdata
	   WHERE fullpkgpath = ?);

$sql{portdata_getnewver} =
	q(SELECT newver
	    FROM portdata
	   WHERE fullpkgpath = ?);

$sql{portdata_clearnewver} =
	q(UPDATE portdata
	     SET newver = NULL, method = NULL
	   WHERE fullpkgpath = ?);

$sql{portdata_dedup} =
	q(SELECT fullpkgpath
	    FROM portdata
	   WHERE basepkgpath = ? AND distfiles = ?);

$sql{portdata_update} =
	q(UPDATE portdata
	     SET name = ?, ver = ?,  comment = ?, cat = ?, distfiles = ?,
	         distname = ?, sufx = ?, mastersites = ?, maintainer = ?,
	         pcfg_comment = ?, homepage = ?, updated = CURRENT_TIMESTAMP,
	         basepkgpath = ?
	   WHERE fullpkgpath = ?);


# Port.pm:BuildPort()

# order by fullpkgpath, make sure shorter (main) path come first
$sql{create_view} = q(CREATE TEMP VIEW RoachData AS
	SELECT
	    FullPkgPath as fullpkgpath,
	    CATEGORIES as categories,
	    case when DISTNAME not null then DISTNAME else FullPkgPath end
	        as distname,
	    ROACH_URL as distfiles,
	    ROACH_SITES as master_sites,
	    MAINTAINER as maintainer,
	    COMMENT as comment,
	    PORTROACH as portroach,
	    PORTROACH_COMMENT as portroach_comment,
	    HOMEPAGE as homepage,
	    FULLPKGNAME as fullpkgname
	FROM portsq
	ORDER BY fullpkgpath);

$sql{ports_select} =
    q(SELECT fullpkgpath, categories, distname, distfiles,
             master_sites, maintainer, comment, portroach,
	     portroach_comment, homepage, fullpkgname
        FROM RoachData);

$sql{ports_select_count} =
    q(SELECT COUNT(fullpkgpath)
        FROM RoachData);

$sql{ports_restrict_maintainer} =
    q(SELECT fullpkgpath, categories, distname, distfiles,
             master_sites, maintainer, comment, portroach,
             portroach_comment, homepage, fullpkgname
        FROM RoachData
       WHERE maintainer like ?);

$sql{ports_restrict_maintainer_count} =
    q(SELECT COUNT(fullpkgpath)
        FROM RoachData
       WHERE maintainer like ?);

$sql{ports_restrict_category} =
    q(SELECT fullpkgpath, categories, distname, distfiles,
             master_sites, maintainer, comment, portroach,
             portroach_comment, homepage, fullpkgname
        FROM RoachData
       WHERE categories like ?);

$sql{ports_restrict_category_count} =
    q(SELECT COUNT(fullpkgpath)
        FROM RoachData
       WHERE categories like ?);

$sql{ports_restrict_port} =
    q(SELECT fullpkgpath, categories, distname, distfiles,
             master_sites, maintainer, comment, portroach,
             portroach_comment, homepage, fullpkgname
        FROM RoachData
       WHERE fullpkgpath like ?);

$sql{ports_restrict_port_count} =
    q(SELECT COUNT(fullpkgpath)
        FROM RoachData
       WHERE fullpkgpath like ?);

$sql{portdata_insert} =
	q(INSERT
	    INTO portdata (name, cat, distname, ver, comment,
	         distfiles, sufx, mastersites, maintainer,
                 pcfg_comment, homepage, method, basepkgpath, fullpkgpath)
	  VALUES (?,?,?,?,?,?,?,?,?,?,?,0,?,?));

$sql{sqlports_count_ports} =
    q(SELECT COUNT(FULLPKGPATH) FROM RoachData);

$sql{portconfig_update} =
	q(UPDATE portdata
	     SET indexsite = ?, limitver = ?,     limiteven = ?,
	         skipbeta = ?,  skipversions = ?, limitwhich = ?,
	         ignore = ?, pcfg_static = ?
	   WHERE fullpkgpath = ?);

# XXX howto set pcfg_static ?
$sql{portconfig_isstatic} =
	q(SELECT pcfg_static
	    FROM portdata
	   WHERE name = ?
	     AND cat = ?);

# CheckPortsDB

$sql{portdata_select} =
	q(SELECT *
	    FROM portdata
	   WHERE ( systemid = (SELECT id
	                         FROM systemdata
	                        WHERE host = ?
	                        LIMIT 1)
	           OR systemid is NULL )
	     AND ignore != true
	ORDER BY fullpkgpath);

$sql{portdata_count} = $sql{portdata_select};
$sql{portdata_count} =~ s/^SELECT \*/SELECT COUNT(*)/i;
$sql{portdata_count} =~ s/ORDER BY.*$/LIMIT 1/i;

$sql{portdata_setchecked} =
	q(UPDATE portdata
	     SET checked = CURRENT_TIMESTAMP
	   WHERE id = ?);

$sql{portdata_setnewver} =
	q(UPDATE portdata
	     SET newver = ?, method = ?, newurl = ?,
	         discovered = CURRENT_TIMESTAMP
	   WHERE id = ?);

$sql{portdata_fixnewver} =
	q(UPDATE portdata
	     SET method = ?, newurl = ?
	   WHERE id = ?);

$sql{portdata_resetnewver} =
	q(UPDATE portdata
	     SET newver = NULL, method = NULL, newurl = NULL,
	         discovered = NULL
	   WHERE id = ?);

$sql{portdata_setmethod} =
	q(UPDATE portdata
	     SET method = ?
	   WHERE id = ?);

$sql{sitedata_exists} =
	q(SELECT COUNT(*)
	    FROM sitedata
	   WHERE host = ?);

$sql{sitedata_select} =
	q(SELECT host, robots, robots_paths, liecount, type,
	         (CURRENT_TIMESTAMP >= robots_nextcheck) AS robots_outofdate
	    FROM sitedata
	   WHERE host = ?
	     AND ignore is not true);

$sql{sitedata_reset} =q(
	UPDATE sitedata
	SET
		successes = 0,
		failures = 0
);

$sql{sitedata_failure} =
	q(UPDATE sitedata
	     SET failures = failures + 1
	   WHERE host = ?);

$sql{sitedata_success} =
	q(UPDATE sitedata
	     SET successes = successes + 1
	   WHERE host = ?);

$sql{sitedata_insert} =
	q(INSERT
	    INTO sitedata (type, host)
	  VALUES (?,?));

$sql{sitedata_initliecount} =
	q(UPDATE sitedata
	     SET liecount = 8
	   WHERE host = ?);

$sql{sitedata_decliecount} =
	q(UPDATE sitedata
	     SET liecount = liecount - 1
	   WHERE host = ?);

#$sql{sitedata_setrobots}

# UncheckPortsDB

$sql{portdata_uncheck} =
	q(UPDATE portdata
	     SET checked = NULL, newver = NULL, status = NULL,
	         newurl = NULL,  method = NULL);

# GenerateHTML

#$sql{portdata_genmaintainers}

#$sql{portdata_gencategories}

#$sql{portdata_gensites}

$sql{portdata_selectall_cat} =
	q(SELECT *
	    FROM portdata
	   WHERE lower(cat) = lower(?)
	ORDER BY cat,name);

$sql{portdata_selectall_limited} =
	q(SELECT *
	    FROM portdata
	   WHERE ( limitver     is not NULL )
	      OR ( limitwhich   is not NULL )
	      OR ( indexsite    is not NULL )
	      OR ( skipversions is not NULL )
	ORDER BY cat,name);

$sql{portdata_selectall_maintainer} =
	q(SELECT *
	    FROM portdata
	   WHERE lower(maintainer) = lower(?)
	ORDER BY cat,name);

$sql{portdata_selectall_outdated} =
	q(SELECT *
	    FROM portdata
	   WHERE newver IS NOT NULL
	ORDER BY cat,name);

$sql{portdata_selectall_site} =
	q(SELECT *
	    FROM portdata
	   WHERE lower(mastersites) LIKE '%' || (?) || '%'
	      OR lower(homepage) LIKE '%' || (?) || '%'
	ORDER BY cat,name);

$sql{portdata_selectall_unknow} =
	q(SELECT *
	    FROM portdata
	   WHERE method IS NULL
	      OR method = 0
	ORDER BY cat,name);

# ShowUpdates

$sql{portdata_selectupdated} =
	q(SELECT lower(maintainer) AS maintainer,
	         fullpkgpath, name, ver, newver
	    FROM portdata
	   WHERE newver IS NOT NULL
	ORDER BY lower(maintainer), fullpkgpath);

$sql{portdata_exists} =
	q(SELECT id
	    FROM portdata
	   WHERE fullpkgpath = ?
	   LIMIT 1);

# MailMaintainers

$sql{maildata_select} =
	q(SELECT address
	    FROM maildata);

$sql{portdata_findnewnew} =
	q(SELECT *
	    FROM portdata
	   WHERE lower(maintainer) LIKE ?
	     AND newver != ver
	     AND newver is not NULL
	     AND ignore != true
	     AND (( mailed != ver AND mailed != newver )
	            OR mailed is NULL )
	ORDER BY cat,name ASC);

$sql{portdata_setmailed} =
	q(UPDATE portdata
	     SET mailed = ?
	   WHERE fullpkgpath = ?);

# AddMailAddrs

$sql{maildata_exists} =
	q(SELECT 1
	    FROM maildata
	   WHERE lower(address) = lower(?)
	   LIMIT 1);

$sql{maildata_insert} =
	q(INSERT
	    INTO maildata (address)
	  VALUES (?));

# RemoveMailAddrs

$sql{maildata_delete} =
	q(DELETE
	    FROM maildata
	   WHERE lower(address) = lower(?));

# Prune

#$sql{portdata_outdate}

$sql{delete_removed} =
    q(DELETE
    	FROM portdata
	WHERE id = ?);

$sql{portdata_fullpkgpaths} =
    q(SELECT id, fullpkgpath
	FROM portdata);

$sql{sqlports_check_fullpkgpath} =
    q(SELECT FULLPKGPATH FROM _Paths WHERE FULLPKGPATH like ?);

# Misc.

$sql{portroach_version} =
	q(SELECT dbver
	    FROM portroach
	ORDER BY dbver DESC
	   LIMIT 1);

$sql{portroach_getstat} =
	q(SELECT val
	    FROM stats
	   WHERE key = ?
	   LIMIT 1);

$sql{portroach_setstat} =
	q(UPDATE stats
	     SET val = ?
	   WHERE key = ?);


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

	bless ($self, $class);
	return $self;
}


#------------------------------------------------------------------------------
# Func: Load()
# Desc: Initialise; load the SQL from the required module.
#
# Args: $db      - DBI engine name.
#
# Retn: $success - true/false
#------------------------------------------------------------------------------

sub Load
{
	my $self = shift;

	my ($db) = @_;

	return 0 if (!$db);

	eval 'use Portroach::SQL::' . $db . ' qw(RegisterHacks);';

	if ($@) {
		warn $@;
		return 0;
	}

	return 1;
}

sub connect_sqlports
{
    my $sqlports_path = shift;
    my $dbh = DBI->connect("dbi:SQLite:dbname=${sqlports_path}", '', '')
	or die "Could not open SQLports database at ${sqlports_path}: $DBI::errstr";
    return $dbh;
}

1;
