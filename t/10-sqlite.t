# Check that the various SQL statements work.

use Test;

BEGIN { plan tests => 52; }

use DBI;
use File::Temp qw(tempfile tempdir);

use strict;
use warnings;

use Portroach::Util;
use Portroach::Config;
use Portroach::SQL;

my (%sths, $dbh, $dir, $dbfile, $ret);

$dir = tempdir(CLEANUP => 1);
(undef, $dbfile) = tempfile(DIR => $dir);

# Create database

qx(sqlite3 $dbfile < sql/sqlite_init.sql);
$ret = $?;

ok(!$ret);
die unless (!$ret);

# Connect

$settings{db_connstr} = "DBI:SQLite:dbname=$dbfile";

Portroach::SQL->Load('SQLite');

$dbh = connect_db();

# Prepare all SQL statements

foreach my $key (keys %Portroach::SQL::sql) {
	next if ($key =~ /^ports_/ || $key eq "create_view");
	eval {
		prepare_sql($dbh, \%sths, $key);
	};
	ok($@, "", $key);
}
