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

my (%sths, $dbh, $dbuser, $dbname, $ret);

$dbname = 'ps_test_' . randstr(8);
$dbuser = $dbname;

# Create database

qx(createuser -DRS -U postgres "$dbuser");
die if $?;
qx(createdb -U postgres -E UNICODE -O "$dbuser" "$dbname");
die if $?;

qx(psql $dbuser $dbname < sql/pgsql_init.sql);
$ret = $?;

ok(!$ret);
die unless (!$ret);

# Connect

$settings{db_user} = $dbuser;
$settings{db_pass} = '';
$settings{db_connstr} = "DBI:Pg:dbname=$dbname";

Portroach::SQL->Load('Pg');

$dbh = connect_db();

# Prepare all SQL statements

foreach my $key (keys %Portroach::SQL::sql) {
	next if ($key =~ /^ports_/ || $key eq "create_view");
	eval {
		prepare_sql($dbh, \%sths, $key);
	};
	ok($@, "", $key);
}

END {
	if ($dbh) {
		finish_sql($dbh, \%sths);
		$dbh->disconnect;
	}
	if ($dbname) { qx(dropdb -U postgres "$dbname"); }
	if ($dbuser) { qx(dropuser -U postgres "$dbuser"); }
}
