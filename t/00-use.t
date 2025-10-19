# Check that all the modules work.

use Test;

BEGIN { plan tests => 32; }

use strict;
use warnings;

eval 'use Portroach::Const ();';                    ok(!$@);
eval 'use Portroach::API();';                       ok(!$@);
eval 'use Portroach::Util ();';                     ok(!$@);
eval 'use Portroach::Config ();';                   ok(!$@);

eval 'use Portroach::SiteHandler ();';              ok(!$@);
eval 'use Portroach::SiteHandler::Bitbucket ();';   ok(!$@);
eval 'use Portroach::SiteHandler::Codeberg ();';    ok(!$@);
eval 'use Portroach::SiteHandler::CPAN ();';        ok(!$@);
eval 'use Portroach::SiteHandler::FTP ();';         ok(!$@);
eval 'use Portroach::SiteHandler::GitHub ();';      ok(!$@);
eval 'use Portroach::SiteHandler::GitLab ();';      ok(!$@);
eval 'use Portroach::SiteHandler::Go ();';          ok(!$@);
eval 'use Portroach::SiteHandler::Hackage ();';     ok(!$@);
eval 'use Portroach::SiteHandler::HTTP ();';        ok(!$@);
eval 'use Portroach::SiteHandler::HttpGuess ();';   ok(!$@);
eval 'use Portroach::SiteHandler::HttpHomepage ();';ok(!$@);
eval 'use Portroach::SiteHandler::Launchpad ();';   ok(!$@);
eval 'use Portroach::SiteHandler::Mozilla ();';     ok(!$@);
eval 'use Portroach::SiteHandler::NPM ();';         ok(!$@);
eval 'use Portroach::SiteHandler::Pear ();';        ok(!$@);
eval 'use Portroach::SiteHandler::Pecl ();';        ok(!$@);
eval 'use Portroach::SiteHandler::PyPI ();';        ok(!$@);
eval 'use Portroach::SiteHandler::RubyGems ();';    ok(!$@);
eval 'use Portroach::SiteHandler::SourceForge ();'; ok(!$@);
eval 'use Portroach::SiteHandler::SourceHut ();';   ok(!$@);

eval 'use Portroach::SQL ();';                      ok(!$@);
eval 'use Portroach::SQL::SQLite ();';              ok(!$@);
eval 'use Portroach::SQL::Pg ();';                  ok(!$@);

eval 'use Portroach::Template ();';                 ok(!$@);

eval 'use Portroach::DataSrc ();';                  ok(!$@);
eval 'use Portroach::DataSrc::Ports ();';           ok(!$@);

eval 'use Portroach ();';                           ok(!$@);
