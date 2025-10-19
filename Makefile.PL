use ExtUtils::MakeMaker;

sub MY::libscan {
	my( $mm, $file ) = @_;
	return if $file =~ /\.orig\.port$/;
	return $file;
}

WriteMakefile(
	NAME => "Portroach",
	VERSION_FROM => "Portroach/Const.pm",
	EXE_FILES => ["bin/portroach"],
	MAN1PODS => {'bin/portroach.pod' => 'blib/man1/portroach.1'},
	test => {TESTS => 't/*.t'}
);
