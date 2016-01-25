# we use `check' to denote that we don't want them installed
check_PROGRAMS = datetime_tests
TESTS=datetime_tests
#datetime_tests_SOURCES = datetime_tests

datetime_tests_SOURCES = datetime_tests.f90

datetime_tests$(EXEEXT) : datetime_tests.f90 ../datetime_module.mod
	$(FC) -I.. $< -o $@ -L.. -ldatetime

CLEANFILES = *.mod
