AC_INIT([lyqi-mode], [0.3.0])
AC_PATH_PROG([locate], [locate])

# we use awk in the Makefile
AC_PROG_AWK
# find the directory containing tex hyphenation patterns
AC_MSG_CHECKING([location of TeX hyphenation pattern files])
LYQI_CHECK_HYPHEN_PATTERNS_DIR()
AC_MSG_RESULT([$hyphenation_patterns_dir])

# where is lilypond?
AC_PATH_PROG([LILYPOND],[lilypond])
LYQI_CHECK_LILYPOND

AC_CONFIG_FILES(hyphenate.el package_header.el lyqi-config.el
                Makefile)
AC_OUTPUT
