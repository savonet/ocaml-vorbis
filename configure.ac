# $Id$

# check for one particular file of the sources
AC_INIT([ocaml-vorbis],[0.7.0],[savonet-users@lists.sourceforge.net])

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT(configuring $PACKAGE_STRING)

AC_BASE_CHECKS()

PKG_PROG_PKG_CONFIG()
PKG_CONFIG_CHECK_MODULE([vorbis])
PKG_CONFIG_CHECK_MODULE([vorbisenc])
PKG_CONFIG_CHECK_MODULE([vorbisfile])

AC_ARG_WITH([ogg-dir],AS_HELP_STRING([--with-ogg-dir=path],[use "path" as the location of ocaml-ogg (autodetected by default)]))
if test -z "$with_ogg_dir"; then
  AC_MSG_CHECKING(for ocaml-ogg)
  if ($OCAMLFIND query ogg > /dev/null 2>&1); then
	OCAMLOGG_INC=`$OCAMLFIND query ogg`
	AC_MSG_RESULT(ok)
  else
	AC_MSG_ERROR(not found.)
  fi
else
  echo $with_ogg_dir | grep ^/ > /dev/null 2>&1 \
  || with_ogg_dir=$PWD/$with_ogg_dir
  OCAMLOGG_INC="$with_ogg_dir"
fi
AC_SUBST(OCAMLOGG_INC)

# Bytes.
AC_CHECK_OCAML_BINDING([bytes],[],[],[1])
INC="$INC `$OCAMLFIND query bytes`"

# substitutions to perform
AC_SUBST(VERSION)
AC_SUBST(INC)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_CONFIG_FILES([examples/Makefile.wav2ogg],[chmod a-w examples/Makefile.wav2ogg])
AC_CONFIG_FILES([examples/Makefile.ogg2wav],[chmod a-w examples/Makefile.ogg2wav])
AC_CONFIG_FILES([examples/Makefile.stream2wav],[chmod a-w examples/Makefile.stream2wav])
AC_CONFIG_FILES([src/META],[chmod a-w src/META])
AC_OUTPUT
