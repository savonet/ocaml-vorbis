# $Id$

# check for one particular file of the sources
AC_INIT(ocaml-vorbis, 0.5.1, savonet-users@lists.sourceforge.net)

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT(configuring $PACKAGE_STRING)

# Add prefix to compilation variables
# if passed
if test "x$prefix" != "xNONE"; then
	CFLAGS="$CFLAGS -I$prefix/include"
	LDFLAGS="$LDFLAGS -L$prefix/lib"
	CPPFLAGS="$CPPFLAGS -I$prefix/include"
fi

AC_CHECK_OCAML_COMPILERS()

AC_PROG_CC
AC_CHECK_TOOL([AR],[ar],no)

# PKG_CHECK_MODULE loses, because we need --libs-only-l
# PKG_CHECK_MODULES([OGG],[ogg])
AC_PATH_PROG(PKGCONFIG, pkg-config, no)
if test "$PKGCONFIG" = no; then
	AC_MSG_ERROR([pkg-config not found.])
fi
if ! $PKGCONFIG --exists ogg; then
	AC_MSG_ERROR([libogg not found.])
fi
OGG_LDFLAGS=`$PKGCONFIG --libs-only-L ogg`
AC_SUBST([OGG_LDFLAGS])
OGG_CFLAGS=`$PKGCONFIG --cflags ogg`
AC_SUBST([OGG_CFLAGS])
OGG_LIBS=`$PKGCONFIG --libs-only-l ogg`
AC_SUBST([OGG_LIBS])

#PKG_CHECK_MODULES([VORBIS],[vorbis vorbisenc vorbisfile])
if ! $PKGCONFIG --exists vorbis vorbisenc vorbisfile; then
	AC_MSG_ERROR([libvorbis not found.])
fi
VORBIS_LDFLAGS=`$PKGCONFIG --libs-only-L vorbis vorbisenc vorbisfile`
AC_SUBST([VORBIS_LDFLAGS])
VORBIS_CFLAGS=`$PKGCONFIG --cflags vorbis vorbisenc vorbisfile`
AC_SUBST([VORBIS_CFLAGS])
VORBIS_LIBS=`$PKGCONFIG --libs-only-l vorbis vorbisenc vorbisfile`
AC_SUBST([VORBIS_LIBS])

AC_ARG_WITH([ogg-dir],AC_HELP_STRING([--with-ogg-dir=path],
        [use "path" as the location of ocaml-ogg (autodetected by default)]))
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

# substitutions to perform
AC_SUBST(VERSION)

# Finally create the Makefile and samples
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_CONFIG_FILES([examples/Makefile.wav2ogg],[chmod a-w examples/Makefile.wav2ogg])
AC_CONFIG_FILES([examples/Makefile.ogg2wav],[chmod a-w examples/Makefile.ogg2wav])
AC_CONFIG_FILES([examples/Makefile.stream2wav],[chmod a-w examples/Makefile.stream2wav])
AC_CONFIG_FILES([src/META],[chmod a-w src/META])
AC_OUTPUT
