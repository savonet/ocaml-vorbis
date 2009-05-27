# $Id$

# check for one particular file of the sources
AC_INIT(ocaml-vorbis, 0.5.0, savonet-users@lists.sourceforge.net)

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT(configuring $PACKAGE_STRING)

# Add prefix to compilation variables
# if passed
if test "x$prefix" != "xNONE"; then
	CFLAGS="$CFLAGS -I$prefix/include"
	LDFLAGS="$LDFLAGS -L$prefix/lib"
	CPPFLAGS="$CPPFLAGS -I$prefix/include"
fi

OCAMLFIND_LDCONF=""
AC_ARG_ENABLE([ldconf], AC_HELP_STRING([--disable-ldconf],[don't modify the dynamic loader configuration file (default is enable)]),[ac_enable_ldconf=$enableval],[ac_enable_ldconf=$enableval],[ac_enable_ldconf=yes])
if test "$ac_enable_ldconf" = no ; then
	AC_MSG_RESULT(disabling modification of ld.conf)
	OCAMLFIND_LDCONF=dummy
fi

# Check for Ocaml compilers

# we first look for ocamlc in the path; if not present, we fail
AC_CHECK_PROG(OCAMLC,ocamlc,`which ocamlc`,no)
if test "$OCAMLC" = no ; then
	AC_MSG_ERROR(Cannot find ocamlc.)
fi

# we look for the directory of ocamlc in $OCAMLC
OCAMLBIN=`dirname $OCAMLC`

# we extract Ocaml version number and library path
OCAMLVERSION=`$OCAMLC -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
echo "ocaml version is $OCAMLVERSION"
OCAMLLIB=`$OCAMLC -v | tail -n 1 | cut -f 4 -d " "`
echo "ocaml library path is $OCAMLLIB"

# then we look for ocamlopt; if not present, we issue a warning
# if the version is not the same, we also discard it
# we set OCAMLBEST to "opt" or "byte", whether ocamlopt is available or not
AC_PATH_PROG(OCAMLOPT,ocamlopt,no)
OCAMLBEST=byte
if test "$OCAMLOPT" = no ; then
	AC_MSG_WARN(Cannot find ocamlopt; bytecode compilation only.)
else
	AC_MSG_CHECKING(ocamlopt version)
	TMPVERSION=`$OCAMLOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT(differs from ocamlc; ocamlopt discarded.)
	    OCAMLOPT=no
	else
	    AC_MSG_RESULT(ok)
	    OCAMLBEST=opt
	fi
fi

# checking for ocamlc.opt
AC_PATH_PROG(OCAMLCDOTOPT,ocamlc.opt,no)
if test "$OCAMLCDOTOPT" != no ; then
	AC_MSG_CHECKING(ocamlc.opt version)
	TMPVERSION=`$OCAMLCDOTOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT(differs from ocamlc; ocamlc.opt discarded.)
	else
	    AC_MSG_RESULT(ok)
	    OCAMLC=$OCAMLCDOTOPT
	fi
fi

# checking for ocamlopt.opt
if test "$OCAMLOPT" != no ; then
    AC_PATH_PROG(OCAMLOPTDOTOPT,ocamlopt.opt,no)
    if test "$OCAMLOPTDOTOPT" != no ; then
	AC_MSG_CHECKING(ocamlc.opt version)
	TMPVER=`$OCAMLOPTDOTOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
	if test "$TMPVER" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT(differs from ocamlc; ocamlopt.opt discarded.)
	else
	    AC_MSG_RESULT(ok)
	    OCAMLOPT=$OCAMLOPTDOTOPT
	fi
    fi
fi

if test "$OCAMLOPT" = no ; then
    BEST=byte
else
    BEST="byte opt"
fi

# ocamldep, ocamllex and ocamlyacc should also be present in the path
AC_PATH_PROG(OCAMLDEP,ocamldep,no)
if test "$OCAMLDEP" = no ; then
	AC_MSG_ERROR(Cannot find ocamldep.)
fi

AC_PATH_PROG(OCAMLLEX,ocamllex,no)
if test "$OCAMLLEX" = no ; then
	AC_MSG_ERROR(Cannot find ocamllex.)
fi

AC_PATH_PROG(OCAMLYACC,ocamlyacc,no)
if test "$OCAMLYACC" = no ; then
	AC_MSG_ERROR(Cannot find ocamlyacc.)
fi

AC_PATH_PROG(OCAMLDOC,ocamldoc,no)

AC_PATH_PROG(OCAMLMKTOP,ocamlmktop,no)
if test "$OCAMLMKTOP" = no ; then
	AC_MSG_ERROR(Cannot find ocamlmktop.)
fi

CAMLLIBPATH=`$OCAMLC -where`

AC_PROG_CC

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

# other progs
AC_PATH_PROG(OCAMLFIND,ocamlfind,no)
if test "$OCAMLFIND" = no ; then
	AC_MSG_ERROR(Cannot find ocamlfind.)
fi

AC_PATH_PROG(OCAMLCP,ocamlcp,no)
if test "$OCAMLCP" = no ; then
	AC_MSG_ERROR(Cannot find ocamlcp.)
fi

AC_PATH_PROG(LATEX,latex,no)
#if test "$LATEX" = no ; then
#	AC_MSG_ERROR(Cannot find LaTeX.)
#fi

AC_PATH_PROG(DVIPS,dvips,no)
#if test "$DVIPS" = no ; then
#	AC_MSG_ERROR(Cannot find dvips.)
#fi

AC_PATH_PROG(PS2PDF,ps2pdf,no)
#if test "$PS2PDF" = no ; then
#	AC_MSG_ERROR(Cannot find ps2pdf.)
#fi

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
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(BEST)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
AC_SUBST(OCAMLBEST)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLBIN)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLMKTOP)	# TODO
AC_SUBST(OCAMLFIND)	# TODO
AC_SUBST(OCAMLFIND_LDCONF)
AC_SUBST(OCAMLCP)	# TODO
AC_SUBST(CAMLLIBPATH)

AC_SUBST(LATEX)		# TODO
AC_SUBST(DVIPS)		# TODO
AC_SUBST(DVIPS)		# TODO
AC_SUBST(PS2PDF)	# TODO

# Finally create the Makefile and samples
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_CONFIG_FILES([examples/Makefile.wav2ogg],[chmod a-w examples/Makefile.wav2ogg])
AC_CONFIG_FILES([examples/Makefile.ogg2wav],[chmod a-w examples/Makefile.ogg2wav])
AC_CONFIG_FILES([examples/Makefile.stream2wav],[chmod a-w examples/Makefile.stream2wav])
AC_CONFIG_FILES([src/META],[chmod a-w src/META])
AC_OUTPUT
