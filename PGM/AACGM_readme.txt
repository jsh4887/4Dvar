NEW FEATURES:

The new version of AACGM coordinates provides two new features:

1) The coordinate system has been extended to 7200 km in altitude (the
	previous version only went up to 2000 km).

2) There are now coefficients tables for 1975, 1980, and 1985, in addition
	to the tables for 1990 and 1995.  NOTE:  because of the change
	in the maximum altitude the 1990 and 1995 tables have changed.  You
	must use the new versions of those tables with the new version of
	the code.

3) The TAR file is accompanied by a PGP signature file (AACGM.tar.sig) which
       can be checked to verify that the contents of the tar file have not
       been tampered with.  The signature is from Kile Baker.  You can
       get his PGP public key from most PGP key servers or by sending
       him email.

BLOCK DATA:
Because of the increase in the number of data files containing coefficients,
the block data routines have been placed in separate files (blkdat75.f,
blkdat80.f, etc).  In order to make sure that the main conversion routine, 
sfc_convert_geo_coord is always initialized to something, you must make sure 
that the most recent blkdata object file is included in the link statement that
creates a program or creates a shareable library.

COEFFICIENTS FILES:
Because of the increase in the number of data files containing coefficients,
the method used by IDL to locate the proper coefficients file has been
changed.  All the files have the form "aacgm_coeffsYYYY.asc" and are 
in plain ascii text.  IDL (see the routine cnvcoord_idl) will use an
environment variable, AACGM_DAT_PREFIX, to define the path to the
filenames.  See below for more information.



LANGUAGES:
The AACGM software is mostly written in FORTRAN,
but there are a few C routines.  Setting up
things so that C and call Fortran routines varies
from system to system and you may have to edit the
C routines to change the names of the fortran 
routines that they call or you may need to add some
dummy routines. See the section "C/FORTRAN incompatibilities
below for more information.



FORTRAN EXTENSIONS:
The Fortran code was written for a VAX/VMS
system and uses some of the VMS extensions
to Fortran.  When compiling the fortran
code you must specify that lines longer
than 72 character are allowed, and the
VAX/VMS extensions to the language are allowed.



SHAREABLE LIBRARY:
If you are planning on using the routines
with IDL you will probably have to create
a shareable library.  This requires that
all the code be position independent and
you will have to set your compiler switches
for both the Fortran and C code appropriately.

In addition to setting up the library,
you need to specify the full path to
the library so that IDL can find it.
This is accomplished through an ENVIRONMENT
variable on UNIX and WINDOWS systems and
a LOGICAL NAME on VMS systems.  The
variables that must be assigned are:

SD_LIB_PGM = full path to the library, including the library name. 
AACGM_DAT_PREFIX = path + prefix for the names of the AACGM coefficients
    The coefficients files all have a name in the form:
    aacgm_coeffsYYYY.asc  where YYYY is the 4-digit year.
    The AACGM_DAT_PREFIX should include the path plus the "aacgm_coeffs"
    EXAMPLE:  AACGM_DAT_PREFIX = /coordinates/AACGM/aacgm_coeffs

You may also want to have explicit paths to the indivicual files such as:
AACGM90_DAT = full path to the data file containing the 1990 coefficients
AACGM95_DAT = full path to the data file containing the 1995 coefficients



C HEADER FILE:
In order to make the code more portable,
the C code uses specially defined integer
types, 'int16' and 'int32'.  These types
are defined in the header file 'SD_types.h'.
The code uses the C preprocessor directive
#include "SD_types.h".  If you want to
place the header file in a directory other
than the one where you put the source code,
you will have to tell your compiler where 
to find the header.




C/FORTRAN INCOMPATIBILITIES:

Some systems try to differentiate between C and FORTRAN names
by added an underscore to the name.  For example, SGI systems
add an underscore to the end of FORTRAN names.  Thus, a 
FORTRAN subroutine called "mysub" will actually create an 
object file with the name "mysub_".  When you write a FORTRAN
program that calls "mysub", the compiler will actually convert the
call in the object code to use "mysub_".  

This causes problems when you try to call a C routine from FORTRAN
and vice versa.  Suppose you have a C routine "mysub1" and a FORTRAN 
subroutine, "mysub2".  If you call both of these routines from a 
FORTRAN program, the FORTRAN compiler will add the underscore to both 
calls.  But the C routine object code does NOT have the underscore 
added, so the FORTRAN program will fail to link properly because it can't
find the subroutine "mysub1_".  Similarly, a C program which tries to 
call both subroutines will fail because it doesn't add an underscore
to either name and so it can't find the FORTRAN object file "mysub2_".

Some compiles and preprocessors have features that allow you to tell
the compile that a subroutine was written in a different language.
If your compiler has such options you can probably modify the code or
the makefile to make use of this feature and allow the subroutines to
be called from any language.

If you don't have this option with your compiler/preprocessor, then 
you might try the following sort of trick, which works for SGI systems.
Create a dummy C routine that adds the underscore character.  For example,
create a dummy routine, "mysub_.c".  This dummy routine should simply contain
a call to "mysub".  Then, a C program that calls "mysub" will go directly
to the C subroutine by that name.  A FORTRAN program that calls "mysub" will
actually go to the dummy C routine "mysub_" which will then call the real
C routine, "mysub".  This adds a bit of overhead to the FORTRAN call, but
in ensures that the subroutine can be called from both C and FORTRAN programs
with no change to the original code.



FORTRAN STRING CONCATENATION PROBLEM:

The Fortran code contains some string
concatenation commands which appear
not to compile properly on some systems.
The offending lines look like this:

<string variable> = <string variable>//'\STRING_CONST\'//<string variable>

On some systems, the backslash is interpreted
as a 'quote' character.  This then tries to
insert the final apostrophe as part of the string.
It then complains that there is no closing apostrophe
for the string.  If this problem occurs for
your system try changing '\STRING_CONST\' 
to '\\STRING_CONST\\'.



DOCUMENTATION:
The file AACGM_users_guide.html provides 
easy to use documentation on how to
call the subroutines.  If you
have a Web server, you can put this
documentation on-line on your own
machine.  You can also contact
the APL web server and get additional
information about the software
as well as the users guide.  The
URL is: http://sd-www.jhuapl.edu/RADAR/AACGM/index.html.


SYSTEM SPECIFIC INFORMATION:

SOLARIS:
The following information has been reported by 
at least one person who has ported the software
to the Sun SOLARIS 2.5 operating system.  I
cannot vouch for this, but if you are running
a SOLARIS system or something very similar and
are having trouble, you should try the following
modifications:
  Use the following options for the compilers
     For C:  cc -c -Kpic -Xt
     for Fortran:  f77 -c -e -pic -xl

IRIX:
The following information has been reported by
Cliff Marcellus (Univ. of Calgary), who has ported
the software to IRIX 5.x and 6.x systems.

    Set the FFLAGS, CFLAGS and LDFLAGS as follows:

FFLAGS = -col120 -static -O
CFLAGS = -dollar -O
LDFLAGS = -shared -all
EXTRALIBS = -lftn -lm


