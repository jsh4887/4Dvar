      SUBROUTINE aacgm_init( filename, ascii_flag)
      IMPLICIT NONE
      character*120 filename
      logical*1 ascii_flag
      INTEGER I_NUM_TERMS, I_NUM_AXES, I_NUM_LEVEL, I_NUM_FLAG
      PARAMETER (I_NUM_TERMS = 121)
      PARAMETER (I_NUM_AXES  =   3)
      PARAMETER (I_NUM_LEVEL =   5)
      PARAMETER (I_NUM_FLAG  =   2)
C
C     This program is designed to initialize the common
C     block used by the AACGM coordinate conversion
C     program, SFC$$convert_geo_coord.f.
C
C     The program, SFC$$convert_geo_coord.f has a set of
C     DATA statements which initialize the coordinate
C     conversion to the current epoch using the AACGM
C     coordinate system.  If the AACGM coordinates for
C     an earlier epoch are desired, or some other
C     coordinate system is desired, the common block
C     can be reinitialized using this subroutine.
C
C     The subroutine reads the data for the common block
C     from a named file.  The data file can be either
C     and ASCII file, or a binary data file.  
C
C     The coefficients are ordered according to the
C     standard ordering of data in FORTRAN.
C
C     CALLING SEQUENCE:
C        call AACGM_INIT( filename, ascii_flag)
C          where filename is the name of the file containing
C          the coefficients, and is a logical*1 array of characters
C          where ascii_flag is a logical*1 flag.  It is set
C          to .TRUE. if the file contains ASCII data and .FALSE.
C          if the file contains binary data.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     $Log: aacgm_init.f,v $
C     Revision 1.1.1.1  2007/02/02 05:01:46  gbust
C     Initial Revision
C
C     Revision 1.1.1.1  2006/03/17 16:48:55  rcalfas
C     First Import of IDA3Df90 SMP Log-Normal Codebase
C
C     Revision 1.1.1.1  2005/04/28 13:40:54  rcalfas
C     First Import of Shared Memory Processor version of IDA3D.
C
C     Revision 1.1  2005/03/31 20:16:15  rcalfas
C     Baseline PGM files.
C
C     Revision 1.1  2004/10/26 21:21:00  paulson
C     aacgm library
C
C     Revision 1.4  1997/11/14 19:25:51  baker
C     Chnaged size of coefficients array to accomodate the
C     new code which extends the coordinate system to 7200 km.
C
C     Revision 1.3  1997/11/14 17:02:52  baker
C     Some systems had trouble with equating a single character from
C     filename to a single character in a character string.  This was solved
C     by explicitly using the "char" function.
C
C     Revision 1.2  1996/03/11 21:43:13  baker
C     *** empty log message ***
C
c Revision 1.1  1996/03/11  19:21:34  baker
c Initial revision
c
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      integer fnum/71/,i,t,a,l,f
      double precision D_COEF(I_NUM_TERMS, I_NUM_AXES, I_NUM_LEVEL,
     $     I_NUM_FLAG)

      common / SPH_HARM_MODEL /D_COEF

      if (.NOT. ascii_flag) then
         open(unit=fnum,form='unformatted', status='old',file=filename)
         read(fnum)((((d_coef(t,a,l,f),t=1,I_NUM_TERMS),a=1,I_NUM_AXES),
     $        l=1,I_NUM_LEVEL),f=1,I_NUM_FLAG)
      else
         print *,'file ',filename
         open(unit=fnum,form='formatted', status='old',file=filename)
         read(fnum,*)((((d_coef(t,a,l,f),t=1,I_NUM_TERMS),
     $        a=1,I_NUM_AXES),l=1,I_NUM_LEVEL),f=1,I_NUM_FLAG)
         endif
      close(fnum)
      return
      end
