      SUBROUTINE SFC$$PUT_USER_MSG(I_MSGID, C_CLASS, C_REP_PARAMS,
     $                           I_SYSTEM_ERR, I_COND_VALUE)   
C
C     DUMMY ROUTINE FOR TESTING 
C
C     FOR USE AT SFC, REMOVE THIS ROUTINE, AND
C     REMOVE COMMENTS FROM LINES BEGINNING WITH
C****
C
C
c
ccccccccccccccccccccccccccccccccccccccccccccccccc
c  $Revision: 1.1.1.1 $
c
c  This subroutine is supposed to provide error information
c  printout.  However, since the subroutines are used in
c  a variety of environments, including mixed fortran and C
c  it is generally not possible to 
c  write a subroutine to perform I/O to the users terminal.
c
c  This subroutine is therefore a dummy routine.  If
c  you want to provide additional error output and you
c  are working in a FORTRAN main program, then you can
c  replace this dummy routine with a fortran routine that
c  prints the character strings C_CLASS and C_REP_PARAMS
c
c  If you are in a C environment you would need to
c  add a call in this routine to a C subroutine
c  and pass C_CLASS and C_REP_PARAMS on to that
c  C routine, which would then print the strings to stderr.
c
c  The more general, and safest approach, however, is to 
c  make sure that whenever you call the routine sfc$$convert_geo_coord
c  you check the value of I_ERROR on the return and take appropriate
c  action of the value is non-zero.
c
cccccccccccccc  Revision Log Follows ccccccccccccccccccccccccccc
c
c $Log: sfc_put_user_msg.f,v $
c Revision 1.1.1.1  2007/02/02 05:01:46  gbust
c Initial Revision
c
c Revision 1.1.1.1  2006/03/17 16:48:55  rcalfas
c First Import of IDA3Df90 SMP Log-Normal Codebase
c
c Revision 1.1.1.1  2005/04/28 13:40:54  rcalfas
c First Import of Shared Memory Processor version of IDA3D.
c
c Revision 1.1  2005/03/31 20:16:15  rcalfas
c Baseline PGM files.
c
c Revision 1.1  2004/10/26 21:21:04  paulson
c aacgm library
c
c Revision 1.2  1997/11/14 16:01:10  baker
c fixed some errors in the comments.
c
c Revision 1.1  1996/03/11 19:26:07  baker
c Initial revision
c
c
cccccccccccccc  beginning of code cccccccccccccccccccccccccccccccc
c
      CHARACTER*1   C_CLASS
      CHARACTER*300 C_REP_PARAMS
C
      RETURN
      END
