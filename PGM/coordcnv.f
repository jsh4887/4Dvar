	program coordcnv
c
c       This program is a short driver program for the Altitude Adjusted
c       Corrected Geomagnetic Coordinates routines.  It demonstrates how 
c       the routine "cnv$coord" is to be called.
c
c       NOTE:  the UNIX and VMS versions of this program are slightly
c       different because of the way end-of-file is signaled in the
c       two operating systems.  The code for both versions is included
c       here, but one set should be commented out.
c
c       $Revision: 1.1.1.1 $
c**********************************************************************
c       $Log: coordcnv.f,v $
c       Revision 1.1.1.1  2007/02/02 05:01:46  gbust
c       Initial Revision
c
c       Revision 1.1.1.1  2006/03/17 16:48:55  rcalfas
c       First Import of IDA3Df90 SMP Log-Normal Codebase
c
c       Revision 1.1.1.1  2005/04/28 13:40:54  rcalfas
c       First Import of Shared Memory Processor version of IDA3D.
c
c       Revision 1.1  2005/03/31 20:16:15  rcalfas
c       Baseline PGM files.
c
c       Revision 1.1  2004/10/26 21:21:02  paulson
c       aacgm library
c
c Revision 1.1  94/10/14  10:49:09  10:49:09  baker (Kile Baker S1G)
c Initial revision
c 
c

	real*4 glat,glong,mlat,mlong,h,r,mlt,t
	integer*4 t0,cnv_mdhms_sec
	integer*2 yr,mn,day,hr,min,sec
	character*1 chr
	integer*2 err,mgflag
	PRINT *,'Coordinate Conversion Program using Spherical Harmonics'
	PRINT *,' '
5	PRINT *,'enter 1 for geographic to geomagnetic and 2 for'
	PRINT *,'geomagnetic to geographic'
	accept *,mgflag
	PRINT *,'enter height (km)'
	accept *,h
c	type *,'enter time(yr,month,day,hr,min,sec)'
c	accept *,yr,mn,day,hr,min,sec
c	t0 = cnv_mdhms_sec(yr,mn,day,hr,min,sec)

10	PRINT *,'enter input latitude and longitude (cntrl-d to exit)'
c10	type *,'enter input latitude and longitude (cntrl-z to exit)'
	read(5,*,end=99)glat,glong
	call cnv$coord(glat,glong,h,4,mlat,mlong,r,mgflag,err)
c	if (mgflag.eq.1)then
c		t=mlt(yr,t0,mlong,mslong)
c	else
c		t=mlt(yr,t0,glong,mslong)
c	end if
	PRINT *,'converted lat and long:',mlat,mlong,'  err=',err
c       type *,'MLT = ',t
	
	goto 10
c
c       UNIX systems use control-d to signal end-of-file
c
99	PRINT *,'enter another cntrl-d to quit or any other character'
	PRINT *,'to change parameters.'
	read(5,1,end=98)chr
1	format(a)
c
c       VMS systems use control-z to signal end-of-file
c
c99	type *,'enter another cntrl-z to quit or any other character'
c	type *,'to change parameters.'
c	read(5,1,end=98)chr
c1	format(a)

	goto 5
98	stop 'normal exit'
	end

