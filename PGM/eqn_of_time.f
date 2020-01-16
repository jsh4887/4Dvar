	real*8 function eqn_of_time(mean_long, iyr)
	implicit NONE
c
c     $Revision: 1.1.1.1 $
c
c     $Log: eqn_of_time.f,v $
c     Revision 1.1.1.1  2007/02/02 05:01:46  gbust
c     Initial Revision
c
c     Revision 1.1.1.1  2006/03/17 16:48:55  rcalfas
c     First Import of IDA3Df90 SMP Log-Normal Codebase
c
c     Revision 1.1.1.1  2005/04/28 13:40:54  rcalfas
c     First Import of Shared Memory Processor version of IDA3D.
c
c     Revision 1.1  2005/03/31 20:16:15  rcalfas
c     Baseline PGM files.
c
c     Revision 1.1  2004/10/26 21:21:02  paulson
c     aacgm library
c
c     Revision 1.4  1997/04/15 16:55:43  baker
c     Added a bit of code to handle the year 2000 problem.
c     If the year is less than 88, we assume it is 2000-something.
c     If the year is between 88 and 99 we assume it is 19xx.
c     If the year is greater than 100 but less than 1900 we
c     assume that it is 1900+the value.  If the year is
c     greater then 1900 we assume it is given correctly in
c     full 4-digit form.
c
c     Revision 1.3  1994/10/17 12:51:20  baker
c     some code had to be rearranged to convince the compiler it was
c     really OK.
c
c Revision 1.2  94/10/17  12:39:41  12:39:41  baker (Kile Baker S1G)
c added ephemerides for 1989 - 1998.  This also required code to be
c added to determine which constants to use.
c 
c Revision 1.1  94/10/14  11:27:56  11:27:56  baker (Kile Baker S1G)
c Initial revision
c 
c
	save
	real*4 mean_long
	integer*2 iyr,index
	real*4 coefs(7,10)
	data coefs/ -105.8,596.2,4.4,-12.7,-429.0,-2.1,19.3,
     1   -105.9,596.2,4.4,-12.7,-429.0,-2.1,19.3,
     2   -106.1,596.2,4.4,-12.7,-428.9,-2.1,19.3,
     3   -106.2,596.2,4.4,-12.7,-428.9,-2.1,19.3,
     4   -106.4,596.1,4.4,-12.7,-428.9,-2.1,19.3,
     5   -106.5,596.1,4.4,-12.7,-428.8,-2.1,19.3,
     6   -106.6,596.1,4.4,-12.7,-428.8,-2.1,19.3,
     7   -106.7,596.1,4.4,-12.7,-428.7,-2.1,19.3,
     8   -106.8,596.1,4.4,-12.7,-428.7,-2.1,19.3,
     9   -107.0,596.1,4.4,-12.7,-428.7,-2.1,19.3/

c
c       compute the index into the coeffiecients table from the year
c
	if (iyr .lt. 88) then 
	   index = iyr + 2000 - 1988
	else if (iyr .ge. 88 .and. iyr .lt. 100) then 
	   index = iyr - 88
	else if (iyr .ge. 100 .and. iyr .lt. 1900) then 
	   index = iyr - 88
	else
	   index = iyr -1988
	endif

	if (index .le. 0) index = 1
	if (index .gt. 10) index = 10

	eqn_of_time = coefs(1,index)*sind(mean_long)
     1               +coefs(2,index)*sind(2.0*mean_long)
     2               +coefs(3,index)*sind(3.0*mean_long)
     3               +coefs(4,index)*sind(4.0*mean_long)
     4               +coefs(5,index)*cosd(mean_long)
     5               +coefs(6,index)*cosd(2.0*mean_long)
     6               +coefs(7,index)*cosd(3.0*mean_long)

	return
	end
