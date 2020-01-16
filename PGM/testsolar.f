      integer*2 yr
      integer*4 t1

      do yr=89,98
         do t1=0,364*86400,86400
            call solar_loc(yr,t1,xl,xd)
            print *,yr,t1,xl,xd
         enddo
      end do
      end
