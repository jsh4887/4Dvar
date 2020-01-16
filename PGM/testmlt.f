      integer*2 yr
      integer*4 t0
      real*4 mlt
      real*4 mlong,mslong
      yr = 89
      mlong = 23.0
      do t0=0,86400,600
         x = mlt(yr, t0, mlong, mslong)
         print *,x,mslong
      enddo
      end
