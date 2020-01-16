      parameter (I_NUM_TERMS = 121)
      parameter (I_NUM_AXES = 3)
      parameter (I_NUM_LEVEL = 5)
      parameter (I_NUM_FLAG = 2)
c
      double precision D_COEF(I_NUM_TERMS, I_NUM_AXES, I_NUM_LEVEL, I_NUM_FLAG)

      integer fnum/71/,i,t,a,l,f
      common / SPH_HARM_MODEL /D_COEF
      character*80 filename

      type *,'enter the filename for the output data'
      accept *,filename

      open(unit=fnum,form='formatted',type='new',name=filename)
      do f=1,I_NUM_FLAG
         do l=1,I_NUM_LEVEL
            do a=1,I_NUM_AXES
               write(fnum,101) (D_COEF(i,a,l,f),i=1,I_NUM_TERMS)
 101           format(24(1x,5d16.9/),d16.9)
               enddo
         enddo
      enddo
      close(fnum)
      stop
      end
