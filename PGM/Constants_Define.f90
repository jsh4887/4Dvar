Module Constants_mod
    use ErrorMessage_mod
 
    type Constants
      private
      double precision :: pi
      double precision :: radToDeg
      double precision :: degToRad
      double precision :: speedOfLight
      double precision :: radiusEarth
      double precision :: TECtoEPMSQ
      double precision, dimension(0:1) :: gmPole
    end type Constants

    type (Constants), private :: constant
    integer,parameter :: MPI_BSEND_OVRHEAD = -40000000

  contains

    subroutine set_default_constants()
      implicit none

      constant%pi = 3.1415926535897931
      constant%radToDeg = 180.0D0/constant%pi
      constant%degToRad = constant%pi/180.0D0
      constant%TECtoEPMSQ = 1.0e16

      ! speedOfLight in m/s
      constant%speedOfLight = 2.99D8

      ! radiusEarth in km
      constant%radiusEarth = 6371.0D0

      ! this is [288.35, 79.4] converted to radians
      constant%gmPole = (/5.032657D0, 1.385791D0/)

      return
    end subroutine set_default_constants

    subroutine initialize_constants()
      implicit none

      call set_default_constants()

    end subroutine initialize_constants

    function GetPi()
      implicit none
      double precision :: GetPi

      GetPi = constant%pi

      return
    end function GetPi

    function GetRadToDeg()
      implicit none
      double precision :: GetRadToDeg

      GetRadToDeg = constant%radToDeg

      return
    end function GetRadToDeg

    function GetDegToRad()
      implicit none
      double precision :: GetDegToRad

      GetDegToRad = constant%degToRad

      return
    end function GetDegToRad

    function GetSpeedOfLight()
      implicit none
      double precision :: GetSpeedOfLight

      GetSpeedOfLight = constant%speedOfLight

      return
    end function GetSpeedOfLight

    function GetRadiusEarth()
      implicit none
      double precision :: GetRadiusEarth

      GetRadiusEarth = constant%radiusEarth

      return
    end function GetRadiusEarth

    function GetGMPoleLat()
      implicit none
      double precision :: GetGMPoleLat

      GetGMPoleLat = constant%gmPole(1)

      return
    end function GetGMPoleLat

    function GetGMPoleLon()
      implicit none
      double precision :: GetGMPoleLon

      GetGMPoleLon = constant%gmPole(0)

      return
    end function GetGMPoleLon

    function GetTECtoEPMSQ()
      implicit none
      double precision :: GetTECtoEPMSQ
 
      GetTECtoEPMSQ = constant%TECtoEPMSQ

      return
    end function GetTECtoEPMSQ

    subroutine print_constants_to_file(outputFile)
      implicit none
      character(LEN=*), intent(in) :: outputFile
      character(LEN=256) :: outfile
      integer :: ios
      integer :: mycount
      character(LEN=200) :: errmsg
      logical :: ex
      character(LEN=8) :: date1
    character (LEN=10) :: time1

      if (outputFile .eq. '') then
        outfile = "./IDA3DConstants.out"
      else
        outfile = outputFile
      endif

      if (outfile .ne. '') then
        !open existing file and append to file    !SG
        inquire(file = outfile, exist = ex)
        if (ex ) then
          open(unit = 12, status = 'OLD', file=outfile, form = 'FORMATTED', position = 'APPEND', iostat = ios)
        else
          open(unit=12, file=outfile, form='FORMATTED', iostat=ios)
        end if
        !open(unit=12, file=outfile, form='FORMATTED', iostat=ios)
        
        if (ios .ne. 0) then
          write(errmsg,*)'Constants:print_constants_to_file(): Incorrect output file specification (',outfile,').\0'
          call set_error_message(errmsg)
        else if (ios .eq. 0) then
          CALL date_and_time(date1,time1)
          write(12,*)'# Generated by Constants on: ',TRIM(date1)
          write(12,'(A3,2x,F18.16)')'Pi:',constant%pi
          write(12,'(A9,2x,F10.7)')'RadToDeg:',constant%radToDeg
          write(12,'(A9,2x,F10.7)')'DegToRad:',constant%degToRad
          write(12,'(A2,2x,E9.2e1)')'c:',constant%speedOfLight
          write(12,'(A12,2x,E9.2e1)')'RadiusEarth:',constant%radiusEarth
          write(12,'(A8,2x,F10.8,2x,F10.8)')'GM Pole:',constant%gmPole(0),constant%gmPole(1)
        endif
      endif

      close(unit=12)
      return
    end subroutine print_constants_to_file

end module Constants_mod

  
