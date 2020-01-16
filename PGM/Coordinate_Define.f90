Module Coordinate_mod
  use Constants_mod
  use ErrorMessage_mod

  interface GeogToGeom
    module procedure GeogToGeom_scalar
  end interface

  interface GeogToGeom
    module procedure GeogToGeom_vector
  end interface

  interface GeomToGeog
    module procedure GeomToGeog_scalar
  end interface

  interface GeomToGeog
    module procedure GeomToGeog_vector
  end interface

  interface ConvertToLatLonAlt
    module procedure ConvertToLatLonAlt_scalar
  end interface

  interface ConvertToLatLonAlt
    module procedure ConvertToLatLonAlt_vector
  end interface

  interface ConvertToXYZ
    module procedure ConvertToXYZ_scalar
  end interface

  interface ConvertToXYZ
    module procedure ConvertToXYZ_vector
  end interface

  interface ConvertSGToLatLon
    module procedure ConvertSGToLatLon_scalar
  end interface

  interface ConvertSGToLatLon
    module procedure ConvertSGToLatLon_vector
  end interface

  interface ConvertToSGXY
    module procedure ConvertToSGXY_scalar
  end interface

  interface ConvertToSGXY
    module procedure ConvertToSGXY_vector
  end interface

  contains

    subroutine set_default_coordinate_state()
      implicit none

      call initialize_constants()

      return
    end subroutine set_default_coordinate_state

    subroutine initialize_coordinate_state()
      implicit none

      call set_default_coordinate_state()

      return
    end subroutine initialize_coordinate_state

    ! Convert from geographic coordinates to tilted dipole
    ! geomagnetic dipole coordinates.
    
    SUBROUTINE GeogToGeom_scalar(latggv, longgv, points_gmv)
      double precision, intent(in) :: latggv
      double precision, intent(in) :: longgv
      double precision, dimension(0:1), intent(inout) :: points_gmv
      double precision :: lat0, lon0
      double precision :: latgm, longm
      double precision :: slatgm, slongm
      double precision :: clatgm, clongm
      double precision :: pi
      integer :: mgflag, i_err
      real*4 :: in_lat, in_lon, out_lat, out_lon, r, h
      character(LEN=200) :: errmsg
  
! RSC the FLOAT intrinsic is for converting from INT to FLOAT
!	the correct intrinsic for Double Precision is SNGL
!      in_lat = FLOAT(latggv*GetRadToDeg()) ! input latitude
!      in_lon = FLOAT(longgv*GetRadToDeg()) ! input longitude
      in_lat = SNGL(latggv*GetRadToDeg()) ! input latitude
      in_lon = SNGL(longgv*GetRadToDeg()) ! input longitude

      h = FLOAT(0)         ! height
      i_err = 0       ! error flag
      mgflag = 1      ! geo -> AACGM
      ! SUBROUTINE call from AACGM library
      ! Input lat in degrees -90 to 90
      ! Input lon in degrees -180 to 180 or 0 to 360
      ! Input height in km
      ! Output is in degrees
      ! lat -90 to 90
      ! lon -180 to 180
      if (in_lon .lt. 0.0) in_lon = in_lon + 360.0D0
      CALL SFC$$CONVERT_GEO_COORD(in_lat, in_lon, h, out_lat, out_lon, mgflag, i_err)
      IF (i_err.NE.0) THEN
        WRITE(errmsg,*) 'GeogToGeom_scalar::',&
        'SFC$$CONVERT_GEO_COORD ERROR -- ',i_err, in_lat, in_lon
        PRINT *,errmsg
        CALL set_error_message(errmsg)
        RETURN
      END IF
      ! convert lon to 0 to 360
      IF (out_lon.LT.0.0D0) out_lon = out_lon + 360.0D0
      ! convert to radians
      points_gmv(0) = out_lat*GetDegToRad()
      points_gmv(1) = out_lon*GetDegToRad()

      RETURN
    END SUBROUTINE GeogToGeom_scalar

    SUBROUTINE GeogToGeom_vector(latggv, longgv, vec_gmv)
      double precision, dimension(0:), intent(in) :: latggv
      double precision, dimension(0:), intent(in) :: longgv
      double precision, dimension(0:,0:), intent(inout) :: vec_gmv 
      double precision :: lat0, lon0
      double precision :: latgg, longg
      double precision :: latgm, longm
      double precision :: slatgm, slongm
      double precision :: clatgm, clongm
      double precision :: pi
      integer :: i, j, k
      integer :: np
      integer :: status
      logical :: ok
      character(LEN=200) :: errmsg
!      double precision, allocatable, target, dimension(:,:), save :: points_gmv
      double precision, allocatable, dimension(:,:) :: points_gmv
      real*4 :: in_lat,in_lon,h,out_lat,out_lon
      integer :: mgflag, i_err

      ok = ((size(latggv)).EQ.(size(longgv)))
      if (.not. ok) then
        write(errmsg,*)'Coordinate:GeogToGeom_vector(): The input ',&
        'lat and lon vectors are not the same length\0'
        call set_error_message(errmsg)
      else
        np = size(latggv)

        if (np .ge. 0) then
          allocate(points_gmv(0:1,0:np-1), STAT=status)
          if (status .gt. 0) then
            write(errmsg,*)'Coordinate:GeogToGeom_vector(): ',&
            'allocation error (points_gmv(2,',np,'))\0'
            call set_error_message(errmsg)
            np = 0
          endif
        endif

        do i=0,np-1
          in_lat = latggv(i)*GetRadToDeg() ! input latitude
          in_lon = longgv(i)*GetRadToDeg() ! input longitude
          h = 0          ! height
          i_err = 0             ! error flag
          mgflag = 1            ! geo -> AACGM
          ! SUBROUTINE call from AACGM library
          ! Input lat in degrees -90 to 90
          ! Input lon in degrees -180 to 180 or 0 to 360
          ! Input height in km
          ! Output is in degrees
          ! lat -90 to 90
          ! lon -180 to 180
          CALL SFC$$CONVERT_GEO_COORD(in_lat, in_lon, h, out_lat, out_lon, mgflag, i_err)
          IF (i_err.NE.0) THEN
             WRITE(errmsg,*) 'GeogToGeom_vector::',&
             'SFC$$CONVERT_GEO_COORD ERROR -- ',i_err
             PRINT *,errmsg
             print *,' i ilon olon ',i,in_lon,out_lon
             CALL set_error_message(errmsg)
             RETURN
          END IF
          ! convert lon to 0 to 360
          IF (out_lon.LT.0.0D0) out_lon = out_lon + 360.0D0
          ! convert to radians
          points_gmv(0,i) = out_lat*GetDegToRad()
          points_gmv(1,i) = out_lon*GetDegToRad()
          vec_gmv(0,i) = points_gmv(0,i)
          vec_gmv(1,i) = points_gmv(1,i)
        END DO
        deallocate(points_gmv)
       END IF

       RETURN
    END SUBROUTINE GeogToGeom_vector

    SUBROUTINE GeomToGeog_scalar(latgmv, longmv, points)
      double precision, intent(in) :: latgmv
      double precision, intent(in) :: longmv
      double precision, dimension(0:1) :: points
      double precision :: lat0, lon0
      double precision :: latgg, longg
      double precision :: sbm, cbm
      double precision :: slm, clm
      double precision :: sbg, cbg
      double precision :: slg, clg
      double precision :: pi
      double precision :: si
      double precision :: ci
      double precision, target, dimension(0:1), save :: points_ggv
      character(LEN=200) :: errmsg     
      real*4 :: in_lat, in_lon, h, out_lat, out_lon
      integer :: mgflag, i_err 

      in_lat = latgmv*GetRadToDeg() ! input latitude
      in_lon = longmv*GetRadToDeg() ! input longitude
      h = 0         ! height
      
      i_err = 0       ! error flag
      mgflag = 2      ! AACGM -> geo
      ! SUBROUTINE call from AACGM library
      ! Input lat in degrees -90 to 90
      ! Input lon in degrees -180 to 180 or 0 to 360
      ! Input height in km
      ! Output is in degrees
      ! lat -90 to 90
      ! lon -180 to 180
      CALL SFC$$CONVERT_GEO_COORD(in_lat, in_lon, h, out_lat, out_lon, mgflag, i_err)
      IF (i_err.NE.0) THEN
        WRITE(errmsg,*) 'GeomToGeog_scalar::',&
        'SFC$$CONVERT_GEO_COORD ERROR -- ',i_err
        PRINT *,errmsg
        CALL set_error_message(errmsg)
        RETURN
      END IF
!      print *,'out_lat,outLon ',out_lat,out_lon
      ! convert lon to 0 to 360
      IF (out_lon.LT.0.0D0) out_lon = out_lon + 360.0D0
      ! convert to radians
      points_ggv(0) = out_lat*GetDegToRad()
      points_ggv(1) = out_lon*GetDegToRad()
      
      points = points_ggv

      RETURN
    END SUBROUTINE GeomToGeog_scalar

    SUBROUTINE GeomToGeog_vector(latgmv, longmv, vec_ggv)
      double precision, dimension(0:), intent(in) :: latgmv
      double precision, dimension(0:), intent(in) :: longmv
      double precision, dimension(0:,0:), intent(inout) :: vec_ggv 
      double precision :: lat0, lon0
      double precision :: latgm, longm
      double precision :: latgg, longg
      double precision :: sbm, cbm
      double precision :: slm, clm
      double precision :: sbg, cbg
      double precision :: slg, clg
      double precision :: pi
      integer :: i
      integer :: np
      integer :: status
      logical :: ok
      double precision :: si
      double precision :: ci
      character(LEN=200) :: errmsg
      double precision, allocatable, target, dimension(:,:), save :: points_ggv
      real*4 :: in_lat, in_lon, h
      integer :: mgflag, ierr

      ok = size(latgmv) == size(longmv)

      if (.not. ok) then
        write(errmsg,*)'Coordinate:GeomToGeog_vector(): ',&
        'The input lat and lon vectors are not the same length\0'
        call set_error_message(errmsg)
      else
        np = size(latgmv)

        if (np .ge. 0) then
          allocate(points_ggv(0:1,0:np-1), STAT=status)
          if (status .gt. 0) then
            write(errmsg,*)'Coordinate:GeomToGeog_vector(): ',&
            'allocation error (points_ggv(2,',np,'))\0'
            call set_error_message(errmsg)
            np = 0
          endif
        endif

        do i=0,np-1
          in_lat = latgmv(i)*GetRadToDeg() ! input latitude
          in_lon = longmv(i)*GetRadToDeg() ! input longitude
          h = 0          ! height
          i_err = 0             ! error flag
          mgflag = 1            ! geo -> AACGM
          ! SUBROUTINE call from AACGM library
          ! Input lat in degrees -90 to 90
          ! Input lon in degrees -180 to 180 or 0 to 360
          ! Input height in km
          ! Output is in degrees
          ! lat -90 to 90
          ! lon -180 to 180
          CALL SFC$$CONVERT_GEO_COORD(in_lat, in_lon, h, out_lat, out_lon, mgflag, i_err)
          IF (i_err.NE.0) THEN
             WRITE(errmsg,*) 'GeomToGeog_vector::',&
             'SFC$$CONVERT_GEO_COORD ERROR -- ',i_err
             PRINT *,errmsg
             CALL set_error_message(errmsg)
             RETURN
          END IF
          ! convert lon to 0 to 360
          IF (out_lon.LT.0.0D0) out_lon = out_lon + 360.0D0
          ! convert to radians
          points_ggv(0,i) = out_lat*GetDegToRad()
          points_ggv(1,i) = out_lon*GetDegToRad()
          vec_ggv(0,i) = points_ggv(0,i)
          vec_ggv(1,i) = points_ggv(1,i)
        END DO
        deallocate(points_ggv)
      END IF

      RETURN
    END SUBROUTINE GeomToGeog_vector


    !
    ! Converts x,y,z to Lat,Lon,Alt
    !
    ! lat and lon are output in Radians
    !
    subroutine ConvertToLatLonAlt_scalar(xp, y, z, lat, lon, alt)
      implicit none
      double precision, intent(in) :: xp
      double precision, intent(in) :: y
      double precision, intent(in) :: z
      double precision, intent(inout) :: lat
      double precision, intent(inout) :: lon
      double precision, intent(inout) :: alt
      double precision :: rho,x
      double precision :: radius
      character(LEN=200) :: errmsg

      x = xp
      lat = 0.0D0
      lon = 0.0D0
      alt = 0.0D0
      IF (abs(x).LE.1.0E-30) x = 1.0E-30
      rho = sqrt(x**2 + y**2)
      radius = sqrt(z**2 + rho**2)
      IF (rho.LE.0.0D0) rho = 1.0E-30

      lat = atan(z / rho)
      lon = atan2(y, x)

      ! ensure longitude is between 0 and 2*pi

      if (lon .lt. 0.0D0) then
        lon = lon + 2.0D0 * GetPi()
      endif

      alt = radius - GetRadiusEarth()

      return
    end subroutine ConvertToLatLonAlt_scalar

    subroutine ConvertToLatLonAlt_vector(x, y, z, lat, lon, alt)
      implicit none
      double precision, dimension(0:), intent(in) :: x
      double precision, dimension(0:), intent(in) :: y
      double precision, dimension(0:), intent(in) :: z
      double precision, dimension(0:), intent(out) :: lat
      double precision, dimension(0:), intent(out) :: lon
      double precision, dimension(0:), intent(out) :: alt
      double precision, allocatable :: rho(:)
      double precision, allocatable :: radius(:)
      double precision :: earthRadius, pi
      integer :: np
      integer :: allocateStatus
      logical :: ok
      character(LEN=200) :: errmsg

      pi = GetPi()
      earthRadius = GetRadiusEarth()

      lat = 0.0D0
      lon = 0.0D0
      alt = 0.0D0

      np = size(x)

      ok = size(x) == np .and. size(y) == np .and. size(z) == np

      if (.not. ok) then
        write(errmsg,*)'Coordinate:ConvertToLatLonAlt_vector(): ',&
        'The input x, y, and z vectors are not the same length\0'
        call set_error_message(errmsg)
      else

        if (np .ge. 0) then
          allocate(rho(0:np-1), STAT=allocateStatus);
          if (allocateStatus .gt. 0) then
            write(errmsg,*)'Coordinate:ConvertToLatLonAlt_vector(): ',&
            'allocation error (rho(',np,'))\0'
            call set_error_message(errmsg)
          else
            allocate(radius(0:np-1), STAT=allocateStatus);
            if (allocateStatus .gt. 0) then
              write(errmsg,*)'Coordinate:ConvertToLatLonAlt_vector(): ',&
              'allocation error (radius(',np,'))\0'
              call set_error_message(errmsg)
            endif

            if (.not. Did_an_Error_Occur()) then
              rho = sqrt(x**2 + y**2)
              radius = sqrt(z**2 + rho**2)

              lat = atan(z / rho)
              lon = atan2(y, x)

              ! ensure longitude is between 0 and 2*pi

              where (lon .lt. 0.0D0)
                lon = lon + 2.0D0 * pi
              endwhere

              alt = radius - earthRadius
            endif
          endif

          if (allocated(rho)) deallocate(rho)
          if (allocated(radius)) deallocate(radius)
        endif
      endif

      IF (allocated(rho)) deallocate(rho)
      IF (allocated(radius)) deallocate(radius)

      return
    end subroutine ConvertToLatLonAlt_vector

    !
    ! Converts lat,lon,alt to x,y,z
    !
    ! lat and lon are input in Radians
    !
    subroutine ConvertToXYZ_scalar(lat, lon, alt, x, y, z)
      implicit none
      double precision, intent(in) :: lat
      double precision, intent(in) :: lon
      double precision, intent(in) :: alt
      double precision, intent(out) :: x
      double precision, intent(out) :: y
      double precision, intent(out) :: z
      double precision :: radius

      x = 0.0D0
      y = 0.0D0
      z = 0.0D0

      radius = alt + GetRadiusEarth()

      z = radius * sin(lat)
      y = radius * cos(lat) * sin(lon)
      x = radius * cos(lat) * cos(lon)

      return
    end subroutine ConvertToXYZ_scalar

    subroutine ConvertToXYZ_vector(lat, lon, alt, x, y, z)
      implicit none
      double precision, dimension(0:), intent(in) :: lat
      double precision, dimension(0:), intent(in) :: lon
      double precision, dimension(0:), intent(in) :: alt
      double precision, dimension(0:), intent(out) :: x
      double precision, dimension(0:), intent(out) :: y
      double precision, dimension(0:), intent(out) :: z
      double precision, allocatable :: radius(:)
      double precision :: earthRadius
      integer :: np
      integer :: allocateStatus
      logical :: ok
      character(LEN=200) :: errmsg

      earthRadius = GetRadiusEarth()

      x = 0.0D0
      y = 0.0D0
      z = 0.0D0

      np = size(lat)

      ok = size(lat) == np .and. size(lon) == np .and. size(alt) == np

      if (.not. ok) then
        write(errmsg,*)'Coordinate:ConvertToXYZ_vector(): The input ',&
        'lat, lon, and alt vectors are not the same length\0'
        call set_error_message(errmsg)
      else
        if (np .ge. 0) then
          allocate(radius(0:np-1), STAT=allocateStatus);
          if (allocateStatus .gt. 0) then
            write(errmsg,*)'Coordinate:ConvertToXYZ_vector(): ',&
            'allocation error (radius(',np,'))\0'
            call set_error_message(errmsg)
          endif

          if (.not. Did_an_Error_Occur()) then
            radius = alt + earthRadius

            z = radius * sin(lat)
            y = radius * cos(lat) * sin(lon)
            x = radius * cos(lat) * cos(lon)
          endif
          if (allocated(radius)) deallocate(radius)
        endif
      endif
      IF (allocated(radius)) deallocate(radius)

      return
    end subroutine ConvertToXYZ_vector


    !
    ! Converts StereoGraphic x,y to Lat,Lon (assumes alt = 0.0)
    ! lat and lon are output in Radians
    !
    subroutine ConvertSGToLatLon_scalar(x, y, lat, lon)
      implicit none
      double precision, intent(in) :: x
      double precision, intent(in) :: y
      double precision, intent(out) :: lat
      double precision, intent(out) :: lon
      double precision :: rho
      double precision :: earthRadius
      double precision :: c
      double precision :: c_lat, c_lon
      character(LEN=200) :: errmsg

      earthRadius = GetRadiusEarth()

      lat = 0.0D0
      lon = 0.0D0

      ! define tangent point where projection plane contacts sphere (south_pole)

      c_lat = -90.0D0 * GetDegToRad()
      c_lon = 0.0D0 * GetDegToRad()
      !if (y .gt. 0) then
      !   c_lon = 0.0D0 * GetDegToRad()
      !else
      !   c_lon = 180.0D0 * GetDegToRad()
      !endif

      rho = sqrt(x*x + y*y)

      c = 2.0D0 * atan(rho / (2.0D0 * earthRadius))

      lat = asin( cos(c) * sin(c_lat) + (y * sin(c) * cos(c_lat)) / rho)
      lon = c_lon + atan2( (x * sin(c)), (rho * cos(c_lat) * cos(c) - y * sin(c_lat) * sin(c)))

      ! ensure longitude is between 0 and 2*pi

      if (lon .lt. 0.0D0) then
        lon = lon + 2.0D0 * GetPi();
      endif

      return
    end subroutine ConvertSGToLatLon_scalar

    subroutine ConvertSGToLatLon_vector(x, y, lat, lon)
      implicit none
      double precision, dimension(0:), intent(in) :: x
      double precision, dimension(0:), intent(in) :: y
      double precision, dimension(0:), intent(out) :: lat
      double precision, dimension(0:), intent(out) :: lon
      double precision, allocatable :: rho(:)
      double precision, allocatable :: radius(:)
      double precision, allocatable :: c(:)
      double precision, allocatable :: c_lat(:), c_lon(:)
      double precision :: earthRadius, pi, DtoR
      integer :: np
      integer :: allocateStatus
      logical :: ok
      character(LEN=200) :: errmsg

      pi = GetPi()
      earthRadius = GetRadiusEarth()
      DtoR = GetDegToRad()

      lat = 0.0D0
      lon = 0.0D0

      np = size(x)

      ok = size(x) == np .and. size(y) == np

      if (.not. ok) then
        write(errmsg,*)'Coordinate:ConvertSGToLatLon_vector(): ',&
        'The input x and y vectors are not the same length\0'
        call set_error_message(errmsg)
      else

        if (np .ge. 0) then
          allocate(rho(0:np-1), STAT=allocateStatus);
          if (allocateStatus .gt. 0) then
            write(errmsg,*)'Coordinate:ConvertSGToLatLon_vector(): ',&
            'allocation error (rho(',np,'))\0'
            call set_error_message(errmsg)
          else
            allocate(radius(0:np-1), STAT=allocateStatus);
            if (allocateStatus .gt. 0) then
              write(errmsg,*)'Coordinate:ConvertSGToLatLon_vector(): ',&
              'allocation error (radius(',np,'))\0'
              call set_error_message(errmsg)
            endif

            allocate(c(0:np-1), STAT=allocateStatus);
            if (allocateStatus .gt. 0) then
              write(errmsg,*)'Coordinate:ConvertSGToLatLon_vector(): ',&
              'allocation error (c(',np,'))\0'
              call set_error_message(errmsg)
            endif

            allocate(c_lat(0:np-1), STAT=allocateStatus);
            if (allocateStatus .gt. 0) then
              write(errmsg,*)'Coordinate:ConvertSGToLatLon_vector(): ',&
              'allocation error (c_lat(',np,'))\0'
              call set_error_message(errmsg)
            endif

            allocate(c_lon(0:np-1), STAT=allocateStatus);
            if (allocateStatus .gt. 0) then
              write(errmsg,*)'Coordinate:ConvertSGToLatLon_vector(): ',&
              'allocation error (c_lon(',np,'))\0'
              call set_error_message(errmsg)
            endif

            if (.not. Did_an_Error_Occur()) then
              ! define tangent point where projection plane contacts sphere (south_pole)

              c_lat = -90.0D0 * DtoR
              c_lon = 0.0D0 * DtoR
              !where (y .gt. 0)
              !   c_lon = 0.0D0 * DtoR
              !elsewhere
              !   c_lon = 180.0D0 * DtoR
              !endwhere

              rho = sqrt(x**2 + y**2)
              radius = earthRadius

              c = 2.0D0 * atan(rho / (2.0D0 * radius))

              lat = asin( cos(c) * sin(c_lat) + (y * sin(c) * cos(c_lat)) / rho)
              lon = c_lon + atan2( (x * sin(c)), (rho * cos(c_lat) * cos(c) - y * sin(c_lat) * sin(c)))

              ! ensure longitude is between 0 and 2*pi

              where (lon .lt. 0.0D0)
                lon = lon + 2.0D0 * pi
              endwhere
            endif
          endif

          if (allocated(rho)) deallocate(rho)
          if (allocated(radius)) deallocate(radius)
          if (allocated(c)) deallocate(c)
          if (allocated(c_lat)) deallocate(c_lat)
          if (allocated(c_lon)) deallocate(c_lon)
        endif
      endif

      return
    end subroutine ConvertSGToLatLon_vector

    !
    ! Converts lat,lon to StereoGraphic x,y (assumes alt = 0.0)
    ! lat and lon are input in Radians
    !
    subroutine ConvertToSGXY_scalar(lat, lon, x, y)
      implicit none
      double precision, intent(in) :: lat
      double precision, intent(in) :: lon
      double precision, intent(out) :: x
      double precision, intent(out) :: y
      double precision :: earthRadius, DtoR
      double precision :: c_lat, c_lon
      double precision :: k
      character(LEN=200) :: errmsg

      earthRadius = GetRadiusEarth()
      DtoR = GetDegToRad()

      x = 0.0D0
      y = 0.0D0

      ! define tangent point where projection plane contacts sphere (south_pole)

      c_lat = -90.0D0 * DtoR
      c_lon = 0.0D0 * DtoR

      k = (2.0D0 * earthRadius) / (1.0D0 + sin(c_lat) * sin(lat) + cos(c_lat) * cos(lat) * cos(lon - c_lon))

      x = k * cos(lat) * sin(lon - c_lon)

      y = k * (cos(c_lat) * sin(lat) - sin(c_lat) * cos(lat) * cos(lon - c_lon))

      return
    end subroutine ConvertToSGXY_scalar

    subroutine ConvertToSGXY_vector(lat, lon, x, y)
      implicit none
      double precision, dimension(0:), intent(in) :: lat
      double precision, dimension(0:), intent(in) :: lon
      double precision, dimension(0:), intent(out) :: x
      double precision, dimension(0:), intent(out) :: y
      double precision, allocatable :: k(:)
      double precision, allocatable :: radius(:)
      double precision, allocatable :: c_lat(:), c_lon(:)
      double precision, allocatable :: lon_diff(:)
      double precision :: earthRadius, DtoR
      integer :: np
      integer :: allocateStatus
      logical :: ok
      character(LEN=200) :: errmsg

      earthRadius = GetRadiusEarth()
      DtoR = GetDegToRad()

      x = 0.0D0
      y = 0.0D0

      np = size(lat)

      ok = size(lat) == np .and. size(lon) == np

      if (.not. ok) then
        write(errmsg,*)'Coordinate:ConvertToSGXY_vector(): ',&
        'The input lat and lon vectors are not the same length\0'
        call set_error_message(errmsg)
      else

        if (np .ge. 0) then
          allocate(k(0:np-1), STAT=allocateStatus)
          if (allocateStatus .gt. 0) then
            write(errmsg,*)'Coordinate:ConvertToSGXY_vector(): ',&
            'allocation error (k(',np,'))\0'
            call set_error_message(errmsg)
          endif

          allocate(radius(0:np-1), STAT=allocateStatus);
          if (allocateStatus .gt. 0) then
            write(errmsg,*)'Coordinate:ConvertToSGXY_vector(): ',&
            'allocation error (radius(',np,'))\0'
            call set_error_message(errmsg)
          endif

          allocate(c_lat(0:np-1), STAT=allocateStatus);
          if (allocateStatus .gt. 0) then
            write(errmsg,*)'Coordinate:ConvertToSGXY_vector(): ',&
            'allocation error (c_lat(',np,'))\0'
            call set_error_message(errmsg)
          endif

          allocate(c_lon(0:np-1), STAT=allocateStatus);
          if (allocateStatus .gt. 0) then
            write(errmsg,*)'Coordinate:ConvertToSGXY_vector(): ',&
            'allocation error (c_lon(',np,'))\0'
            call set_error_message(errmsg)
          endif

          allocate(lon_diff(0:np-1), STAT=allocateStatus);
          if (allocateStatus .gt. 0) then
            write(errmsg,*)'Coordinate:ConvertToSGXY_vector(): ',&
            'allocation error (lon_diff(',np,'))\0'
            call set_error_message(errmsg)
          endif

          if (.not. Did_an_Error_Occur()) then
            ! define tangent point where projection plane contacts sphere (south_pole)

            c_lat = -90.0D0 * DtoR
            c_lon = 0.0D0 * DtoR

            radius = earthRadius

            lon_diff = lon - c_lon

            k = (2.0D0 * radius) / (1.0D0 + sin(c_lat) * sin(lat) + cos(c_lat) * cos(lat) * cos(lon_diff))

            x = k * cos(lat) * sin(lon_diff)
            y = k * (cos(c_lat) * sin(lat) - sin(c_lat) * cos(lat) * cos(lon_diff))
          endif

          if (allocated(k)) deallocate(k)
          if (allocated(radius)) deallocate(radius)
          if (allocated(c_lat)) deallocate(c_lat)
          if (allocated(c_lon)) deallocate(c_lon)
          if (allocated(lon_diff)) deallocate(lon_diff)
        endif
      endif

      return
    end subroutine ConvertToSGXY_vector

    !*******************************************************
!     SUBROUTINE: Convert_ElAzAlt_To_XYZ
!     PURPOSE: This routine takes a coordinate in elevation,
!     azimuth, and altitude and returns the result in ECEF
!     XYZ coordinates.
!     ARGUMENTS:
!     Input: el - satellite elevation (degrees)
!     az - satellite azimuth (degrees)
!     alt - satellite altitude (kilometers)
!     rlat - reference site latitude (degrees)
!     rlon - reference site longitude (degrees)
!     ralt - reference site altitude (kilometers)
!     Output:X (kilometers)
!     Y (kilometers)
!     Z (kilometers)
!     RETURNS:none
!     CREATED:Don Tucker, 02/11/2004
!     MODIFIED:
!*******************************************************
    subroutine Convert_ElAzAlt_To_XYZ(elev,azim,alt,reflat,reflon,ralt,x,y,z)
      use Constants_mod
      !use Coordinate_mod
      
      implicit none
                                !arguments
      double precision, intent(in) :: elev
      double precision, intent(in) :: azim
      double precision, intent(in) :: alt
      double precision, intent(in) :: reflat
      double precision, intent(in) :: reflon
      double precision, intent(in) :: ralt
      double precision, intent(out) :: x
      double precision, intent(out) :: y
      double precision, intent(out) :: z
                                !constants
      double precision :: RtoD, DtoR, earthRadius, PI
                                !local variables
      double precision :: r0, r1, temp, range1, ca, test1, test2, test
      double precision :: a, cg, g, sa, lat, lon, el, az, rlat, rlon
      
      earthRadius = GetRadiusEarth()
      PI = GetPi()
      RtoD = GetRadToDeg()
      DtoR = GetDegToRad()
      
      el = elev * DtoR
      az = azim * DtoR
      rlat = reflat * DtoR
      rlon = reflon * DtoR
      
                                !get the range to the satellite
      r0 = earthRadius + ralt
      r1 = earthRadius + alt
      temp = r0/r1*cos(elev)
      if(temp .gt. 1.0D0) temp = 1.0D0
      range1 = acos(temp) - elev
                                !Get the latitude and longitude coordinate of the
                                !satellite
      ca = sin(rlat)*cos(range1) + cos(rlat)*sin(range1)*cos(az)
      test1 = -1.0D0
      test2 = -1.0D0*test1
      if(ca .gt. test1) then
         test = ca
      else
         test = test1
      endif
      if(test .lt. test2) then
         ca = test
      else
         ca = test2
      endif
      a = acos( ca )
      lat = PI/2.0 - a
      cg = (cos(range1) - sin(rlat)*ca)/(cos(rlat)*sin(a))
      if(cg .gt. test1) then
         test = cg
      else
         test = test1
      endif
      if(test .lt. test2) then
         cg = test
      else
         cg = test2
      endif
      g = acos(cg)
      sa = sin(az)
      if(sa .ge. 0.0D0) then
         lon = MOD((rlon + g),(2.0D0*PI))
      else
         lon = MOD((rlon - g),(2.0D0*PI))
      endif
      
                                !convert satellite lat lon alt into XYZ
      x = r1 * cos(lat) * cos(lon)
      y = r1 * cos(lat) * sin(lon)
      z = r1 * sin(lat)
      
      return
    end subroutine Convert_ElAzAlt_To_XYZ
      
    SUBROUTINE getazel(rvec,svec,ovec)
      use Constants_mod
      !use Coordinate_mod
      implicit none
      double precision, dimension(0:2), intent(in) :: rvec, svec
      double precision, dimension(0:1), intent(inout) :: ovec
!     INPUT:
!     rvec: 3 vector of receiver latitude (radians), longitude (radians)
!     and height above earth (km)
!     svec: same as rvec, but for satellite position
!     OUTPUT:
!     returns azimith and elevation angle as [az,beta]
      double precision :: rad_earth,xr1,yr1,zr1,xs1,ys1,zs1, range1, az, r0, r1
      double precision :: arc_len, arc_len_min, cb, beta
      integer :: earth_sphere
      
      rad_earth = GetRadiusEarth() !6371.0
      earth_sphere=1
      xr1 = rvec(0)             ! receiver latitude
      yr1 = rvec(1)             ! receiver longitude
      zr1 = rvec(2)             ! height of receiver above earth
      xs1 = svec(0)             ! satellite latitude
      ys1 = svec(1)             ! satellite longitude
      zs1 = svec(2)             ! height of satellite above earth
      
      CALL ll2raz_scalar(xr1*1.0D0, yr1*1.0D0, xs1*1.0D0, ys1*1.0D0, range1, az)
      IF (earth_sphere.EQ.1) THEN
         r0 = rad_earth+zr1
         r1 = rad_earth+zs1
      ELSE
                                !r0 = r_earth(xr1*GetRadToDeg())+zr1
                                !r1 = r_earth(xs1*GetRadToDeg())+zs1
      END IF
      arc_len = SQRT(r0**2.+r1**2. - 2.0D0*r0*r1*cos(range1))
      IF (r1.GT.r0) THEN
         arc_len_min = SQRT(r1**2.-r0**2.)
         cb = r1*sin(range1)/arc_len
         IF (cb.GT.1.) cb = 1.  ! rare numerical error case
         IF (cb.LT.-1.) cb = -1.
         beta = acos(cb)
         IF (arc_len.GT.arc_len_min) THEN
            beta = -1.0*beta
         END IF
      ELSE
         cb = r1*sin(range1)/arc_len
         beta = -1.0*acos(cb)
      END IF
      ovec(0) = az
      ovec(1) = beta
    END SUBROUTINE getazel
      
      
    SUBROUTINE ll2raz_scalar (lati, loni, lato, lono, range1, az)
      use Constants_mod
      !use Coordinate_mod
      implicit none
      double precision :: lati,loni,lato,lono,range1,az
!     Converts from lat and lon to azimuth and range
!     Gary Bust -- 9.11.97
!     azimuth is between -pi and pi.
      double precision :: pib2, sin1, cos1, cos2, s2, ca, cr
      
      pib2 = GetPi()/2.0
      
                                ! n_pole:
      IF (ABS(lati - pib2).GT.1.0e-6) THEN !goto, s_pole
                                ! s_pole:
         IF (ABS(lati + pib2).GT.1.0e-6) THEN !goto, the_same
                                ! the_same:
            IF ((abs(lati-lato).GT.1.0e-6).OR.(abs(loni-lono).GT.1.0e-6)) THEN ! goto, general
                                ! general:
               sin1 = sin(lati)
               cos1 = cos(lati)
               s2 = sin(lato)
               cr = sin1*s2+cos1*cos(lato)*cos(lono-loni)
               IF (cr.GT.1.0) cr = 1.0D0
               IF (cr.LT.-1.0) cr = -1.0D0
               range1 = acos(cr)
               IF (abs(range1).LT.1.e-6) THEN
                  az = 0.0D0
               ELSE
                  ca = (s2 - sin1*cr)/(cos1*sin(range1))
                  IF (ca.GT.1.0) ca = 1.0D0
                  IF (ca.LT.-1.0) ca = -1.0D0
                  az = acos(ca)
               END IF
               if (sin(lono-loni).LT.0.0) az = 2.0*GetPi() - az 
               if (az.GT.GetPi()) az = az - 2.0*GetPi()
               RETURN
            END IF
            range1 = 0.0
            az = 0.0
                                !PRINT *,'same'
            RETURN
         END IF
         range1 = pib2 + lato
         az = 0.0
                                !PRINT *,'s_pole'
         RETURN
      END IF
      range1 = pib2 - lato
      az = GetPi()
                                !PRINT *,'n_pole'
      RETURN
    END SUBROUTINE ll2raz_scalar
      
    SUBROUTINE getlatlon_scalar (z,rvec,beta, azimuth,TanMinus, ovec)
      use Constants_mod
      !use Coordinate_mod
      implicit none
      double precision :: z
      double precision, dimension (0:2), intent(in) :: rvec
      double precision :: beta, azimuth
      integer :: TanMinus
      double precision, dimension(0:1) :: ovec
!     INPUTS:
!     z:	height above earth lat and lon is desired (km)
!     rvec: receiver station vector
!     beta: elevation angle of ray (radians)
!     azimuth: azimuth of ray (radians)
!     tanminus: when beta < 0, there are two possible solutions.
!     one for distances less than the tangent point
!     one for distances larger than the tangent point
!     the DEFAULT is for larger than tangent point.
!     if want less, set TanMinus to 1
!     OUTPUTS:
!     returns lat and longitude of intercept in radians as [x,y]
      double precision :: r_earth, xr, yr, zr, r0, r1, cgamma, az, x, y, test
      
      r_earth = GetRadiusEarth() !6371.0
      
      xr = rvec(0)
      yr = rvec(1)
      zr = rvec(2)
      r0 = r_earth + zr
      r1 = r_earth + z
      test = r0/r1*cos(beta)
      IF (test.GT.1.0D0) test = 1.0D0
      IF ((TanMinus.EQ.1).AND.(beta.LT.0.0)) THEN
         cgamma = -1.0*acos(test) - beta 
      ELSE 
         cgamma = acos(test) - beta
      END IF
      az = azimuth
      CALL raz2ll_scalar(xr,yr,cgamma,az,x,y)
      ovec(0) = x
      ovec(1) = y
      
      RETURN
    END SUBROUTINE getlatlon_scalar
     
    SUBROUTINE raz2ll_scalar (lati, loni, range1, az, lato, lono)
      use Constants_mod
      !use Coordinate_mod
      implicit none
      double precision :: lati,loni,range1,az,lato,lono   
!     Converts range, az from given lati,loni to lato,lono
!     range in radians.  east longitude.
!     Liz Klodginski  --  4.29.97
      double precision :: pi, ca, test1, test2, test, a, cg, g, sa
      
      pi = acos(-1.0D0)         !GetPi()
      ca = sin(lati)*cos(range1) + cos(lati)*sin(range1)*cos(az)
      test1 = -1.0 
      test2 = -1.*test1
      IF (ca.GT.test1) test = ca
      IF (ca.LE.test1) test = test1
      IF (test.LT.test2) ca = test
      IF (test.GE.test2) ca = test2
      a = acos( ca )
      cg = (cos(range1) - sin(lati)*ca)/(cos(lati)*sin(a))
      IF (cg.GT.test1) test = cg
      IF (cg.LE.test1) test = test1
      IF (test.LT.test2) cg = test
      IF (test.GE.test2) cg = test2
      g = acos(cg)
      lato = GetPi()/2.0 - a
      lono = lato
      sa = sin(az)
      test2 = 0.D0
      IF (sa.GE.test2) lono = mod ((loni+g),(2.0*GetPi())) 
      IF (sa.LT.test2) lono = mod ((loni-g),(2.0*GetPi())) 
      RETURN
    END SUBROUTINE raz2ll_scalar
            
    subroutine print_coordinate_to_file(outputFile)
      implicit none
      character(LEN=*), intent(in) :: outputFile
      character(LEN=256) :: outfile
      integer :: ios
      integer :: mycount
      character(LEN=200) :: errmsg

      if (outputFile .eq. '') then
        outfile = "./IDA3DCoordinate.out"
      else
        outfile = outputFile
      endif

      if (outfile .ne. '') then
        open(unit=12, file=outfile, form='FORMATTED', iostat=ios)
        if (ios .ne. 0) then
          write(errmsg,*)'Coordinate:print_coordinate_to_file(): ',&
          'Incorrect output file specification (',outfile,').\0'
          call set_error_message(errmsg);
          close(unit=12)
        else if (ios .eq. 0) then
          write(12,*)'# Notes here'
          close(unit=12)

          call print_constants_to_file(outputFile)
          if (Did_an_Error_Occur()) then
            call add_trace_to_error_message('print_coordinate_to_file()\0')
            !'
          endif
        endif
      endif

      return
    end subroutine print_coordinate_to_file

end module Coordinate_mod

  
!
! Subroutines that are used by C functions that handle Qhull in order to access functions in
! the Coordinates module
! lat and lon are input in Degrees
!
subroutine convert_to_euclid_xyz(lat, lon, alt, x, y, z)
  use Constants_mod
  use Coordinate_mod

  implicit none
  double precision, intent(in) :: lat
  double precision, intent(in) :: lon
  double precision, intent(in) :: alt
  double precision, intent(out) :: x
  double precision, intent(out) :: y
  double precision, intent(out) :: z
  double precision :: DtoR
  double precision :: lat_rad
  double precision :: lon_rad

  DtoR = GetDegToRad()

  lat_rad = lat * DtoR
  lon_rad = lon * DtoR

  call ConvertToXYZ(lat_rad, lon_rad, alt, x, y, z)

  return
end subroutine convert_to_euclid_xyz

!
! lat and lon are output in Degrees
!
subroutine convert_euclid_to_latlonalt(x, y, z, lat, lon, alt)
  use Constants_mod
  use Coordinate_mod

  implicit none
  double precision, intent(in) :: x
  double precision, intent(in) :: y
  double precision, intent(in) :: z
  double precision, intent(out) :: lat
  double precision, intent(out) :: lon
  double precision, intent(out) :: alt
  double precision :: RtoD

  call ConvertToLatLonAlt(x, y, z, lat, lon, alt)

  RtoD = GetRadToDeg()

  lat = lat * RtoD
  lon = lon * RtoD

  return
end subroutine convert_euclid_to_latlonalt


!
! lat and lon are input in Degrees
!
subroutine convert_to_stereog_xy(lat, lon, x, y)
  use Constants_mod
  use Coordinate_mod

  implicit none
  double precision, intent(in) :: lat
  double precision, intent(in) :: lon
  double precision, intent(out) :: x
  double precision, intent(out) :: y
  double precision :: DtoR
  double precision :: lat_rad
  double precision :: lon_rad

  DtoR = GetDegToRad()

  lat_rad = lat * DtoR
  lon_rad = lon * DtoR

  call ConvertToSGXY(lat_rad, lon_rad, x, y)

  return
end subroutine convert_to_stereog_xy

!
! lat and lon are output in Degrees
!
subroutine convert_stereog_to_latlon(x, y, lat, lon)
  use Constants_mod
  use Coordinate_mod

  implicit none
  double precision, intent(in) :: x
  double precision, intent(in) :: y
  double precision, intent(out) :: lat
  double precision, intent(out) :: lon
  double precision :: RtoD

  call ConvertSGToLatLon(x, y, lat, lon)

  RtoD = GetRadToDeg()

  lat = lat * RtoD
  lon = lon * RtoD

  return

end subroutine convert_stereog_to_latlon

!
! Subroutines that are used by C functions that handle Qhull in order to access functions in
! the Coordinates module.  I renamed these because, even though they should be here, they only
! seem to be seen when in Qhull_Define.f.  Therefore, I renamed these functions by adding an
! 'a' in front of all of the function names.
!

!
! lat and lon are input in Degrees
!
subroutine aconvert_to_euclid_xyz(lat, lon, alt, x, y, z)
  use Constants_mod
  use Coordinate_mod

  implicit none
  double precision, intent(in) :: lat
  double precision, intent(in) :: lon
  double precision, intent(in) :: alt
  double precision, intent(out) :: x
  double precision, intent(out) :: y
  double precision, intent(out) :: z
  double precision :: DtoR
  double precision :: lat_rad
  double precision :: lon_rad

  DtoR = GetDegToRad()

  lat_rad = lat * DtoR
  lon_rad = lon * DtoR

  call ConvertToXYZ(lat_rad, lon_rad, alt, x, y, z)

  return
end subroutine aconvert_to_euclid_xyz

!
! lat and lon are output in Degrees
!
subroutine aconvert_euclid_to_latlonalt(x, y, z, lat, lon, alt)
  use Constants_mod
  use Coordinate_mod

  implicit none
  double precision, intent(in) :: x
  double precision, intent(in) :: y
  double precision, intent(in) :: z
  double precision, intent(out) :: lat
  double precision, intent(out) :: lon
  double precision, intent(out) :: alt
  double precision :: RtoD

  call ConvertToLatLonAlt(x, y, z, lat, lon, alt)

  RtoD = GetRadToDeg()

  lat = lat * RtoD
  lon = lon * RtoD

  return
end subroutine aconvert_euclid_to_latlonalt


!
! lat and lon are input in Degrees
!
subroutine aconvert_to_stereog_xy(lat, lon, x, y)
  use Constants_mod
  use Coordinate_mod

  implicit none
  double precision, intent(in) :: lat
  double precision, intent(in) :: lon
  double precision, intent(out) :: x
  double precision, intent(out) :: y
  double precision :: DtoR
  double precision :: lat_rad
  double precision :: lon_rad

  DtoR = GetDegToRad()

  lat_rad = lat * DtoR
  lon_rad = lon * DtoR

  call ConvertToSGXY(lat_rad, lon_rad, x, y)

  return
end subroutine aconvert_to_stereog_xy

!
! lat and lon are output in Degrees
!
subroutine aconvert_stereog_to_latlon(x, y, lat, lon)
  use Constants_mod
  use Coordinate_mod

  implicit none
  double precision, intent(in) :: x
  double precision, intent(in) :: y
  double precision, intent(out) :: lat
  double precision, intent(out) :: lon
  double precision :: RtoD

  call ConvertSGToLatLon(x, y, lat, lon)

  RtoD = GetRadToDeg()

  lat = lat * RtoD
  lon = lon * RtoD

  return

end subroutine aconvert_stereog_to_latlon
