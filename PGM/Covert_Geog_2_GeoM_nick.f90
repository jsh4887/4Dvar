program Covert_Geog_2_GeoM_nick
 implicit none
double precision, allocatable :: latggv(:),longgv(:)
double precision, allocatable ::vec_gmv(:,:)
integer:: num_in_vec,ios,allocatedStatus, i
double precision :: geo_lat, geo_lon, Alti


OPEN (UNIT=4, FILE='geo_lat_lon_vector.txt', FORM='FORMATTED', IOSTAT=ios)
read (4,*)num_in_vec, Alti
allocate(latggv(0:num_in_vec-1), STAT=allocatedStatus)
allocate(longgv(0:num_in_vec-1), STAT=allocatedStatus)
allocate(vec_gmv(0:num_in_vec-1,0:num_in_vec-1), STAT=allocatedStatus)


do i = 0,num_in_vec-1
    read (4,*)geo_lat, geo_lon
    latggv(i) = geo_lat
    longgv(i) = geo_lon
end do

call GeogToGeom_vector(latggv, longgv,Alti, vec_gmv)

close (unit=4)
! produce a fort.26 file once the code is done running
! this will help the wrapper to know that this process is finished and the data is ready
write(26,*)2

contains
    SUBROUTINE GeogToGeom_vector(latggv, longgv,Alti,vec_gmv)
      double precision, dimension(0:), intent(in) :: latggv
      double precision, dimension(0:), intent(in) :: longgv
      double precision, dimension(0:,0:), intent(inout) :: vec_gmv
      double precision :: lat0, lon0
      double precision :: latgg, longg
      double precision :: latgm, longm
      double precision :: slatgm, slongm
      double precision :: clatgm, clongm
      double precision :: pi, radToDeg, degToRad, Alti
      integer :: i, j, k
      integer :: np
      integer :: status
      logical :: ok
      character(LEN=200) :: errmsg
!      double precision, allocatable, target, dimension(:,:), save :: points_gmv
      double precision, allocatable, dimension(:,:) :: points_gmv
      real*4 :: in_lat,in_lon,h,out_lat,out_lon
      integer :: mgflag, i_err
      pi = 3.1415926535897931
      radToDeg = 180.0D0/pi
      degToRad = pi/180.0D0

      ok = ((size(latggv)).EQ.(size(longgv)))
      if (.not. ok) then
!        write(errmsg,*)'Coordinate:GeogToGeom_vector(): The input ',&
!        'lat and lon vectors are not the same length\0'
!        call set_error_message(errmsg)
      else
        np = size(latggv)

        if (np .ge. 0) then
          allocate(points_gmv(0:1,0:np-1), STAT=status)
          if (status .gt. 0) then
!            write(errmsg,*)'Coordinate:GeogToGeom_vector(): ',&
!            'allocation error (points_gmv(2,',np,'))\0'
!            call set_error_message(errmsg)
!            np = 0
          endif
        endif

        do i=0,np-1
          in_lat = latggv(i)!*radToDeg ! input latitude
          in_lon = longgv(i)!*radToDeg ! input longitude
          h = Alti  ! height
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
!          out_lat = in_lat!*degToRad
!          out_lon = in_lon!*degToRad
          IF (i_err.NE.0) THEN
!             WRITE(errmsg,*) 'GeogToGeom_vector::',&
!             'SFC$$CONVERT_GEO_COORD ERROR -- ',i_err
!             PRINT *,errmsg
!             print *,' i ilon olon ',i,in_lon,out_lon
!             CALL set_error_message(errmsg)
             RETURN
          END IF
          ! convert lon to 0 to 360
          IF (out_lon.LT.0.0D0) out_lon = out_lon + 360.0D0
          ! convert to radians
          points_gmv(0,i) = out_lat!*degToRad
          points_gmv(1,i) = out_lon!*degToRad
          vec_gmv(0,i) = points_gmv(0,i)
          vec_gmv(1,i) = points_gmv(1,i)
          WRITE(24,7117) in_lat, in_lon ,vec_gmv(0,i), vec_gmv(1,i)
7117    FORMAT(F10.6, 3x, F10.6,3x,F10.6, 3x, F10.6)
        END DO
        print*,'** Finished coverting geographic to geomagnetic **'
        deallocate(points_gmv)
       END IF

       RETURN
    END SUBROUTINE GeogToGeom_vector



End program Covert_Geog_2_GeoM_nick




















