
function [sun_up_down] = Day_or_night_fxn (hrut, doy,glats,glons,alts) 
% chicrit is the zenith angle below which the Sun is visible.
% For points on the surface this is just pi/2, but at higher
% altitudes it is bigger.
% idea from SAMI2 physics model code ---
% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04
   
day = doy;
%glats =-33.30 ; glons =26.50;
%glats =33.43 ; glons =126.30;
%33.43°N	126.30°E
m2km=1e-3;
pie     = pi;  
re      = 6370.0;
alts    = alts.*m2km;
po180  = 1.745329e-02; %degree to radian (pi/180)
rtod = 180/pi;
dayve = 80.;
sidyr = 365.4;
solinc = 23.5;
sun_up_down = zeros(size(alts));

  hrl = hrut + glons/15.;
 hrl(hrl> 24) =  hrl(hrl> 24)-24;


 sdec  = rtod * asin ( sin (2.*pie*(day-dayve)/sidyr)* sin (solinc/rtod) );
         cossdec      = cos ( po180 * sdec ); % cos of sdec in radian 
         sinsdec      = sin ( po180 * sdec ); % sin of sdec  in radian 
         clat         = cos ( po180 .* glats ); % cos of lat 
         slat         = sin ( po180 .* glats);  % sin of lat 
     
         cx           =   (slat .* sinsdec) - clat .* cossdec .* cos ( 15.0.*po180.*hrl );
        

% MS: Since we will be taking acos of this value in photprod, make
% sure that the absolute value does not minutely exceed 1 because of
% round-off error.

        tm_cx = abs(abs(cx)-1);
        sgn = sign(cx);
        cx ( tm_cx <1.e-6 & sgn < 1)=1;

        

        
        
coschicrit = cos(pie - asin( 1./ (1. + alts./re) ));
coschi = cx;
sun_up_down(coschi > coschicrit)=1 ;



