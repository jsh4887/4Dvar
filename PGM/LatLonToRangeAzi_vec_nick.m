function [range2, az] = LatLonToRangeAzi_vec_nick(lati, loni, lato, lono)
      

      range2 = zeros(length(lati));
      az =zeros(length(lati));

      npts = length(lati);

 

  
       pib2 = pi/2.0;
     
       % do the following only if grid points do exist 
    
          
              % n_pole test case 
              % pib2 = 180/2 = 90 deg which is the north pole in 
              % latitude so difference in latitude is all those points 
              % subtract the refrence point Lato
             
              idex =  (abs(lati)-pib2) < 1e-6);
              
              if ~ isempty(range2(idex)) 
                
                range2(idex) = pib2 - lato;
                az(idex) = pi;
                
              
              end 
              clear idex 
               % lets do the same for the south pole 
               % but if north pole is +ve, South pole should be -ve 
               % so all points that -pi + points ~ 0 should be at the South pole
               
               idex = (abs(lati + pib2) < 1e-6);
               
               if ~ isempty(range2(idex)) 
                 
                 range2(idex) = pib2 + lato;
                 az(idex) = 0.0 ;
                 
               end 
               clear idex
               % check if both lats and loni points are at the north pole 
                
               idex = ((abs(lati - lato) < 1e-6) & (abs(loni - lono) < 1e-6));
               
               if ~ isempty(range2(idex)) 
                 
                 range2(idex) =0;
                 az(idex)=0;
                 
               end 
               
               clear idex
               
               % points dont lie at the pole general case 
               
               idex = ((abs(lati- pib2) > 1e-6 ) & (abs(lati + pib2)> 1e-6) & ...
               ((abs(lati - lato)> 1e-6) | (abs(loni - lono) > 1e-6)))
               
             if ~ isempty(range2(idex)) 
                  
              sin1 = sin(lati(idex));
              cos1 = cos(lati(idex));
              s2 = sin(lato);
              cr = (sin1.* s2) + (cos1.*cos(lato)).* cos((loni(idex).*-1) + lono);
              range2(idex) = acos(cr);
              ca = (s2 - sin1.* cr) ./ (cos1.* sin(range2(idex)));
              ca(ca < -1.0) = -1.0;
              ca(ca >1.0) =  1.0;
              az(idex) = acos(ca);
              az(sin(lono - loni(idex))< 0.0) = az(idex) - 2.0*pi
              end 
              clear idex
            