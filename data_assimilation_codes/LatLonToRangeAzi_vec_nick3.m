function [range2, az] = LatLonToRangeAzi_vec_nick3(lati, loni, lato, lono)
  
    
      
        npts = numel(lati);
        nptsj = numel(lato);
        range2 = zeros(1,nptsj);
     
        az = zeros(1,nptsj);

         pib2 = pi/2.0;
       
         % do the following only if grid points do exist 
      
            
                % n_pole test case 
                % pib2 = 180/2 = 90 deg which is the north pole in 
                % latitude so difference in latitude is all those points 
                % subtract the refrence point Lato
               
                idex =  ((abs(lati)-pib2) < 1e-6);
       
                if ~ isempty(range2(idex)) 
                  range2(:) = pib2 - lato;
                  az(:)= pi;
                end 
              %  clear idex 
                 % lets do the same for the south pole 
                 % but if north pole is +ve, South pole should be -ve 
                 % so all points that -pi + points ~ 0 should be at the South pole
                 
                 idex = (abs(lati + pib2) < 1e-6);
                 
                 if ~ isempty(range2(idex)) 
  
                   range2(:) = pib2 + lato(:);
                   az(:) = 0.0;
                   
                 end 
                % clear idex
                 % check if both lats and loni points are at the north pole 
                  
                 idex = ((abs(lati - lato) < 1e-6) & (abs(loni - lono) < 1e-6));
                 
                 if ~ isempty(lato(idex)) 
                   
                   range2(idex) =0;
                   az(idex)=0;
                   
                 end 
                
                 %clear idex
                 
                 % points dont lie at the pole general case 
 
         idex = ((abs(repmat(lati- pib2, size(lato))) > 1e-6 ) & (repmat(abs(lati + pib2),size(lato))> 1e-6) & ...
                 ((abs(lati - lato)> 1e-6) | (abs(loni - lono) > 1e-6)));
                 
              
                     
               if ~ isempty(lato(idex)) 
                    
                    sin1 = sin(lati);
                    cos1 = cos(lati);
                    s2 = sin(lato(idex));

                    cr = (sin1.* s2) + (cos1.*cos(lato(idex))).* cos((loni.*-1) + lono(idex));
                 
                    range2(idex) = acos(cr);
                  
                    ca1 = (s2 - sin1.* cr);
                    ca2 = (cos1.* sin(range2(idex)))';
                    ca = ca1./ca2;
                    ca(ca < -1.0) = -1.0;
                    ca(ca >1.0) =  1.0;

                    az(idex) = acos(ca);
                    
                    Zs  = az(idex); 
                    ranges =  range2(idex);
                    lonos =   lono(idex);
                    
                    idexs = sin((loni.*-1) + lonos)< 0.0;
                  
                    Zs(idexs) = (Zs(idexs) - (2.0*pi));
                    
                    az(idex) = Zs;
              end 

            %  clear idex

            