function  [hcorrPtr]= ComputeHorizontalCorr3 (PGM_location,All_lat,All_HGT300,All_lon,GetLatScale,GetLonScale)
  
%*******************************************************
% This function computes the correlations between the points
% in the horizontal grid plane.  Pay attention to the units
% in self.latCorrUnits and self.lonCorrUnits which determine
% how we handle distances.
% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04
%********************************************************


useLatCorr = 1;
useLonCorr = 1;
%useAltCorr = 1;

% Some tolerances
ExpMax = 20.0;
small = 0.0000001;
big = 1.0/small; 
% covert Deg 2 rad 
Deg2Rad = 3.1415926535897931/180.0;
% get total number of grid points in Horizontal plane 
numPlane  = numel(All_lat(:))* numel(All_lon(:));

% asign space for magnetic cordinates 
% 1st column is Geomagnetic Lat 
% 2nd column is Geomagnetic Lon 
magcord = zeros(numPlane,2);

% Covert geographic to geomagnetic
%[geomLon,geomLat] = meshgrid(All_lon,All_lat)

[geomLat, geomLon] = geog_2_geom_nick (PGM_location,All_lat,All_HGT300,All_lon);

% Covert the degrees to radians 
magcord(:,1) = geomLat(:).*Deg2Rad;
magcord(:,2) = geomLon(:).*Deg2Rad;


%clear geomLat geomLon 
% Intialize the gammamin with a dummy value higher than expected 

gammamin = 10000000.00; 

% solar terminator to be revised later 
% to include whether a point is in night or sun side 
% for the start all points lie in the same zone

dailyIndexVecPtr = ones(1,numPlane);


    for i=1 :numPlane
       magLat1 =  magcord (i,1);
       magLon1 = magcord (i,2);

       [gammavec,~] = LatLonToRangeAzi_vec_nick (magcord(:,1), magcord(:,2), magLat1,magLon1);
     
       gamma = min(gammavec(gammavec > 0.0001));
  
       if (gamma < gammamin) 
           gammamin = gamma;
       end 
       
        
    end
    
    
   
    
% allocate space to hold the horizontal correlations 
hcorrPtr = zeros(numPlane,numPlane);
  
    
    if (gammamin < 2.0*pi/180)
        gammamin =  2.0*pi/180;
    end

% Minimum correlation value allowed - used to default to in certain cases
    L2_small = (gammamin)^2.0;
   for i=1:numPlane 
       magLat1 =  magcord (i,1);
       magLon1 = magcord (i,2);

      l_scale_1 = GetLatScale*pi/180;     %GetLatScale(this, magLat1, i)
      lon_scale_1 = GetLonScale*pi/180;%GetLonScale(this, magLat1, i)   
      
      
%      for j=1:numPlane
       xx = 0.D0;
%       magLat2 =  magcord (j,1);
%       magLon2 = magcord (j,2);
         l_scale_2 = GetLatScale*pi/180;%(this, magLat2, j)
         lon_scale_2 =GetLonScale*pi/180;% GetLonScale(this, magLat2, j)
         % get the azimuths from 1-2 and from 2-1. great circle distance is the same but
         % azimuths are not 180 degrees apart on a sphere% so for symmetry need both.
%         [~,az1] = LatLonToRangeAzi_vector( magLat1, magLon1 , magLat2,  magLon2)
%         [gamma,az2] = LatLonToRangeAzi_vector(magLat2,  magLon2, magLat1, magLon1)
% 
%           [~,az1]  = LatLonToRangeAzi_vec_nick ( magLat1, magLon1, magLat2,  magLon2);
%           [gamma,az2] =  LatLonToRangeAzi_vec_nick (magLat2,  magLon2, magLat1, magLon1);
%           
           
           [~,az1x] = LatLonToRangeAzi_vec_nick3(magLat1,  magLon1, magcord (:,1),magcord (:,2));
           
           [gammax,az2x] =  LatLonToRangeAzi_vec_nick (magcord (:,1),magcord (:,2),magLat1, magLon1);
           
   
       
        

        % for the case of both lat and lon correlations, we have to 
         % adjust each correlation length by the direction.
         oneByL2 = (((cos(az1x)).^2)./l_scale_1^2) + ((sin(az1x)).^2)./(lon_scale_1^2);
         Ll1x = (oneByL2).^(-.5);
         oneByL2 = (((cos(az2x)).^2)./l_scale_2^2) + ((sin(az2x)).^2)./(lon_scale_2^2);
         Ll2x = (oneByL2).^(-.5);
         
         % for the case where we only have lats or lons
         Llat2x = l_scale_1*l_scale_2;
         Llon2x = lon_scale_1*lon_scale_2;
     
         
        % crossing terminator - small
%         if (dailyIndexVecPtr(i) ~= dailyIndexVecPtr(i)) 
%            Llat2 = L2_small;
%            Llon2 = L2_small;
%            Ll1 = sqrt(L2_small);
%            Ll2 = sqrt(L2_small);
%         end

    
    
              if((useLatCorr==1)&&(useLonCorr==1)) 
                  botm = (Ll1x(:).*Ll2x(:));
                  
                  xx = ((gammax(:)).^2)./botm(:);
              end 

               xx(xx>ExpMax) = ExpMax;
  
           hcorrPtr(i,:) = exp(-1.*xx);
%%%%
%%%%       %     end 
%%%%      % Check the diagonal terms and reset just in case
%%%%
%%%%      GHTY = [];
%%%%      
%%%%      for j=1:numPlane
%%%%       xx = 0.D0;
%%%%       magLat2 =  magcord (j,1);
%%%%       magLon2 = magcord (j,2);
%%%%         l_scale_2 = GetLatScale*pi/180;%(this, magLat2, j)
%%%%         lon_scale_2 =GetLonScale*pi/180;% GetLonScale(this, magLat2, j)
%%%%         % get the azimuths from 1-2 and from 2-1. great circle distance is the same but
%%%%         % azimuths are not 180 degrees apart on a sphere% so for symmetry need both.
%%%%%         [~,az1] = LatLonToRangeAzi_vector( magLat1, magLon1 , magLat2,  magLon2)
%%%%%         [gamma,az2] = LatLonToRangeAzi_vector(magLat2,  magLon2, magLat1, magLon1)
%%%%% 
%%%%           [~,az1]  = LatLonToRangeAzi_vec_nick ( magLat1, magLon1, magLat2,  magLon2);
%%%%           [gamma,az2] =  LatLonToRangeAzi_vec_nick (magLat2,  magLon2, magLat1, magLon1);
%%%%
%%%%
%%%%         % for the case of both lat and lon correlations, we have to 
%%%%         % adjust each correlation length by the direction.
%%%%         oneByL2 = (((cos(az1))^2)/l_scale_1^2) + ((sin(az1))^2)/(lon_scale_1^2);
%%%%         Ll1 = sqrt(1.0/oneByL2);
%%%%         oneByL2 = (((cos(az2))^2)/l_scale_2^2) + ((sin(az2))^2)/(lon_scale_2^2);
%%%%         Ll2 = sqrt(1.0/oneByL2);
%%%%                   
%%%%         % for the case where we only have lats or lons
%%%%         Llat2 = l_scale_1*l_scale_2;
%%%%         Llon2 = lon_scale_1*lon_scale_2;
%%%%        % crossing terminator - small
%%%%         if (dailyIndexVecPtr(i) ~= dailyIndexVecPtr(j)) 
%%%%            Llat2 = L2_small;
%%%%            Llon2 = L2_small;
%%%%            Ll1 = sqrt(L2_small);
%%%%            Ll2 = sqrt(L2_small);
%%%%         end
%%%%  
%%%% 
%%%%%% select which correlations to compute (lon, lat or alt)
%%%%%         if ((useLatCorr==0)&&(useLonCorr==1)) 
%%%%%            if(abs(magLat1 - magLat2)< small)
%%%%%               xx = ((gamma)^2)/Llon2;
%%%%%            end 
%%%%%         end 
%%%%%         
%%%%%         if ((useLatCorr==1)&&(useLonCorr==0)) 
%%%%%            if (abs(magLon1 - magLon2)<small) 
%%%%%               xx = ((gamma)^2)/Llat2;
%%%%%            end
%%%%%         end
%%%%%         
%%%%         if((useLatCorr==1)&&(useLonCorr==1)) 
%%%%
%%%%            xx = (gamma)^2/(Ll1*Ll2);
%%%%         end 
%%%%         
%%%%          
%%%%         if (xx>ExpMax)
%%%%             xx = ExpMax;
%%%%         end 
%%%%         GHTY = [GHTY;  xx ];
%%%%        hcorrPtr(i,j) = exp(-1.*xx);
%%%%
%%%%  end
%%%%  
%%%% 
%%%%      size(xxx)
%%%%      plot(xxx,'*-r')
%%%%      hold on 
%%%%      grid on 
%%%%      
%%%%      plot(GHTY,'o-b')
%%%%      
%%%%      hold off
%%%%      pause 











      hcorrPtr(i,i) = 1.0  ;    
   end 
