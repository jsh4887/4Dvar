function [geomLat, geomLon] = geog_2_geom_nick (PGM_location,All_lat,HGT,All_lon)

if ispc 
     slashx = '\';
elseif isunix 
    slashx = '/';
end

%formulate the  path to PGM folder 
PGM_PATH = [PGM_location, slashx];
 
 
% Latitude 
latin = All_lat;  

% Longitude 
lonin = All_lon; 

% height 
Alti = HGT;


[lonmat,latmat]= meshgrid(lonin,latin);
% number of points in the horizontal plane 
numPlane = numel(lonmat(:));

if (exist([PGM_PATH,'geo_lat_lon_vector.txt'],'file') == 2)
    delete([PGM_PATH,'geo_lat_lon_vector.txt'])
end 

fid = fopen ([PGM_PATH,'geo_lat_lon_vector.txt'],'w');


if fid > 0 
  
    formatSpec = '%d %d\n';
    fprintf(fid, formatSpec, numPlane, Alti)
    
    for j = 1:numPlane 
        fprintf(fid, formatSpec, latmat(j), lonmat(j))
    end 
    
    fclose(fid);

    if (exist([PGM_PATH,'fort.26'],'file') == 2)
       delete([PGM_PATH,'fort.26'])
    end 

    if (exist([PGM_PATH,'fort.24'],'file') == 2)
       delete([PGM_PATH,'fort.24'])
    end 

    if ispc 
        y = dos ([PGM_PATH,'PGMGeog_2_GeoM_nick.exe  &']);
    elseif isunix 
        cd (PGM_PATH)
        system([PGM_PATH,'PGMGeog_2_GeoM_nick'])
    end 
    clear lonmat latmat
    geomLat = zeros (numPlane,1);
    geomLon = zeros (numPlane,1);
    
    while 1 

          if (exist([PGM_PATH,'fort.26'],'file') == 2)
              fid1 = fopen([PGM_PATH,'fort.24']);
              out = textscan (fid1, '%f %f %f %f');
              fclose all; 
              geomLat = out{3};
              geomLon = out{4};
              delete([PGM_PATH,'fort.24'])
              delete([PGM_PATH,'fort.26'])


            cd ..
              break
          end 
          
    end 
    
    
else 
     fclose all
        geomLat = [];
        geomLon = [];
    errordlg('failed to create geographic coordinates file to be read by XGeog_2_GeoM_nick')
end 


