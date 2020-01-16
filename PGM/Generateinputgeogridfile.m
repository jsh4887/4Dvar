
clc  
clear all 
fclose all 


% Latitude 
latbegin =10;     % Start of Latitude 
latend   =40;       % End Latitude 
latstep  =3;     % Lat resolution  

% Longitude 
longbegin=120;    % Start of Longitude
longend  =160;     % End Longitude
longstep =3;      % Long resolutio
Alti = 0; 
latin = latbegin:latstep :latend; 
lonin = longbegin:longstep:longend; 
[lonmat,latmat]= meshgrid(lonin,latin); 
fid = fopen ('/home/nick/Desktop/PGM/geo_lat_lon_vector.txt','w'); 
formatSpec1 = '%d %d\n';
fprintf(fid, formatSpec1, length(lonmat(:)), Alti)

formatSpec2 = '%d %d\n';
for j = 1:length(lonmat(:))
fprintf(fid, formatSpec2, latmat(j), lonmat(j))
end 
fclose(fid);

delete('/home/nick/Desktop/PGM/fort.24')
delete('/home/nick/Desktop/PGM/fort.26')

if ispc 
 y = dos ('XGeog_2_GeoM_nick.exe  &');
elseif isunix 
system('/home/nick/Desktop/PGM/XGeog_2_GeoM_nick')
end 

if exist('/home/nick/Desktop/PGM/fort.26','file')
fid1 = fopen('/home/nick/Desktop/PGM/fort.24');
out = textscan (fid1, '%f %f %f %f');
fclose all; 
geomLat = out{3};
geomLon = out{4};

end 


