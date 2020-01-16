clear all 
clc 
fclose all 
close all 
working_dir = '/Four_Dvar_Nick';
cd (working_dir)
Year = 2015; 
DOY = 075; 
% Height 
Gheight{1}=100:10:450;
Gheight{2}= 600:200:1800;

% Latitude 
latbegin =30;%-35; % Start of Latitude 
latend =40;%10;   % End Latitude 
latstep =5;    % Lat resolution  

% Longitude 
longbegin=120;%5;% Start of Longitude
longend= 130;%36; % End Longitude
longstep =5; % Long resolution  
Grdlatst =latbegin; 
Grdlatend =latend;
Grdlongst =longbegin;
Grdlongend = longend;

Glat{1}= latbegin:latstep:latend;
Glon{1}=longbegin:longstep:longend;
figure(1)
for Time = [0:1:23]
[~,RXo,All_lat,All_lon,All_HGT]=density (Glon,Glat,Gheight,Year,DOY,Time);

 RXb = RXo(:);
[JejuI,ICheI,OkinI,KokuI,WakkI,~,~,~,~,~]=get_iondata_korea(DOY,All_lat,All_lon,All_HGT,RXb,Time);

plot(Time,JejuI,'*')
hold on 
% delete('D:\Four_Dvar_Nick\IRI\fort.20')
% delete('D:\Four_Dvar_Nick\IRI\fort.15')
% delete('D:\Four_Dvar_Nick\IRI\fort.7')
pause(.3)
end 
 