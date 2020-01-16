
function [sat]= calcsatposnk (Elmat, Azmat, stationLong,stationLat,  satalitude)
El = Elmat;
Az = Azmat;
hipp = satalitude; % 20000*1000;
Re = 64E5;
ru = stationLat;
rln = stationLong; 


Elev = El*pi/180;
Azim = Az*pi/180;
rcu = ru*pi/180;
rlon =rln*pi/180;

phipp =((pi/2)-Elev- asin((Re/(Re+hipp)).*cos(Elev)));

ipplat = asin(sin(rcu).*cos(phipp) + cos(rcu).*sin(phipp).*cos(Azim));

ipplong = rlon + asin((sin(phipp).*sin(Azim))./cos(ipplat)); 

% Latsat = ipplat*180/pi;
% longsat = ipplong*180/pi;
sat.lat = ipplat*180/pi;
sat.long= ipplong*180/pi;

% tic
% for i= 1:length(Elmat)
% phipp =((pi/2)-Elev(i)- asin((Re/(Re+hipp))*cos(Elev(i))));
% 
% ipplat = asin(sin(rcu)*cos(phipp) + cos(rcu)*sin(phipp)*cos(Azim(i)));
% 
% ipplong = rlon + asin((sin(phipp)*sin(Azim(i)))/cos(ipplat)); 
% 
% Latsat = ipplat*180/pi;
% longsat = ipplong*180/pi;
% sat.lat1(i) =Latsat;
% sat.long1(i) =longsat;
% 
% end 
% toc





