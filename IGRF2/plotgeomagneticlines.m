
 figure
 latbegin = 30;
 latend = 40;
 latstep  = 1;
 longbegin = 130;
 longend = 131;
 longstep =1;
 hgtbegin =100;
 hgtend =10000;
[lat,lon, alt]= plotgeomagnick(latbegin,latend,latstep,longbegin,longend,longstep,hgtbegin,hgtend);
s = size(lat);
figure
for i = 1:s(2)
    z = lon(:,i);
    x = lat(:,i); 
    y = alt(:,i);
    if (x(1)~=0) && (z(1)==longbegin)
    plot(x,y,'.b')
    hold on 
    grid on 
    ylim([100 1000])
    end 
   
end 