data0 = load('fortx.24');
data300= load('fort.24'); 
geomlat0 = data0(:,3);
geomlon0 = data0(:,4);
geomlat300 = data300(:,3);
geomlon300 = data300(:,4);
geomlat1 = data300(:,1);
geomlon1 = data300(:,2);
plot(geomlon1,geomlat1,'*')
hold on 
plot(geomlon0,geomlat0,'o')
plot(geomlon300,geomlat300,'+')
grid on 
