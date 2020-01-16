% PLOTBEARTH Plot the Earth's magnetic field lines using the IGRF.
% 
% Plots a globe and a number of magnetic field lines starting at each point
% specified by the vectors in lat_start and lon_start. Both distance and
% nsteps should be the same length as lat_start. The plot will spin if spin
% is true and will continue to spin until the user hits CTRL+C.

time = datenum([2012 7 17 6 30 0]);
lat_start = 30:1:50; % Geodetic latitudes in degrees.
lon_start = 140:1:141; % Geodetic longitudes in degrees.
alt_start = 100; % Altitude in km.
A = linspace(100,10000, length(lat_start));
distance = -sign(lat_start).*A;%[30e3 70e3 150e3]; % km.
nsteps = abs(distance)/10;
spin = false;

% Get the magnetic field line points.
lat = zeros(max(nsteps(:))+1, numel(lat_start)*numel(lon_start));
lon = zeros(max(nsteps(:))+1, numel(lat_start)*numel(lon_start));
alt = zeros(max(nsteps(:))+1, numel(lat_start)*numel(lon_start));
for index1 = 1:numel(lat_start)
    for index2 = 1:numel(lon_start)
        [lat(1:nsteps(index1)+1, ...
            index1*(numel(lon_start)-1)+index2) lon(1:nsteps(index1)+1, ...
            index1*(numel(lon_start)-1)+index2) alt(1:nsteps(index1)+1, ...
            index1*(numel(lon_start)-1)+index2)] = ...
            igrfline(time, lat_start(index1), lon_start(index2), ...
            alt_start, 'geod', distance(index1), nsteps(index1));
    end
end
plot(lat,alt, '.r'); % geod coord


