function [modelDensityPtr] = set_min_density(modelDensityPtr) 
% In this function we set the minimum density 
% if any value is negative or below the minimum, 
% set the value to 1.0e6
% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04
dens_min = 0.01*modelDensityPtr;
den_min_indx = dens_min<1.0e6;
modelDensityPtr(den_min_indx) = 1.0e6;