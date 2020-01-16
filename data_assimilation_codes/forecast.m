function [Xk]=forecast(Xo,RXo,tau,deltaT,propagation_time)

% we Forecast Using the Gauss Markov filter 
% See equation in paper by Nicholas Sseaanga 
% Since the Assimilation window is small, we have
% assumed that all time samples have the same
% background

% In the future, for longer assimilations windows we will have to compute this Xb_k_1 from
% model

% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04
 
%number_of_interaions = propagation_time/deltaT; 
Xk = Xo;
Xb_k_1 = RXo; 
Xb_k = RXo;%Xb_k_1;
dT = 0;
while 1

if dT > propagation_time
    break
end 
Xk_1 = Xb_k_1 + (Xk-Xb_k).*exp(-1*(dT/tau));
Xk =  Xk_1;
dT = dT + deltaT;
end 

