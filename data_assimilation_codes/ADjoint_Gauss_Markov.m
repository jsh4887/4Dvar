function [ADjoint_idax] =ADjoint_Gauss_Markov(tau,deltaT, All_lat,All_lon,All_HGT)
% This is the ADjoint 
% or d[M(Xk)]   d[Xk+1]
%     ------- = -------
%      d[Xk]    d[Xk]
% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-02
lengthall= length(All_lat)*length(All_lon)*length(All_HGT);


Px = repmat(exp(-1*(deltaT/tau)), lengthall,1); 

ADjoint_idax = spdiags(Px ,0,lengthall,lengthall);




