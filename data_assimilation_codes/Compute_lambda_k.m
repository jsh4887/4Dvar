function [lambda_k_1,gradient_langrange,R] = Compute_lambda_k(H,STEC,XK,All_lat,All_lon,All_HGT,lambda_k,tau,deltaT, bError,time,increase_sd)

% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-02

% Evaluate the adjoint
[ADjoint_Gauss] =ADjoint_Gauss_Markov(tau,deltaT,All_lat,All_lon,All_HGT);                                                                       
                                                             
% get the R 
[R]= R_covariance_Matrix2x(STEC,bError,time,increase_sd);

% evaluate  = 
%             d[Xk+1]^T
% lamda_k =   -------   lambda_k+1 + Hk^T * Rk^-1 (Hk*Xk-Yk ),k=N-1,N-2,?0      
%             d[Xk]


gradient_langrange = ADjoint_Gauss'*lambda_k;

lambda_k_1 = gradient_langrange - (H'*R^-1)*(H*XK-STEC);

                 