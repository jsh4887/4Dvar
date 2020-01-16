function [lambda_N,R] = Compute_lambda_Nx (H,STEC,Xn,bError,Time,increase_sd)
% Code computes lambda_N 
% used in computing lambda_k+1
% By Nicholas Ssessanga while at Chungnam National University
% 2018-july-02

% formulate the data coveraince matrix

[R]=R_covariance_Matrix2x(STEC,bError,Time,increase_sd);


% calculate lambda_N 

lambda_N = (H'*R^-1)*(STEC-H*Xn);
