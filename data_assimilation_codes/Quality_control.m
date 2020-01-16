function [Hq,STECq,bErrorq] = Quality_control (HK,Yk,XK,bError)

% does fucntion performs the simple buddy check quality control
% see discription in a paper by Ssessanga et la 2019 
% On imaging South African regional ionosphere using 4D-var technique
% As a data quality control process, we utilized a simple “buddy check” 
%procedure to remove outliers that may have resulted from inherent 
% measurement processes. That is to say, an observation or datum is 
% considered an outlier if the magnitude of the innovation, 
% |Hk*Xk-Yk|, exceeds a threshold C*(Sigmaok^2+Sigmafk^2 )^(1/2)
% where C is a pre-defined multiple (here set to 2), Sigmaok^2 andSigmafk^2
% are observation and model (in observation space) error variances, 
% respectively, at time t_k (see, e.g., Dee et al., 2011). 

% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04


spacefxn 
disp('Quality Control process intiated....')
disp(['Number of data before Quality Control....', num2str(length(Yk))])
 
C = 2.5;

% compute Hk*Xk
HkXk = HK*XK; 


% C*(Sigmaok^2+Sigmafk^2 )^(1/2)
Sigma_o_k = std (Yk);
sigma_f_k = std (HkXk);

% |Hk*Xk-Yk|
abs_DelatY = abs(HkXk -Yk); 

dist_btwn_sigmas = (Sigma_o_k^2 + sigma_f_k^2)^.5 ;
while 1


    threshold_value = dist_btwn_sigmas*C; 

    % get indicies to corresponding to outliers 
    outlier_ind = abs_DelatY(:) < threshold_value;

    Hq = HK(outlier_ind,:);STECq = Yk(outlier_ind,:) ;bErrorq = bError(outlier_ind,:);
    % calculate reduction in data size 
    Per_reduc = ((length(Yk)-length(STECq))/length(Yk))*100; 

    % if this reduction is greater than 50 percent increase C 
    % may be our background is so bad but the data are okay 
    % this could be possible especially at night or during the storm period. 
    % we assume that 50% of the the data all cant be bad
    if Per_reduc > 50 
        C = C + .2;
    else 
        disp(['Number of data after Quality Control....', num2str(length(STECq))])
        break 
    end 


end 
