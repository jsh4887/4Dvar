function [] = cov_B_matrix2(All_lat,All_lon,All_HGT,RXo,PGM_location,Blocation,Bmatfactor ,...
                              type_cov,All_HGT300,GetLatScale,GetLonScale,hmax)
% This definition of the Covariance matrix is based on the 
% paper by Bust et la 2004 
% or See Assimilation of Multiple Data types to a Regional Ionosphere Model with a 3D-Var algorithm (IDA4D)
% Chalachew et la., (2019) 
% we need to revise this covarience so that it depends on the Kp index 
% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04
    

disp('Computing the Covariance Matix ..........')
if strcmp(type_cov,'full')
    
        % calculate the total number of points in defined grid
        np = length(All_HGT)*length(All_lat)*length(All_lon);
        ras_ind = 1:np; % these will be the indicies of a rasta_scaned vector()
        epsilon = 2d-1; % this will be used in defining the cut off point, beyond which every thing is set to zero
                        % Zeros make the matrix sparse and hence easy to deal with in computation

       %  ~~~~~~~~~~~~ Horizontal and Vertical Correlations ~~~~~~~~~~~~~~~~~

        % height used in computing the horizontal correlation 

     

        % call this fxn to compute the horrizontal correlation 
       % [hcorrPtr]= ComputeHorizontalCorr3(PGM_location,All_lat,All_HGT300,All_lon
        [hcorrPtr]= ComputeHorizontalCorr3(PGM_location,All_lat,All_HGT300,All_lon,GetLatScale,GetLonScale);
        % call this fxn to compute the vertical correlation 
       % [vcorrPtr ]= ComputeVerticalCorr(All_lat,All_HGT,All_lon);
        [vcorrPtr ]= ComputeVerticalCorr(All_lat,All_HGT,All_lon,hmax);

        % reshape the input density into Vertical and Horrizontal shape
        % these will be used in computing the assumed Variance values 
        sigma_ptr = reshape(RXo(:),length(vcorrPtr),length(hcorrPtr));
        clear RXo
        pCorrVal= zeros(np, np);
        % ~~~~~~~~~~~~~~ Generate the Covariance matrix ~~~~~~~~~~~~~~ 
        k =1;
          for i = 1:length(hcorrPtr)
              for j = 1:length(vcorrPtr)
                 row =vcorrPtr(j,:)'*hcorrPtr(:,i)';
                 sigma = sigma_ptr(:,i)*sigma_ptr(j,:);
                 sigma = 0.4*sigma(:);
                 row = row(:); 
                 pCorrVal(k, ras_ind(row> epsilon)) =  sparse(row(ras_ind(row> epsilon)).*sigma(row> epsilon));
                 k =k+1;
              end
          end
          
            B = sparse(pCorrVal);
            cd(Blocation)
            save Bmatrix B 
            clear B


elseif strcmp(type_cov,'diagonal')

    B = spdiags((RXo.^2).*Bmatfactor ,0,length(RXo ),length(RXo));
    cd(Blocation)
    save Bmatrix B 
    clear B


end 
disp('Finished computing the Covariance Matix ..........')

