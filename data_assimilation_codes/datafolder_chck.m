function [Data_location] = datafolder_chck(Year,DOY,Data_STEC_CMN_folder,slashx)
    % This function creates the path to the Data folder 
    % and also check whether the folder contains any data (.cmn files) for processing 
    % By Nicholas Ssessanga while at Chungnam National University
    % 2019-july-04

        Year_fld = num2str(Year); 

        if DOY < 10 

            DOY_folder = ['00',num2str(DOY)];

        elseif   DOY >=10 && DOY <100

            DOY_folder = ['0',num2str(DOY)];

        elseif DOY >= 100

             DOY_folder = num2str(DOY);

        end 

        % formulate the Data location path 
        Data_location = [Data_STEC_CMN_folder,slashx,Year_fld,slashx, DOY_folder];

        if exist (Data_location, 'dir')
            numb_files = length(dir ([Data_location,slashx,'*.cmn']));
            if numb_files ==0
            numb_files = length(dir ([Data_location,slashx,'*.Cmn']));
            end 


            if  numb_files > 1
               disp ( [num2str(numb_files),  ' CMN data files available for processing'])
            else 
               error('No cmn files available in folder, recheck folder..... ')   
            end 

        else
             error('data folder does not exist, recheck folder..... ')  
        end