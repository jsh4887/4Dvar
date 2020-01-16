function [fnl_sln_file_name]  =  formulate_solution_name(Year,DOY,Time_of_start)     
% this Function  formulates part of the final solution file name
% using Year, DOY, Time 
% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04

        strhr = floor(Time_of_start);
        strminx = 60* (Time_of_start-strhr);
        strmin = floor(strminx); 
        strsec = 60* (strmin -strmin);
        if DOY < 10 

            DOY_str = ['00',num2str(DOY)];

        elseif   DOY >=10 && DOY <100

            DOY_str = ['0',num2str(DOY)];

        elseif DOY >= 100

             DOY_str = num2str(DOY);

        end 

    
        if strhr<10 
            strhrs = ['0' num2str(strhr)];  
        else 
              strhrs = num2str(strhr); 
        end 

        if strmin<10 
            strmins = ['0' num2str(strmin)];  
        else 
              strmins = num2str(strmin); 
        end 
        
       

        if strsec<10 
            strsecs = ['0' num2str(strsec)];  
        else 
             strsecs = num2str(strsec); 
        end 

   fnl_sln_file_name = [num2str(Year), '_',DOY_str,'_',strhrs,'h_',strmins,'min_',strsecs,'sec']; 