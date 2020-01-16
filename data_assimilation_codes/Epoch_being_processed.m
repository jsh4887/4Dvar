function [Epch]= Epoch_being_processed (Year,DOY,Time)
% generate the string for the Epoch being processed 
% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04
Current_tm_strhr = floor(Time);
Current_tm_strmin = 60* (Time-Current_tm_strhr);

if Current_tm_strhr<10 
    Current_tm_strhrs = ['0' num2str(Current_tm_strhr)];  
else 
      Current_tm_strhrs = num2str(Current_tm_strhr); 
end 

if Current_tm_strmin <10 
    Current_tm_strmins = ['0' num2str(Current_tm_strmin)];  
else 
    Current_tm_strmins = num2str(Current_tm_strmin); 
end 

Epch = [num2str(Year), '_',num2str(DOY),'_',Current_tm_strhrs,'h',Current_tm_strmins,'min'];

disp (['Epoch being processed ', Epch])