function [H,STEC,bError] = loadsavedHSTEC_temp(diffusiontimex,simulation_analysis,time_to_collect_rays,time_sample,locationx)
% use this function to load or save the data to temp folder 
% its quick to access this format than reading from observaion files 
% The files are saved in correspondence to the Time being processed
% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04
if simulation_analysis ==0
prex ='O'; 
elseif simulation_analysis == 1
prex ='S'; 
end 
time_samplex=num2str(time_sample);
time_samplex(time_samplex=='.')='_';

colect=num2str(time_to_collect_rays);
colect(colect=='.')='_';

savetime=roundn(diffusiontimex,-2);
tm = num2str(savetime);
tm(tm=='.')='_';

 cd (locationx)
 RE3 =  ['load HSTEC',prex,tm,'_',colect,'_',time_samplex,'.mat '];
 eval(RE3);
 
 
 RE1 = ['H = HSTEC',prex,tm,'_',colect,'_',time_samplex,'.H;'];

 eval(RE1);
 
 RE2 = ['STEC  = HSTEC',prex,tm,'_',colect,'_',time_samplex,'.STEC;'];

 eval(RE2);
 
  RE2 = ['bError  = HSTEC',prex,tm,'_',colect,'_',time_samplex,'.bError;'];

 eval(RE2);
 

 cd .. 
