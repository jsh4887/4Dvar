%% This is the main code to the 4D-var routine used in regional estimation
%  of the south African ionosphere.
%  Details of the derivations of the equations can be found in a
%  4D-var documantaion by Nicholas Ssessanga
%  Also read a paper "On imaging South African regional ionosphere using
%  4D-var technique"
% By Nicholas Ssessanga while at Chungnam National University
% 2019-july-04


%% before starting, clear and close all currenly open variables.
% This will free up memory

clear all ;
clc;
close all ;
fclose all;

%% Define the year and DOY being processed

Year = 2015;
DOYS = [074, 075, 076, 077];

% This is the time within a single day when assimilation should start
% and end
Time_of_start0 =0;      % Analysis Time 4Dvar should Start the processing
Time_of_end0 = 23.75;    % Analysis Time 4Dvar should End the processing

store_sln_every_T = 0;   % store a collection of final solutions every number of hours
% this helps to  free up the memory, ie we dont have to keep all the solutions in memory

%% ~~~~~~~ Define the folders needed during computation  ~~~~~~~ %%
% Define your current working directory where the code is located
working_dir = '/root/Desktop/4DVar/Seheon/Four_Dvar_rev_PGM';
% Lets change to this directory
cd (working_dir)

% Folder containing Location of the Data to be assimilated
Data_STEC_CMN_folder = '/data/Users/Seheon/Data_STEC_CMN_folder';

% Folder where the final solution should be saved
Final_solution_flder = '/root/Desktop/4DVar/Seheon/Four_Dvar_rev_PGM/Final_solution';


%                           TEMP FOLDER
% all data corresponding to the different time segments within a single assimilation
% window are processed and stored in this temp_folder, for quick access during the iteration
% process. This process highly reducess the computation time!
% Note: make sure the location of the temp_folder is on a drive with enough free space ~ 1 GB
Temp_fld_Loc = '/root/Desktop/4DVar/Seheon/Four_Dvar_rev_PGM';


%                       Location of Ionosonde data
% This is data is not used anywhere in the code, its
% only for comparison to check the perfomance after computation.

% Ionosonde_data_Loc = '/root/Desktop/4DVar/Seheon/Four_Dvar_rev_PGM/Data_STEC_CMN_folder';


%%  ~~~~~~~ Define parameters to used in computation   ~~~~~~~   %%
% Minimum elevation angle: all Reciever - Transimitter (GNSS) rays
% with eleven below this value are neglected. The ensures that multi-path effects are mitigated
Elevation=30;

% This is the maximum number of ground GNSS recievers to be considered
% in computation
numbstatn=200;

% minumber distance between previous and next station to be analysed
% currently set to 100 km ~ 1 deg
min_dist_stn = 1; % in degrees
% ~~~~~~~  covergence  parameters ~~~~~~~

% This is the momentum or Dump factor used in determing the step size
% which determines the covergence rate
B_dump_factor  = .05;% Dump factor on B
B_dump_min     =   1e-23;   % Min mum value that can be set

% type of coveriance for the Backgroung errors, either diagonal or
% full-> This means using correlation length
% 'diagonal' or 'full'
type_Bcov = 'full';


Maxiterations  = 250;        % maximum number of iteration that should be performed while
% sitting inside the inner loop

max_while_iter = 30;         % maximum number of repetions for a a single solutions if the chi-sqaured value is bad

% Either of   these two can be used as test for covergence
chitol= 0.05;             % chi-squared tolorance to be used in determining covergence
chivalue= 0.5;              % chi-squared  value  to be used in determining covergence



%  ~~~~~~~ Definition of assimilation window and correlation times (Tau) ~~~~~~~

Len_Assim_win =15;% 15; % assimilation window length in minutes
tauhours   = 1;         % correlation time in hours (Tau) exp(-deltaT/tau)

tauhoursBx =24;       % time to elapse before recomputing new B matrix from background(IRI):
% currently equal to tau but can be changed to
% another value as desired

tauhoursXb =24;        % time to elapse before recomputing new Xb from background(IRI):
% currently equal to tau but can be changed to
% another value as desired
compute_Xb_every_run = 0;

time_sample = 1.25/2;    % sample or length of segments in assimilation window
%(in minutes,  currently 2.5 min). We divide by two because
% the window is centred at the middle value (\..!../)

time_to_collect_rays = 1.25/2; % same as time_sample above but can be changed

increase_sdo = 0;

chivalue_rerun=0;    % This flag is used to test whether the final chi sqaure value was to high
% if thats the case, change the data variance by a fold of increased_sd
% and set the flag chivalue_rerun to 1.
% new_data_variance = old_data_variance*increased_sd


% ~~~~~~~  Define grid in Quest: ~~~~~~~

% Height
Gheight{1}=100:30:450;
Gheight{2}= 600:200:1400;

% Latitude
latbegin =20;     % Start of Latitude
latend   =50;       % End Latitude
latstep  =2.5;     % Lat resolution

% Longitude
longbegin=120;    % Start of Longitude
longend  =160;     % End Longitude
longstep =3;      % Long resolution


%% ~~~~~~~~~~~~~~~~~~~~~~  SHOULD NOT EDIT BELOW UNLESS IF YOU KNOW WHAT YOU ARE DOING ~~~~~~~~~~~~~~~~~~~~~~~~

% Find out whether the platform is pc or unix
if ispc
    slashx = '\';
elseif isunix
    slashx = '/';
end


% ~~~~~~~  Add these paths where the routines to be used in computation
%                        are located  ~~~~~~~

% add this path to the folder to your path
% this folder contains all the assimilation codes
addpath([working_dir,slashx,'data_assimilation_codes']);


% location of the geometry code
addpath([working_dir,slashx,'geometry_bath']);

% location of IGRF % goemagnetic code
% Note: this should be revised and replaced with the corrected version
addpath ([working_dir,slashx,'IGRF2'])



%  ~~~~~~~  set up the temp_folder  ~~~~~~~
disp('** 4D-var temp folder setup **')
locationx =[Temp_fld_Loc,slashx,'Fdvar_temp_folder'];


% Check and see if this folder does exist.
% if this is the first time running at this location create the temp folder

if exist (locationx, 'dir')
    disp('4D-var_temp_folder already existing in ....')
    disp(Temp_fld_Loc)
    disp ('Only a clean up will be performed!')
    
    % perform the clean up now (delete all files currently in this temp dir)
    tic
    disp ('cleaning up .......')
    filex = dir([locationx,slashx ,'*.mat']);
    for f=1:length(filex)
        delete([locationx,slashx ,filex(f).name])
    end
    disp ('finished cleaning up .......')
    toc
    
else % this folder does not exist, lets create a new one
    [s,mess,messid] = mkdir (Temp_fld_Loc,'Fdvar_temp_folder');
    
    if s~=1 % status for scussefully creating folder, 1 = okay otherwise throw an error
        disp('failed to create Fdvar_temp_folder!')
        error('check temp folder path ..... ')
    end
end


% ~~~~~~~  call this fxn to create some space  ~~~~~~~
spacefxn

%  ~~~~~~~  B matrix folder set up   ~~~~~~~
%  define where the B matrix  should be saved every time we generate a new one
disp('** B matrix temp folder setup **')
Blocation = [Temp_fld_Loc,slashx ,'Bmatrix_flder'];

if exist (Blocation, 'dir')
    disp('B matrix folder already existing in ....')
    disp(Temp_fld_Loc)
    disp ('Only a clean up will be performed!')
    % perform the clean up now of B matrix folder
    tic
    disp ('cleaning up B matrix folder.......')
    filex = dir([Blocation,slashx,'*.mat']);
    for f=1:length(filex)
        delete([Blocation,slashx,filex(f).name])
    end
    disp ('finished cleaning up  B matrix folder .......')
    toc
    
else % this folder does not exist, lets create a new one
    [s,mess,messid] = mkdir (Temp_fld_Loc,'Bmatrix_flder');
    if s == 1
        disp('succefully created B matrix folder....')
        
    else % status for scussefully creating folder, 1 = okay otherwise throw an error
        disp('failed to create B matrix folder!')
        error('check temp folder path ..... ')
    end
end

% ~~~~~~~  call this fxn to create some space  ~~~~~~~
spacefxn

% ~~~~~~~  Final solution folder set up   ~~~~~~~

disp('** Final solution folder set up **')
store_sln_every = Len_Assim_win/60;
if exist (Final_solution_flder, 'dir')
    
    disp('Final solution folder already existing in ....')
    disp(Final_solution_flder)
    
    disp ('cleaning up final folder.......')
    filex = dir([Final_solution_flder,slashx ,'*.mat']);
    for f=1:length(filex)
        delete([Final_solution_flder,slashx ,filex(f).name])
    end
    disp ('finished cleaning up final folder.......')
    
else % this folder does not exist, lets create a new one
    [s,mess,messid] = mkdir (Final_solution_flder);
    if s == 1
        disp('succefully created Final solution folder....')
        
    else % status for scussefully creating folder, 1 = okay otherwise throw an error
        disp('failed to create Final solution folder!')
        error('check Final solution folder path ..... ')
    end
end


%  ~~~~~~~       end of SET UP  ~~~~~~~  %



%%  ~~~~~~~ Computation begins below   ~~~~~~~  %%

for DOY =DOYS
    
    
    % time to start and end for the entire run within a single day
    Time_of_start = Time_of_start0;  % Analysis Time 4Dvar should Start the processing
    Time_of_end   = Time_of_end0;    % Analysis Time 4Dvar should End the processing
    
    
    start_assim_win = Time_of_start; % set the begining of the assimilation win to Time_of_start
    
    % fomulate part of the name to the final solution the Name of the final solution
    [fnl_sln_file_name1st]  =  formulate_solution_name(Year,DOY,Time_of_start);
    
    
    
    % ~~~~~~~  call this fxn to create some space  ~~~~~~~
    spacefxn
    
    % Define where the STEC data to be ingested is located
    % This data should be in .CMN format, processed using the Gopi program
    disp('** Data folder check **')
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
        disp(Data_location)
        error('data folder does not exist, recheck folder..... ')
    end
    
    
    
    
    % ~~~~~~~  call this fxn to create some space  ~~~~~~~
    spacefxn
    
    % Grid specifications
    disp ('** Grid specifications **')
    
    Grdlatst =latbegin;
    Grdlatend =latend;
    Grdlongst =longbegin;
    Grdlongend = longend;
    
    Glat{1}= latbegin:latstep:latend;
    Glon{1}=longbegin:longstep:longend;
    
    [All_lat,All_lon,All_HGT]=gridvectors(Glon,Glat,Gheight);
    
    
    
    disp(['Number of Latitudes: ... ',num2str(length(All_lat(:))) ])
    disp(['Number of Longitudes: ... ',num2str(length(All_lon(:))) ])
    disp(['Number of Heights: ... ', num2str(length(All_HGT(:)))])
    numgrid = length(All_lat(:))* length(All_lon(:))* length(All_HGT(:));
    disp(['Total Grid cells: ', num2str(numgrid)])
    
    % assume satellites are located at this altitude
    satheight = All_HGT(end)*1E3+50E3;
    
    
    % ** Covert the CMN data into binary format for faster reading of the files data files  **
    %               This proceedure highly speeds up the computation
    covert_cmn_data2binaryx ( Data_location,Year,DOY,All_lat,All_lon,numbstatn,min_dist_stn)
    
    
    
    % create the assimilation window container
    % Last time sample in the assimilation window will be start point + length of window
    % remember this time is in hours so covert every thing to hours
    
    End_of_Assim_win =  start_assim_win + Len_Assim_win/60; %hours
    
    %  Vxi vessel contains all  the time segments within the assimilation
    %  window moving backwards in time. This will be the first assimilation
    %  window to be processed. The next window will be updated at the end of
    %  the first while loop.
    Vxi = End_of_Assim_win :-2*(time_sample/60):(start_assim_win);
    
    
    
    
    % Covert time_sample and tau to common units of seconds
    deltaT = 2*time_sample*60;   % minutes to seconds
    tau = tauhours*60*60; % hours to seconds
    
    
    % Intialize flags to be used in computation
    LastcounterXs=1;
    % check whether to compute new Background
    corrletionT = 0;
    corrletionTx =0;
    
    % check whether to compute new B matrix
    corrBcompuT  = 0;
    corrBcomputx = 0;
    
    % chi break flag set (1) to indicate covergence is reached
    chibreakicrease=0;
    % This flag is checked to see whether we are starting from a cold run
    % intially (1)
    cold_run =1;
    
    % flag to check whether we are using data from a file(observation
    % data)
    %
    usingfiledata=1;
    simulation_analysis = 0;
    
    % Set this value( B_dump_factoro) which will help to reset the dump_factor
    % if its modified within the code. For every new assimilation window,
    % this factor should be restored to the original value.
    
    B_dump_factoro = B_dump_factor;
    
    increase_sd =increase_sdo;
    
    % counter used in storing results
    xcount=1;
    
    % intialize structure to store data
    Xanalysis.start = 0;
    X.start = 0;
    
    %          ~~~~~~~     go through each  Assimilation segement ~~~~~~~
    
    
    
    while 1    % this while loop goes through all the Time samples to the End
        % this is the bigger Loop start time ----> End_time
        
        
        % number of iterations in a single while loop
        iterations = 1;
        
        % intialize counter to count number of while repeats
        % please do not confuse this with number of iterations
        countwhile =1;
        
        % This flag is checked so that all data is stored in the first
        % iteration
        gettingData = 1;
        % this flag helps in determining the choice of the Dumping_ factor
        % when approaching the solution, the dumping factor is continously
        % divided by 2 for each 10 iteration in order to approach the solution
        % while taking small steps
        
        count_B_dump_flag = 1;
        
        % intilialize the counting to determine whether 10 iterations are
        % done, so  that we can reduce the dumpfactor
        count_B_dump=1;
        
        
        
        % This is the optmisation loop for each Assimilation Window
        while 1
            
            % we must work moving backwards in time because of the adjoint operation
            
            intial = 1;
            timecounterXs = LastcounterXs;
            timecounter = 1;
            chi_better_b4_iter = 0;
            
            % get the number of samples in the assimilation window
            number_of_time_samples = length(Vxi);
            
            
            
            if iterations == 1
                
                % ~~~~~~~  call this fxn to create some space  ~~~~~~~
                spacefxn
                %    display the following  if this the first iteration
                disp ('** Time assimilation details **')
                disp (['Length of assimilation Window,...', num2str(Len_Assim_win), ' min'])
                disp (['Number of samples in assimilation window,...', num2str(number_of_time_samples)])
                disp (['Length of each sample,...', num2str(2*time_sample), ' min'])
                
            end
            
            
            
            % Lets go through the time samples while moving back in time
            for simulationtime =Vxi
                
                
                % this is the current time to analyse along the time trajectory
                Time =simulationtime;
                
                
                % this is dT in the paper  used in  exp(-dT/tau), units here are minutes
                Total_process_time  =  (Time - start_assim_win)*60;
                
                
                
                
                
                %********************************* Xb and B_matrix *****************%
                % Here we compute the background and its corresponding Covariance
                % matrix
                % only generate these quantities if its the intial iteration within
                % the  assimilation window. other wise this value is the same
                if intial && iterations == 1
                    
                    
                    
                    % compute the Xb densities using the IRI 2016 module
                    % and store the results in RXo;
                    if corrletionT ~= 1 || compute_Xb_every_run == 1
                        
                        if corrletionT ~= 1
                            [~,RXb,All_lat,All_lon,All_HGT]=density (Glon,Glat,Gheight,Year,DOY,Time);
                            RXb = RXb(:);
                            RXo=RXb(:);
                            Xo = RXo;
                            prev_Xo = Xo;
                            
                        elseif compute_Xb_every_run == 1
                            [~,RXb,~,~,~]=density (Glon,Glat,Gheight,Year,DOY,Time);
                            RXb = RXb(:);
                        elseif (compute_Xb_every_run == 0)&&(cold_run ==1) ||  (compute_Xb_every_run == 0)&& (chivalue_rerun==1)
                            [~,RXb,All_lat,All_lon,All_HGT]=density (Glon,Glat,Gheight,Year,DOY,Time);
                            RXb = RXb(:);
                            RXo=RXb(:);
                            Xo = RXo;
                            prev_Xo = Xo;
                            cold_run =0;
                            chivalue_rerun=0;
                        elseif (compute_Xb_every_run ==1)&&(cold_run ==1)||(compute_Xb_every_run == 1)&& (chivalue_rerun==1)
                            
                            RXo=RXb(:);
                            Xo = RXo;
                            prev_Xo = Xo;
                            cold_run =0;
                            chivalue_rerun=0;
                            
                        elseif (compute_Xb_every_run ==1)
                            [~,RXb,All_lat,All_lon,All_HGT]=density (Glon,Glat,Gheight,Year,DOY,Time);
                            RXb = RXb(:);
                            
                        end
                        
                        % if this was cold run, set everything to IRI and
                        % set the cold run flag to 0
                        
                        %                               if cold_run ==1
                        %                                      RXo=RXb(:);
                        %                                      Xo = RXo;
                        %                                      cold_run =0;
                        %                               end
                        
                        
                        % set the flag to compute Background to 1, to indicate that
                        % Xb is already generated
                        corrletionT =1;
                    end
                    
                    
                    % compute the B using the IRI 2016 module
                    % and store file in Bmatrix folder
                    if corrBcompuT ~=1
                        % set the flag to compute Background coveriance (B)to 1, to indicate that its
                        % already generated
                        Bmatfactor=1;
                        
                        PGM_location = [Temp_fld_Loc,slashx ,'PGM'];
                        All_HGT300 = 300;
                        GetLatScale = 5;
                        GetLonScale = 8;
                        hmax = 350;
                        
                        cov_B_matrix2(All_lat,All_lon,All_HGT,RXo,PGM_location,Blocation,Bmatfactor,type_Bcov, All_HGT300,GetLatScale,GetLonScale,hmax)
                        
                        corrBcompuT=1;
                        
                    end
                    
                    fclose all;
                    
                end
                %   ********** End of computing  Xb and B_matrix **************%
                
                
                
                
                %******************** Xbi ****************************************%
                % Set intial for all time samples as the background
                % Here we are assuming that within 15 minutes the background will not
                % change that much. so lets set intial Xn to Background.
                
                if iterations == 1 && intial
                    
                    [~,~,Xo,X] = set_all_background(X,Vxi,RXo,timecounterXs) ;
                    
                end
                %******************** Xb ****************************************%
                
                
                
                
                %******************** Forecast ****************************************%
                % we Forecast Using the Gauss Markov filter
                % See equation in paper by Nicholas Sseaanga
                % Since the Assimilation window is small, we have
                % assumed that all time samples have the same
                % background
                propagation_time = Total_process_time*60;% time to propagate the density in seconds
                
                [XK]=forecast(Xo,RXo,tau,deltaT,propagation_time);
                
                
                %******************** end forecast ****************************************%
                
                
                
                
                
                
                
                
                %******************** data ****************************************%
                % This only to check if we are using data from file(obervation ) or simulation
                % but currently this is set to using observed data
                
                if usingfiledata==1
                    
                    
                    % if this is the first iteration collect all data and store it
                    % Here we store the STEC, H, Rx and Tx
                    % We will not have to look for this data again once collected.
                    
                    if iterations ==1 && (gettingData == 1)&&(countwhile==1)&&(chivalue_rerun==0)
                        
                        
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
                        
                        
                        disp (['Epoch being processed ', num2str(Year), '_',num2str(DOY),'_',Current_tm_strhrs,'h',Current_tm_strmins,'min'])
                        
                        % ~~~~~~~  call this fxn to create some space  ~~~~~~~
                        spacefxn
                        
                        disp('Getting data in 1st iteration ...... ')
                        
                        
                        % This function gets the H, b = STEC, and corresponding b Error.
                        [H,STEC,bError,Rxinfo]= creat_H_b_bError(Year,DOY,Time,time_to_collect_rays,Elevation,numbstatn,satheight,...
                            All_HGT,All_lat,All_lon,min_dist_stn,Data_location);
                        
                        fclose all;
                        cd (working_dir )
                        
                    else
                        % if all the data with in the assimilation is stored, in
                        % the next assimilation we will not have to search for the data
                        % and create H. we just have to use the data in the temp folder
                        
                        [H,STEC,bError] = loadsavedHSTEC_temp(Total_process_time,simulation_analysis,time_to_collect_rays,time_sample,locationx);
                        
                        %disp('Fast computing iterations now ...... ')
                        
                    end
                    
                end
                
                %****************************** data ******************************%
                
                
                
                
                % ***************** Quality control *******************************%
                if iterations ==1 && (gettingData == 1)&&(countwhile==1)&& (chivalue_rerun == 0);
                    % only perform quality control if the data are enough for
                    % for statics (here greater or equal 100)
                    
                    if size(STEC,1) >= 100
                        [H,STEC,bError1] = Quality_control (H,STEC,XK,bError);
                        
                    else
                        disp('Not enough data for Quality control statistics')
                    end
                    
                    
                    % save the data after Quality control
                    % Save the corresponding data to data (H, STEC and bError) temp folder in accordance to time
                    saveHSTEC_temp(Total_process_time,H,STEC,bError,simulation_analysis,time_to_collect_rays,time_sample,locationx)
                    
                    % Save the corresponding PRN and Rx info to data (Rx and Tx ) in temp folder in accordance to time
                    saveRxLat_RxLon_PRN_temp(Total_process_time,Rxinfo.RxLat,Rxinfo.RxLon,Rxinfo.prn,Rxinfo.time,...
                        simulation_analysis,time_to_collect_rays,time_sample,locationx)
                end
                
                % ***************** End of Quality control *********************************************%
                
                
                
                
                %************************ Compute lambda_N *************************************%
                
                if intial
                    
                    [lambda_k_1,~] = Compute_lambda_Nx(H,STEC,XK,bError,Time,increase_sd );
                    
                    intial = 0;
                end
                
                %************************end of Compute lambda_N *************************************%
                
                
                
                
                %************************************************************************
                %                         lambda_k
                %use lambda_N to compute the next lambda_k as you move back in time
                %************************************************************************
                
                if timecounter > 1 && timecounter < length(Vxi)
                    
                    [lambda_k,gradient_langrange,~] = Compute_lambda_k(H,STEC,XK,All_lat,All_lon,All_HGT,lambda_k_1,tau,deltaT,bError,Time,increase_sd);
                    
                    
                    lambda_k_1= lambda_k;
                    
                    
                end
                
                
                if simulationtime == Vxi(end) && iterations ==1
                    % we have reached the last time smaple in the assimilation window
                    % remember we were moving backwards in time
                    % this is the time sample corresponding to Xo
                    % Now test and see whether the original Xo
                    % fits the observed data.
                    % if the chi-sqaured values is less than the set
                    % optimal value, no Need to Optimise this Xo
                    % just exit and move on the to next assimilation
                    % window. Other wise continue and perform
                    % optimization
                    % To break out of optimization set
                    % chi_better_b4_iter = 1
                    [sd] = R_covariance_Matrix2x(STEC,bError,Time,increase_sd);
                    
                    chivaluenew = sum((((H*Xo-STEC)).^2)./(diag(sd)))/length(STEC);
                    
                    
                    if chivaluenew <=chivalue
                        
                        chibreakicrease=1;
                        chi_better_b4_iter = 1;
                        break
                    end
                end
                
                
                
                
                
                timecounter = timecounter +1;
                timecounterXs = timecounterXs+1;
                
            end % Assimilation window end
            
            
            
            
            
            
            %  Lets Compute the Xo using Xo = Xb + B*?o; Xb here is previous Xo
            if chi_better_b4_iter ~= 1;
                
                % call this fxn to create some space
                spacefxn
                disp ('** computing New solution **')
                disp (['For epoch: ', num2str(Year), '_',num2str(DOY),'_',Current_tm_strhrs,'h',Current_tm_strmins,'min'])
                
                prev_Xo = Xo; % store previous Xo before any new adjustments
                % if anything went wrong in the computation, then we will set
                % the densities to back to the prevous value
                
                
                
                %  ~~~~~  Load Bmatrix stored in Blocation ~~~~~
                B = load([Blocation,slashx,'Bmatrix.mat']);
                B = (B.B);
                
                % ~~~~~~~~~~  New solution ~~~~~~~~~~
                %  check and see if dump factor is less than the
                %  minimum set value if so limit it
                if    B_dump_factor < B_dump_min
                    B_dump_factor = B_dump_min;
                end
                
                Xo = Xo + B_dump_factor *(B*lambda_k(1:length(B(1,:))));
                
                
                % Set a condition to limit any densities from
                % going below the minimum value here this value is set as 1.0e6
                % Nontheless, analysis densities never go below this value from my exprience
                [Xo] = set_min_density(Xo);
                clear B
                %~~~~~~~~~~ Done with New solution ~~~~~~~~~~
                
                
                
                % ~~~~~~~~~~  Chi squared value  test (covergence) ~~~~~~~~~~
                spacefxn
                disp ('** Determing goodness of fit using Chi test **')
                % This will help us determine how good the new solution is.
                % or The goodness of our model fit and choice of parameters
                % See underst_chi.pdf in document folder
                % here we take mu = mean as the measured STEC.
                
                % so lets get the variance (var)
                [sd]=R_covariance_Matrix2x(STEC,bError,Time,increase_sd);
                
                % now compute the chi squared value using the previous estimate and the
                % current estimate
                % A lower chi sqaured value than the previous indicates a better solution
                chivaluenew = sum((((H*Xo-STEC)).^2)./(diag(sd)))/length(STEC);
                chivalueold = sum((((H*prev_Xo-STEC)).^2)./(diag(sd)))/length(STEC);
                disp(['Old chi value...', num2str(chivalueold)])
                disp(['New chi value...', num2str(chivaluenew)])
                
                % Check and see where the solution we are approaching optimal,
                % if so, reduce the dump factor
                diffChi = (chivalueold-chivaluenew);
                if   ( chivaluenew <chivalue+ 2*chivalue && chivaluenew > 1.5*chivalue) ||(abs(diffChi)> 1.5 *chitol && abs(diffChi)) < 2*chitol
                    
                    if count_B_dump==10
                        
                        B_dump_factor = B_dump_factor/10;
                        count_B_dump=1;
                        
                    else
                        count_B_dump = count_B_dump + 1;
                        
                        if  count_B_dump_flag == 1
                            B_dump_factor = B_dump_factor/10;
                            count_B_dump_flag = 0;
                            
                        end
                        
                    end
                end
                
                
                % if the change in chi squared is smaller than the
                % the tolorance  then just break  and move on to the next assimilation
                % window. Thats the best solution
                
                if  diffChi < chitol
                    disp (['less chitol ', 'B_dump_factor: ', num2str(B_dump_factor)])
                end
                
                % if chi is increasing, that means the current solution is worse than the
                % previous one
                if  chivaluenew > chivalueold
                    disp('chi value increasing...')
                    
                    % set the the current solution back to the previous
                    Xo = prev_Xo;
                    % decrease the dump facor by 10
                    B_dump_factor  = B_dump_factor/10;
                    % and start counting the number of whileloop repeats for this solution
                    
                    countwhile = countwhile +1;
                    
                    spacefxn
                    disp([ num2str(countwhile), ':Repeat solution with new factors...'])
                    disp (['New B dump factor:... ', num2str(B_dump_factor)])
                    
                    % if the maximum number of iterations is reached just stop every thing
                    % and move on to the next assimilation window
                    if countwhile >=  max_while_iter
                        chibreakicrease=1;
                        countwhile=1 ;
                        
                        disp ('Maxmum number of iterations reached for this solution!')
                    end
                    
                else
                    % Otherwise every thing is going okay!
                    % set the previous density equal to new density
                    % and also reset the while loop repeat counter
                    prev_Xo =Xo;
                    countwhile=1;
                    disp('chi value decreasing in correct way....')
                    disp(['sd muptiple factor: ', num2str(increase_sd)])
                end
                chi_better_b4_iter=0;
                
            else
                
                % call this fxn to create some space
                spacefxn
                disp('chi better before iterations....')
                disp(['Chi value...', num2str(chivaluenew)])
                chivalueold=chivaluenew;
                chi_better_b4_iter=0;
                
                
                
            end
            
            % also check the maximum number of iterations
            if iterations >= Maxiterations
                chibreakicrease=1;
                
            end
            
            
            
            
            
            % lets reverse some flags if we are done with iteration
            gettingData=0;
            intial = 1;
            
            
            
            
            
            %********************* End of covergence test *****************************%
            if iterations > 1 || chibreakicrease==1
                
                
                if  chivaluenew <=chivalue ||chibreakicrease==1 ||abs(chivalueold-chivaluenew) <= chitol
                    
                    
                    %                            if abs(chivaluenew) >= 10 || isnan(chivaluenew)
                    %
                    %                                   % the previous results is not trusted.
                    %                                   % lets repeat every thing while reducing
                    %                                   % increasing the variance
                    % %                                   corrletionT = 0;
                    % %                                   corrletionTx =0;
                    % % %                                   corrBcomputx  = 0;
                    % % %                                   corrBcompuT  = 0;
                    % %                                   cold_run =1;
                    %                                    Xo=prev_Xo;
                    %                                   increase_sd =increase_sd + .05;
                    %                                   countwhile = countwhile +1;
                    %                                   chibreakicrease =1;
                    %                                   chivalue_rerun = 1;
                    %                                   count_B_dump_flag = 1;
                    %                                   count_B_dump=1;
                    %
                    %                                   B_dump_factor = B_dump_factoro;
                    %
                    %                            else
                    
                    
                    % store the results if every thing was okay
                    [Xanalysis]= add_sln_to_structure (Time,xcount,All_lat,All_lon,All_HGT,chivaluenew,Xo,RXb,Xanalysis);
                    
                    xcount = xcount+1;
                    chibreakicrease=0;
                    countwhile=1;
                    chivalue_rerun=0;
                    count_B_dump_flag = 1;
                    count_B_dump=1;
                    increase_sd=0;
                    
                    %                          end
                    
                    
                    
                    
                    
                    break % Get out of the assimilation window optimization window
                end
                
                
            end
            %*********************************end covergence*****************************%
            
            iterations = iterations+1;
            
            
            
        end
        
        fclose all;
        
        
        if chibreakicrease ==0
            
            % previous result as the new background
            RXo=Xo;
            
            % reset the parameters back to default
            increase_sd =increase_sdo;
            countwhile=1;
            B_dump_factor  = B_dump_factoro;
            gettingData =1;
            
            
            LastcounterXs = timecounterXs-1;
            
            % Increament the Correlation time counter
            corrletionTx = corrletionTx + Len_Assim_win/60;
            
            % if above the correlation time is higher than the set
            % correlation time,
            % set these flags so that we compute every thing back from
            % IRI model
            if corrletionTx >=tauhoursXb
                corrletionT = 0;
                corrletionTx =0;
                
            end
            
            
            % Increament the B Correlation time counter
            corrBcomputx  = corrBcomputx + Len_Assim_win/60;
            % if above the correlation time is higher than the set
            % correlation time,
            % set these flags so that we compute B from
            % from the Backround
            if corrBcomputx >=tauhoursBx %tauhoursx%60/60
                corrBcomputx  = 0;
                corrBcompuT  = 0;
                cold_run =1;
            end
            
            
            
            %           SAVE THE FINAL SOLUTION         %
            % ~~~ Dump the solution to final folder every after store_sln_every_T period (in hrs)~~~%
            
            
            
            if  End_of_Assim_win >= Time_of_end || (store_sln_every) >= store_sln_every_T
                
                
                % save the final solution
                [fnl_sln_file_name1st,store_sln_every] = save_final_solution(Year,DOY,End_of_Assim_win,Len_Assim_win,fnl_sln_file_name1st,...
                    Final_solution_flder,slashx,Xanalysis);
                
                
                
                % remove field from memory
                Xanalysis= rmfield(Xanalysis, 'Xanal');
                Xanalysis= rmfield(Xanalysis, 'XIRI');
                Xanalysis= rmfield(Xanalysis, 'Time');
                Xanalysis= rmfield(Xanalysis, 'Count');
                Xanalysis= rmfield(Xanalysis, 'Chisquared');
                clear Xanalysis
                Xanalysis.start = 0;
                xcount =1;
                
                % if we have reached the final in that day
                % break  start a new day analysis
                if  End_of_Assim_win > Time_of_end
                    
                    
                    
                    
                    start_assim_win = Time_of_start;
                    End_of_Assim_win =  start_assim_win + Len_Assim_win/60; %hours
                    
                    break
                end
                
            end
            
            % Form the new basket of assimilation window
            start_assim_win = Vxi(1);% remember time is backwards.
            End_of_Assim_win =  start_assim_win + Len_Assim_win/60; %hours
            
            Vxi = End_of_Assim_win :-2*(time_sample/60):(start_assim_win);
            
            store_sln_every =  store_sln_every +  Len_Assim_win/60;
            
            
            if  start_assim_win > Time_of_end
                
                
                break
            end
            
        end
        
        
    end
    cold_run =1;
end
