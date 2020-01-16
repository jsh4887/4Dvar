function [Jeju,IChe,Okin,Koku,Wakk,Jeju_ion,IChe_ion,Okin_ion,Koku_ion,Wakk_ion]=get_iondata_korea(All_lat,All_lon,All_HGT,Xv,Year, DOY, Time_needed, Ionosonde_data_Loc )




        if ispc 
             slashx = '\';
        elseif isunix 
             slashx = '/';
        end
    
        if DOY < 10 

            DOY_str = ['00',num2str(DOY)];

        elseif   DOY >=10 && DOY <100

            DOY_str = ['0',num2str(DOY)];

        elseif DOY >= 100

             DOY_str = num2str(DOY);

        end 







elec = reshape(Xv,length(All_HGT),length(All_lat),length(All_lon));



%Beijing	BP440	China	40.30°N	116.20°E	30.52°N	172.17°W
%I-Cheon	IC437	S. Korea	37.14°N	127.54°E	27.80°N	161.86°W
%Jeju	JJ433	S. Korea	33.43°N	126.30°E	24.04°N	162.72°W
%Okinawa	OK426	Japan	26.33°N	127.80°E	17.06°N	160.88°W
%Kokubunji	TO536	Japan	35.70°N	139.50°E	27.17°N	150.89°W
%Wakkanai	WK546	Japan	45.40°N	141.70°E	36.99°N	150.17°W

% location of ionosonde whose data is needed
 latJeju_ion =33.43 ;lonJeju_ion =126.30;
 
 latIChe_ion  =37.14 ;lonIChe_ion =127.54;
 
 latOkin_ion= 26.33;lonOkin_ion =127.80;
 
 latWakk_ion =45.40;lonWakk_ion =141.70;
 
  latKoku_ion = 35.70;lonKoku_ion =139.50;

 [ ~, indexJeju_ionj ] = min(abs(All_lat(:) - latJeju_ion));
 [ ~, indexJeju_ioni ] =  min(abs(All_lon(:)-lonJeju_ion));
  [ ~, indexIChe_ionj ] = min(abs(All_lat(:) - latIChe_ion));
 [ ~, indexIChe_ioni ] =  min(abs(All_lon(:)-lonIChe_ion));
  [ ~, indexOkin_ionj ] = min(abs(All_lat(:) - latOkin_ion));
 [ ~, indexOkin_ioni ] =  min(abs(All_lon(:)-lonOkin_ion));
  [ ~, indexKoku_ionj ] = min(abs(All_lat(:) - latKoku_ion));
 [ ~, indexKoku_ioni ] =  min(abs(All_lon(:)-lonKoku_ion));
   [ ~, indexWakk_ionj ] = min(abs(All_lat(:) - latWakk_ion));
 [ ~, indexWakk_ioni ] =  min(abs(All_lon(:)-lonWakk_ion));

% Gindexs = abs(lat(:)-lat(indexG))<0.001 & abs(lon(:)-lon(indexG))<0.001;
% [ ~, indexH ] = min(((lat(:) - latH).^2 + (lon(:)-lonH).^2).^.5);
% Hindexs = abs(lat(:)-lat(indexH))<0.001 & abs(lon(:)-lon(indexH))<0.001;
% [ ~,indexM ] = min(((lat(:) - latM).^2 + (lon(:)-lonM).^2).^.5);
% Mindexs = abs(lat(:)-lat(indexM))<0.001 & abs(lon(:)-lon(indexM))<0.001;
%  [ ~, indexL ] = min(((lat(:) - latL).^2 + (lon(:)-lonL).^2).^.5);
%  Lindexs = abs(lat(:)-lat(indexL))<0.001 & abs(lon(:)-lon(indexL))<0.001;

% All_lon(indexGi)
% All_lat(indexGj)
denG = squeeze(elec(:, indexJeju_ionj ,indexJeju_ioni));
Jeju = max(denG);

denH = squeeze(elec(:, indexIChe_ionj ,indexIChe_ioni));
IChe = max(denH);

denM = squeeze(elec(:,indexOkin_ionj,indexOkin_ioni ));
Okin = max(denM);

denL = squeeze(elec(:, indexKoku_ionj ,indexKoku_ioni));
Koku = max(denL);


denL = squeeze(elec(:,indexWakk_ionj ,indexWakk_ioni));
Wakk = max(denL);


% Open the ionosonde file that contains the data 
 fid = fopen([Ionosonde_data_Loc,'\',num2str(Year),DOY_str,'_foF2_5station.txt']); 
 output = textscan(fid, '%f %f %f %f %f %f %f %f %f', 'headerLines',1);
 Hour = output{3};
 minx = output{4};
 Hourdec = Hour + minx/60; 
 foF2_Jeju = output{5};  
 foF2_ICheon= output{6};
 foF2_Okinawa= output{7};
 foF2_Kokubunji = output{8};
 foF2_Wakkanai = output{9};
 [~, indexmin]= min(abs(Hourdec-Time_needed));
 Jeju_ion = foF2_Jeju(indexmin);
 IChe_ion = foF2_ICheon(indexmin);
 Okin_ion = foF2_Okinawa(indexmin);
 Koku_ion =  foF2_Kokubunji(indexmin);
 Wakk_ion = foF2_Wakkanai(indexmin);

