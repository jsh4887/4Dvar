function [fdata_cmat,IRI_cmat,RMSE4Dvar,RMSEIRI] = RMSE_CORR_nmF2_4Dvar(fvrdatain,obsdatain,modeldatain)
nansind = isnan(obsdatain);
fvrdatain(nansind)=[];
modeldatain(nansind)=[];
obsdatain(nansind)=[];
fdvarmean = mean(fvrdatain);
obsmean=    mean(obsdatain); 
IRImean =  mean(modeldatain);
fdvarstd = std(fvrdatain);
obsstd=    std(obsdatain); 
IRIstd =   std(modeldatain);
fdvardata = (fvrdatain-fdvarmean)/fdvarstd;
obsdata = (obsdatain-obsmean)/obsstd;
IRIdata=(modeldatain-IRImean)/IRIstd ;
fdata_cmat = corr(fdvardata,obsdata);
IRI_cmat = corr(IRIdata,obsdata);
RMSE4Dvar = sqrt(sum((fvrdatain-obsdatain).^2)/length(obsdatain));
RMSEIRI = sqrt(sum((modeldatain-obsdatain).^2)/length(obsdatain));
    

    