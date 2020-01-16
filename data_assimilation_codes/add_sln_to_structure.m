function [Xanalysis]= add_sln_to_structure (Time,xcount,All_lat,All_lon,All_HGT,chivaluenew,Xo,RXb,Xanalysis)

if xcount ==1 
                                   
    Xanalysis.GridLat= All_lat; 
    Xanalysis.GridLon= All_lon;
    Xanalysis.GridHGT= All_HGT;

    Xanalysis(xcount).Xanal=reshape(Xo,length(All_HGT),length(All_lat),length(All_lon)) ; 
    Xanalysis(xcount).XIRI=reshape(RXb,length(All_HGT),length(All_lat),length(All_lon));
    Xanalysis(xcount).Time = Time;
    Xanalysis(xcount).Count =xcount;
    Xanalysis(xcount).Chisquared=chivaluenew; 
   
else
    Xanalysis(xcount).Xanal=reshape(Xo,length(All_HGT),length(All_lat),length(All_lon)) ; 
    Xanalysis(xcount).XIRI =reshape(RXb,length(All_HGT),length(All_lat),length(All_lon));
    Xanalysis(xcount).Time = Time;
    Xanalysis(xcount).Count=xcount;
    Xanalysis(xcount).Chisquared=chivaluenew; 
end 