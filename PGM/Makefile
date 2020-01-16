# this make file generates IRI excutable to be used in running the 4D_var code 
# By Nicholas Ssessanga 

# Define the compiler to use 
FC = gfortran 
f77 = gfortran
CC = gfortran

OPTIONS  = -O1 -fdollar-ok -ffree-line-length-none 
f77FLAGS = -fno-automatic $(OPTIONS)


objects =   Covert_Geog_2_GeoM_nick.o 

PGM_OBJS = 	cnvcoord.o sfc_convert_geo_coord.o rylm.o cgm_to_altitude.o altitude_to_cgm.o \
sfc_put_user_msg.o aacgm_init.o blkdat95.o

all : $(objects) $(PGM_OBJS) 

output :$(objects) $(PGM_OBJS) 
	$(f77) $(OPTIONS) $(objects) $(PGM_OBJS) -o PGMGeog_2_GeoM_nick

cnvcoord.o : cnvcoord.f
	$(f77) $(f77FLAGS) -c cnvcoord.f -o cnvcoord.o

sfc_convert_geo_coord.o : sfc_convert_geo_coord.f
	$(f77) $(f77FLAGS) -c sfc_convert_geo_coord.f -o sfc_convert_geo_coord.o

rylm.o : rylm.f
	$(f77) $(f77FLAGS) -c rylm.f -o rylm.o

cgm_to_altitude.o : cgm_to_altitude.f
	$(f77) $(f77FLAGS) -c cgm_to_altitude.f -o cgm_to_altitude.o

altitude_to_cgm.o : altitude_to_cgm.f
	$(f77) $(f77FLAGS) -c altitude_to_cgm.f -o altitude_to_cgm.o

sfc_put_user_msg.o : sfc_put_user_msg.f
	$(f77) $(f77FLAGS) -c sfc_put_user_msg.f -o sfc_put_user_msg.o

aacgm_init.o : aacgm_init.f
	$(f77) $(f77FLAGS) -c aacgm_init.f -o aacgm_init.o

blkdat95.o : blkdat95.f
	$(f77) $(f77FLAGS) -c blkdat95.f -o blkdat95.o

Covert_Geog_2_GeoM_nick.o : Covert_Geog_2_GeoM_nick.f90
	$(f77) $(f77FLAGS) -c Covert_Geog_2_GeoM_nick.f90

clean: 
	
