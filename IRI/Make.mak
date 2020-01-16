# this make file generates IRI excutable to be used in running the 4D_var code 
# By Nicholas Ssessanga 

# Define the compiler to use 
FC = gfortran 
F77 = gfortran
CC = gfortran

objects =   irisub.o irifun.o iritec.o iridreg.o igrf.o cira.o iriflip.o IRI2016_nick.o

all : $(objects)

output : $(objects) 
	$(f77) $(objects) -o iri2016xnick
	
IRI2016_nick.o : IRI2016_nick.f
	$(f77) -c IRI2016_nick.f
irisub.o : irisub.for
	$(f77) -c irisub.for
irifun.o : irifun.for
	$(f77) -c irifun.for
iritec.o : iritec.for
	$(f77) -c iritec.for
iridreg.o : iridreg.for
	$(f77) -c iridreg.for
igrf.o : igrf.for
	$(f77) -c igrf.for
cira.o : cira.for
	$(f77) -c cira.for
iriflip.o : iriflip.for
	$(f77) -c iriflip.for
clean: 
	