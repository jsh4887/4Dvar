/*  $Revision: 1.1.1.1 $ */
/*  This routine provides the IDL interface to the
    magnetic coordinates routine, "cnv$coord"
    It is called from IDL using the call_external mechanism.
    The calling sequence is:
    ret_val = call_external(<libpgm.sl>,"cnv_coord_idl",
               in_pos, order, out_pos, mg_flag, err, model)

    where in_pos is the 3-element input position,
          order is a left-over from old software and is ignored
	  out_pos is the 3-element output position
	  mg_flag is 1 for geo -> mag  and 2 for mag->geo
	  err is the error code 
	  model is the date of the IGRF model (1990 or 1995)
*/
/*
$Log: cnvcoord_idl.c,v $
Revision 1.1.1.1  2007/02/02 05:01:46  gbust
Initial Revision

Revision 1.1.1.1  2006/03/17 16:48:55  rcalfas
First Import of IDA3Df90 SMP Log-Normal Codebase

Revision 1.1.1.1  2005/04/28 13:40:54  rcalfas
First Import of Shared Memory Processor version of IDA3D.

Revision 1.1  2005/03/31 20:16:41  rcalfas
Baseline PGM files.

Revision 1.1  2004/10/26 21:21:02  paulson
aacgm library

Revision 1.4  1998/02/27 17:22:10  baker
Changed the way initialization was handled so that the
default year is not a hardwired part of this code.
The default year should be set only by the main FORTRAN
routine.

Revision 1.3  1998/02/27 16:14:51  baker
Changed the initialization section to allow the use of
all the new epochs that have been added.

Revision 1.2  1996/03/20 22:54:14  baker
Changed the declaration of the arguments to use
the types "int16" and "int32" instead of short and long.
This is to avoid conflicts with machines that have
different word sizes.  This requires a new header
file, "SD_types.h" which provides the typedef
for int16 and int32.

 * Revision 1.1  1996/03/12  18:23:30  baker
 * Initial revision
 *
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "SD_types.h"

extern double sfc$$coeffs_com[2][3][3][121];

long cnvcoord_idl(argc,argv)
     int argc;
     void *argv[];
{
  static short myear = 0;
    
  int16 *mgflag;
  int16 *order;
  int16 *err;
  int16 *model;
  
  float *inpos;
  float *in_lat;
  float *in_long;
  float *height;
  float *outpos;
  float *out_lat;
  float *out_long;
  float *out_r;

  char filename[129],year_str[5];
  char ascii_flag = 1;
  
  if (argc < 5) 
  {
      fprintf(stderr,"cnvcoord_idl: invalid argument list. argc < 5\n");
      return -1;
  }
  
  inpos  = (float *) argv[0];
  order  = (int16 *) argv[1];
  outpos = (float *) argv[2];
  mgflag = (int16 *) argv[3];
  err    = (int16 *) argv[4];

  if (argc >= 6) model  = (short *) argv[5];
  else model = NULL;
  
  if (inpos == NULL || outpos == NULL || mgflag == NULL || err == NULL) 
  {
      fprintf(stderr,"cnvcoord_idl: invalid argument list from call_external\n");
      return -1;
  }
  
  in_lat  = inpos;
  in_long = (inpos+1);
  height  = (inpos+2);

  out_lat  = outpos;
  out_long = (outpos+1);
  out_r    = (outpos+2);

/* check the coordinate model year.  If no model is specified,
   the default is the most recent model.  If a model is
   specified and it is not the same as the previous model,
   then aacgm_init is called to load the coefficients of
   the desired model. */

  if ((model != NULL) && (*model != myear)) {
      myear = *model;
      sprintf(year_str, "%4.4d",myear);
      
      strcpy(filename, getenv("AACGM_DAT_PREFIX"));
      
      if (filename == NULL) {
	  fprintf(stderr,
	   "%%cnvcoord_idl: MISSING ENVIRONMENT VARIABLE: AACGM_DAT_PREFIX\n");
	  return -1;
	}

      strcat ( filename, year_str);
      strcat (filename, ".asc");

      if (filename == NULL) 
      {
	  fprintf(stderr,"%%cnvcoord_idl: NULL filename\n");
	  return -1;
      }
      fprintf(stderr,"filename = %s\n",filename);
      
      aacgm_init(filename,&ascii_flag);

  }
  
  cnv$coord(in_lat,in_long,height,order,out_lat,out_long,out_r,mgflag,err);
  if (*err != 0){
      return -1;
  }
  else return 0;
}
