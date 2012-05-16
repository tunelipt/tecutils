#include <math.h>


/**
   Code taken from http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
   Author:  W. Randolph Franklin 
 */
int pnpoly(int nvert, double *vertx, double *verty, double testx, double testy)
{
  int i, j, c = 0;
  for (i = 0, j = nvert-1; i < nvert; j = i++) {
    if ( ((verty[i]>testy) != (verty[j]>testy)) &&
	 (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
       c = !c;
  }
  return c;
}


int find_zone(double *xv, double *yv, int I, int J, double x, double y, int *i, int *j){

  double px[4];
  double py[4];
  int ii, jj;

  for (jj = 0; jj < (J-1); ++jj)
    for (ii = 0; ii < (I-1); ++ii)
      {
	px[0] = xv[ii+jj*J]; py[0] = yv[ii+jj*J];
	px[1] = xv[ii+1+jj*J]; py[1] = yv[ii+1+jj*J];
	px[2] = xv[ii+1+(jj+1)*J]; py[2] = yv[ii+1+(jj+1)*J];
	px[3] = xv[ii+(jj+1)*J]; py[3] = yv[ii+(jj+1)*J];

	if(pnpoly(4, px, py, x, y))
	  {
	    *i = ii;
	    *j = jj;
	    return 1;
	  }
      }
  return 0;
}


double polygon_area(int n, double *xv, double *yv)
{
  int i;
  double area = 0.0;
  
  for (i = 0; i < (n-1); ++i)
    area += xv[i]*yv[i+1] - xv[i+1]*yv[i];

  area += xv[n-1]*yv[0] - xv[0]*yv[n-1];

  return(0.5*area);
}


  


