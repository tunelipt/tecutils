#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

#include "polygon.h"

static SEXP
Rpoly_pnpoly(SEXP xv, SEXP yv, SEXP x, SEXP y)
{
  PROTECT(xv = AS_NUMERIC(xv));
  PROTECT(yv = AS_NUMERIC(yv));
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  
  
  int n = LENGTH(xv);
  double *xvert = NUMERIC_POINTER(xv);
  double *yvert = NUMERIC_POINTER(yv);
  double xtest = REAL(x)[0];
  double ytest = REAL(y)[0];


  int c = pnpoly(n, xvert, yvert, xtest, ytest);

  SEXP ret;
  PROTECT(ret = NEW_LOGICAL(1));
  LOGICAL(ret)[0] = c;
  
  UNPROTECT(5);
  return ret;
}

static SEXP
Rpoly_find_zone(SEXP xv, SEXP yv, SEXP II, SEXP JJ, SEXP x, SEXP y)
{

  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(II = AS_INTEGER(II));
  PROTECT(JJ = AS_INTEGER(JJ));
  
  double xtest = REAL(x)[0];
  double ytest = REAL(y)[0];
  int I = INTEGER(II)[0];
  int J = INTEGER(JJ)[0];
  
  UNPROTECT(4);
    
  PROTECT(xv = AS_NUMERIC(xv));
  PROTECT(yv = AS_NUMERIC(yv));
  double *xvert = NUMERIC_POINTER(xv);
  double *yvert = NUMERIC_POINTER(yv);
  
  int i, j;
  SEXP ret;
  
  if (find_zone(xvert, yvert, I, J, xtest, ytest, &i, &j)){

    PROTECT(ret = NEW_INTEGER(2));
    INTEGER(ret)[0] = i;
    INTEGER(ret)[1] = j;
    UNPROTECT(3);
    return ret;
  }

  UNPROTECT(2);
  return R_NilValue;
}


	    
 



R_CallMethodDef callMethods[] = {
  {"Rpoly_pnpoly", (DL_FUNC) &Rpoly_pnpoly, 4},
  {"Rpoly_find_zone", (DL_FUNC) &Rpoly_find_zone, 6},
  {NULL, NULL, 0}
};

void 
R_init_tecutils(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}




    
