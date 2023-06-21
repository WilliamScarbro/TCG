#include "Mult.h"

void mult(int n, int p, int *X, int *Y,int *Z){
  for(int i=0; i<n; i++){
    Z[i]=(X[i]*Y[i])%p;
  }
}
