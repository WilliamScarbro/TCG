#include "Multiply.h"
#include "Monty.h"


void point_multiply(int n, int *X,int *Y,int *Z){
  for (int i=0; i<n; i++){
    Z[i]=X[i]*Y[i];
  }
}


void point_multiply_monty(int n, int *X,int *Y,int *Z,monty_str* monty){
  for (int i=0; i<n; i++){
    Z[i]=REDC(monty,X[i]*Y[i]);
  }
}


