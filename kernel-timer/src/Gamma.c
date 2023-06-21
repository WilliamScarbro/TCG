#include <stdio.h>

#include "Gamma.h"
#include "NTLib.h"

void Gamma_W(int w, int n, int d, int N, int p, int* W){
  for (int i=0; i<n; i++){
    int index = (N-d)/n*i;
    W[i]=power(w,index,p);
  }
}

void Gamma(int n, int d, int N, int p, int* X, int* Y, int* W){
  #ifdef DEBUG
  printf("Gamma: n:%d d:%d N:%d p:%d\n",n,d,N,p);
  #endif
  for (int i=0; i<n; i++){
    Y[i]=X[i]*W[i]%p;
  }
}

void Gamma_inv_W(int w, int n, int d, int N, int p, int* W){
  for (int i=0; i<n; i++){
    int index = power(N-(N-d)/n*i,1,N);
    W[i]=power(w,index,p);
  }
}

void Gamma_inv(int n, int d, int N, int p, int* X, int* Y, int* W){
  #ifdef DEBUG
  printf("Gamma_inv: n:%d d:%d N:%d p:%d\n",n,d,N,p);
  #endif
  for (int i=0; i<n; i++){
    Y[i]=X[i]*W[i]%p;
  }
}

