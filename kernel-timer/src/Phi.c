#include <stdio.h>

#include "NTLib.h"
#include "Phi.h"

void Phi_W(int w,int d,int N,int k,int p,int *W){
  int z,i;
  for (z=0; z<k; z++){
    for (i=0; i<k; i++){
      int index=power((d+z*N)/k*i,1,N);
      W[z*k+i]=power(w,index,p);
    }
  }
}

void Phi(int n,int k,int d,int N,int p,int *X,int *Y,int *W){
  #ifdef DEBUG
  printf("Phi n:%d k:%d d:%d N:%d p:%d\n",n,k,d,N,p);
  #endif
  int m=n/k;
  int z,j,i;
  for(z=0; z<k; z++){
    for(j=0; j<m; j++){
      // for less ops
      int w_step=(d+z*N)/k;
      int y_new=0;
      for(i=0; i<k; i++){
	//printf("(%d %d %d) X:%d W_i:%d\n",z,j,i,X[i*m+j],w_ind);
	y_new+=(X[i*m+j]*W[z*k+i])%p;
      }
      Y[z*m+j]=y_new%p;
    }
  }
}

void Phi_inv_W(int w,int d,int N,int k,int p,int *W){
  int z,i;
  for (z=0; z<k; z++){
    for (i=0; i<k; i++){
      int index=power(-1*(d+z*N)/k*i,1,N);
      W[i*k+z]=power(w,index,p);
    }
  }
}

void Phi_inv(int n,int k,int d,int N,int p,int *X,int *Y,int *W){
  #ifdef DEBUG
  printf("Phi_inv n:%d k:%d d:%d N:%d p:%d\n",n,k,d,N,p);
  #endif
  int m=n/k;
  int z,j,i;
  int k_inv=inverse(k,p);
  for(i=0; i<k; i++){
    for(j=0; j<m; j++){
      // for less ops
      //int w_step=(d+z*N)/k;
      int y_new=0;
      for(z=0; z<k; z++){
	//printf("(%d %d %d) X:%d wi:%d W_i:%d\n",i,j,z,X[z*m+j],w_ind,W[w_ind]);
	y_new+=(X[z*m+j]*W[i*k+z])%p;
      }
      Y[i*m+j]=(y_new*k_inv)%p;
      //printf("<- %d\n",bad);
    }
  }
}
