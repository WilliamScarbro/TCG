#include "Generic.h"
#include "Monty.h"

void square(int p,int n, int* X, int* Y, int* pcc){
  for (int i=0; i<n; i++){
    Y[i]=0;
    for (int j=0; j<n; j++){
      Y[i]+=X[j]*pcc[i*n+j];
      Y[i]%=p;
    }
  }
}

void square_monty( monty_str* monty,int n, int* X, int* Y, int* pcc){
  for (int i=0; i<n; i++){
    Y[i]=0;
    for (int j=0; j<n; j++){
      Y[i]+=REDC(monty,X[j]*pcc[i*n+j]);
    }
  }
}

// pcc : k*k
// X,Y : k*m
void itensor(int p, int k, int m, int* X, int* Y,int* pcc){
  int i0,i1,i2,y_index;
  for (i0=0; i0<k; i0++){
    for (i1=0; i1<m; i1++){
      y_index=m*i0+i1;
      Y[y_index]=0;
      for (int i2=0; i2<k; i2++){
	Y[y_index]+=X[m*i2+i1]*pcc[k*i0+i2];
	Y[y_index]%=p;
      }
    }
  }
}

// pcc : k*k
// X,Y : k*m
void itensor_monty(monty_str* monty,int k, int m, int* X, int* Y,int* pcc){
  int i0,i1,i2,y_index;
  for (i0=0; i0<k; i0++){
    for (i1=0; i1<m; i1++){
      y_index=m*i0+i1;
      Y[y_index]=0;
      for (int i2=0; i2<k; i2++){
	Y[y_index]+=REDC(monty,X[m*i2+i1]*pcc[k*i0+i2]);
      }
    }
  }
}


    
void diagonal(int p, int n, int* X, int* Y,int* pcc){
  for (int i=0; i<n; i++)
    Y[i]=(pcc[i]*X[i])%p;
}
    
void diagonal_monty(monty_str* monty,int n, int* X, int* Y,int* pcc){
  for (int i=0; i<n; i++)
    Y[i]=REDC(monty,pcc[i]*X[i]);
}



    
