
#include <stdio.h>
#include <stdlib.h>


#include "LPerm.h"

void LPerm(int n, int k, int* X, int *Y){
  #ifdef DEBUG
  printf("LPerm %d %d\n",n,k); 
  #endif
  int m=n/k;
  for (int i=0; i<m; i++){
    for (int j=0; j<k; j++){
      Y[j+k*i]=X[m*j+i];
    }
  }
}
  
void TPerm(int di, int dj, int dk, int* X, int* Y){
  #ifdef DEBUG
  printf("TPerm %d %d %d\n",di,dj,dk);
  #endif

  for (int i=0; i<di; i++){
    for (int j=0; j<dj; j++){
      for (int k=0; k<dk; k++){
	Y[(i*dj+j)*dk+k]=X[(j*di+i)*dk+k];
      }
    }
  }
//  int m=(n/k)/l;
//  
//  for (int h=0; h<l; h++){
//    for (int i=0; i<m; i++){
//      for (int j=0; j<k; j++){
//	Y[(j+k*i)*l+h]=X[(m*j+i)*l+h];
//      }
//    }
//  }
}
    
