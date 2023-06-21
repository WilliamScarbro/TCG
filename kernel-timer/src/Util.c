#include "Util.h"


void Id(int n, int* X, int* Y){
  for (int i=0; i<n; i++){
    Y[i]=X[i];
  }
}

void swap(int **X, int **Y){
  int *temp=*X;
  *X=*Y;
  *Y=temp;
}

void print_array(char* name,int* arr,int len){
  printf("%s: ",name);
  for(int i=0; i<len; i++){
    printf("%d ",arr[i]);
  }
  printf("\n");
}
