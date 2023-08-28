#include "Util.h"

#include <stdio.h>
#include <stdlib.h>


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

int *allocate(int n, int *staticArray) {
    int *newArray = (int *)malloc(n * sizeof(int));

    if (newArray == NULL) {
        printf("Memory allocation failed.\n");
        return NULL;
    }

    for (int i = 0; i < n; i++) {
        newArray[i] = staticArray[i];
    }

    return newArray;
}
