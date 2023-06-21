#include <stdio.h>
#include <stdlib.h>
#include "NTLib.h"

int power(int x, int e, int p){
  //printf("Flag power %d^%d mod %d\n",x,e,p);
  int res=1;
  //printf("pow %d %d %d\n",res,x,e);
  while (e>0){
    if (e%2==1)
      res=(res*x)%p;
    x=(x*x)%p;
    e/=2;
    //printf("pow %d %d %d\n",res,x,e);
  }
  //printf("End power %d\n",x);
  if (res<0)
    res=p+res;
  return res;
}

// bad things will happen if p is not prime, but we do not check
int generator(int p){
  //printf("Flag generator %d\n",p);
  for (int i=2; i<p; i++){
    int x=i;
    int isgen=1;
    for (int j=1; j<p-1; j++){
      if (x==1){
	isgen=0;
	break;
      }
      //printf("x %d\n",x);
      x=(x*i)%p;
    }
    if (isgen==1){
      //printf("End generator %d\n",i);
      return i;
    }
  }
  //printf("End generator, not found");
  return 0;
}

int inverse(int x, int p){
  int y=x;
  int counter=1;
  while (y!=1&&(counter++)<p){
    y=(y*x)%p;
  }
  if (y!=1){
    printf("Error: %d is not prime\n",p);
    exit(1);
  }
  int inv=power(x,counter-1,p);
  //printf("inverse: %d*%d=%d\n",x,inv,x*inv%p);
  return inv;
}

int Nth_root(int p, int gen, int N){
  //printf("Flag Nth_root\n");
  if ((p-1)%N!=0){
    printf("%d does not divide %d-1\n",N,p);
    exit(1);
  }
  //printf("end Nth_root\n");
  return power(gen,(p-1)/N,p);
}

// W is length N+1 with 1s at either end
void root_powers(int p, int N, int* W){
  //printf("Flag root_powers %d %d\n",p,N);
  int g=generator(p);
  int w=Nth_root(p,g,N);
  W[0]=1;
  //printf("W: ");
  for (int i=1; i<N+1; i++){
    //printf("%d ",W[i-1]);
    W[i]=(W[i-1]*w)%p;
  }
  //printf("%d\n",W[N-1]);
  //printf("end root_powers\n");
}
