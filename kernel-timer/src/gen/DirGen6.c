#include <stdio.h>
#include <stdlib.h>
#include "../Util.h"

void gen(int* X, int* Y){
int t0;
int t1;
int t2;
int t3;
int t4;
int t5;
int t6;
int t7;
int t8;
int t9;
int t10;
int t11;
int t12;
int t13;
int t14;
int t15;
int t16;
int t17;
t0 = ((X[0] + ((X[2] + X[4]) % 7)) % 7);
t1 = ((X[0] + ((((2 * X[2]) % 7) + ((4 * X[4]) % 7)) % 7)) % 7);
t2 = ((X[0] + ((((4 * X[2]) % 7) + ((2 * X[4]) % 7)) % 7)) % 7);
t3 = ((X[1] + ((X[3] + X[5]) % 7)) % 7);
t4 = ((X[1] + ((((2 * X[3]) % 7) + ((4 * X[5]) % 7)) % 7)) % 7);
t5 = ((X[1] + ((((4 * X[3]) % 7) + ((2 * X[5]) % 7)) % 7)) % 7);
t6 = t0;
t7 = t2;
t8 = t4;
t9 = ((3 * t1) % 7);
t10 = t3;
t11 = ((2 * t5) % 7);
t12 = ((t6 + t7) % 7);
t13 = ((t6 + ((6 * t7) % 7)) % 7);
t14 = ((t8 + t9) % 7);
t15 = ((t8 + ((6 * t9) % 7)) % 7);
t16 = ((t10 + t11) % 7);
t17 = ((t10 + ((6 * t11) % 7)) % 7);
Y[0] = t12;
Y[1] = t13;
Y[2] = t14;
Y[3] = t15;
Y[4] = t16;
Y[5] = t17;

}

int main(int argc,char** argv){
  int* X = malloc(sizeof(int)*6);
  int* Y = malloc(sizeof(int)*6);

  for(int i=0; i<6; i++){
    X[i]=i;
  }
  gen(X,Y);

  print_array("result",Y,6);

  free(X);
  free(Y);
}

