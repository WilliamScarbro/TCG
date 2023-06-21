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
int t18;
int t19;
int t20;
int t21;
int t22;
int t23;
t0 = ((X[0] + X[4]) % 17);
t1 = ((X[0] + ((16 * X[4]) % 17)) % 17);
t2 = ((X[1] + X[5]) % 17);
t3 = ((X[1] + ((16 * X[5]) % 17)) % 17);
t4 = ((X[2] + X[6]) % 17);
t5 = ((X[2] + ((16 * X[6]) % 17)) % 17);
t6 = ((X[3] + X[7]) % 17);
t7 = ((X[3] + ((16 * X[7]) % 17)) % 17);
t8 = ((t0 + t4) % 17);
t9 = ((t0 + ((16 * t4) % 17)) % 17);
t10 = ((t2 + t6) % 17);
t11 = ((t2 + ((16 * t6) % 17)) % 17);
t12 = ((t1 + ((13 * t5) % 17)) % 17);
t13 = ((t1 + ((4 * t5) % 17)) % 17);
t14 = ((t3 + ((13 * t7) % 17)) % 17);
t15 = ((t3 + ((4 * t7) % 17)) % 17);
t16 = ((t8 + t10) % 17);
t17 = ((t8 + ((16 * t10) % 17)) % 17);
t18 = ((t9 + ((13 * t11) % 17)) % 17);
t19 = ((t9 + ((4 * t11) % 17)) % 17);
t20 = ((t12 + ((9 * t14) % 17)) % 17);
t21 = ((t12 + ((8 * t14) % 17)) % 17);
t22 = ((t13 + ((15 * t15) % 17)) % 17);
t23 = ((t13 + ((2 * t15) % 17)) % 17);
Y[0] = t16;
Y[1] = t17;
Y[2] = t18;
Y[3] = t19;
Y[4] = t20;
Y[5] = t21;
Y[6] = t22;
Y[7] = t23;

}

int main(int argc,char** argv){
  int* X = malloc(sizeof(int)*8);
  int* Y = malloc(sizeof(int)*8);

  for(int i=0; i<8; i++){
    X[i]=i;
  }
  gen(X,Y);

  print_array("result",Y,8);

  free(X);
  free(Y);
}

