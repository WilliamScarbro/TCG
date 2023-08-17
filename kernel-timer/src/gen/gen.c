#include <stdio.h>
#include <stdlib.h>
#include "../Util.h"
#include "../timer.h"
#include "../Monty.h"

void gen(int* X,int* Y,monty_str* monty){
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
    t0 = (X[0] + X[4]);
    t4 = (X[0] - X[4]);
    t1 = (X[1] + X[5]);
    t5 = (X[1] - X[5]);
    t2 = (X[2] + X[6]);
    t6 = (X[2] - X[6]);
    t3 = (X[3] + X[7]);
    t7 = (X[3] - X[7]);
    t8 = (t0 + t2);
    t10 = (t0 - t2);
    t9 = (t1 + t3);
    t11 = (t1 - t3);
    t12 = (t4 + REDC(monty,(8 * t6)));
    t14 = (t4 + REDC(monty,(9 * t6)));
    t13 = (t5 + REDC(monty,(8 * t7)));
    t15 = (t5 + REDC(monty,(9 * t7)));
    t16 = (t8 + t9);
    t17 = (t8 - t9);
    t18 = (t10 + REDC(monty,(8 * t11)));
    t19 = (t10 + REDC(monty,(9 * t11)));
    t20 = (t12 + REDC(monty,(16 * t13)));
    t21 = (t12 + REDC(monty,(1 * t13)));
    t22 = (t14 + REDC(monty,(4 * t15)));
    t23 = (t14 + REDC(monty,(13 * t15)));
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
    
    monty_str monty;
    monty_init(&monty,17,32,15,8,5);
    initialize_timer();
    start_timer();
    
    for(int i=0; i<8; i++){
    X[i] = toResidue(&monty,X[i]);
    }
    
    gen(X,Y,&monty);
    for(int i=0; i<8; i++){
    Y[i] = fromResidue(&monty,Y[i]);
    }
    
    
    stop_timer();
    printf("Elapsed time: %f\n",elapsed_time());
    
    for(int i=0; i<8; i++){
      Y[i]=(((Y[i]+17)%17)+17)%17;
    }
    
    print_array("result",Y,8);
    
    free(X);
    free(Y);
    

}

// op count: fromList [(CBinary CAdd,17),(CBinary CSubtract,7),(CBinary CMultiply,10)]
