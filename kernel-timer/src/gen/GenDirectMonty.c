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
    t0 = (X[0] + X[2]);
    t1 = (X[0] - X[2]);
    t2 = (X[1] + X[3]);
    t3 = (X[1] - X[3]);
    t4 = (t0 + t2);
    t5 = (t0 - t2);
    t6 = (t1 + REDC(monty,(1 * t3)));
    t7 = (t1 + REDC(monty,(4 * t3)));
    Y[0] = t4;
    Y[1] = t5;
    Y[2] = t6;
    Y[3] = t7;

}

int main(int argc,char** argv){
    int* X = malloc(sizeof(int)*4);
    int* Y = malloc(sizeof(int)*4);
    
    for(int i=0; i<4; i++){
      X[i]=i;
    }
    
    monty_str monty;
    monty_init(&monty,5,8,3,2);
    for(int i=0; i<4; i++){
      toResidue(&monty,X[i]);
    }
    
    initialize_timer();
    start_timer();
    
    gen(X,Y,&monty);
    
    stop_timer();
    printf("Elapsed time: %f\n",elapsed_time());
    
    for(int i=0; i<4; i++){
      fromResidue(&monty,Y[i]);
    }
    
    for(int i=0; i<4; i++){
      Y[i]=(((Y[i]+5)%5)+5)%5;
    }
    
    print_array("result",Y,4);
    
    free(X);
    free(Y);
    

}

// op count: fromList [(CBinary CAdd,5),(CBinary CSubtract,3),(CBinary CMultiply,2)]
