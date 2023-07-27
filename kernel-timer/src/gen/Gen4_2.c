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
    t0 = (X[0] + (X[1] + (X[2] + X[3])));
    t1 = (X[0] + (REDC(monty,(1 * X[1])) + (REDC(monty,(4 * X[3])) - X[2])));
    t2 = (X[0] + ((X[2] - X[3]) - X[1]));
    t3 = (X[0] + (REDC(monty,(4 * X[1])) + (REDC(monty,(1 * X[3])) - X[2])));
    Y[0] = t0;
    Y[1] = t1;
    Y[2] = t2;
    Y[3] = t3;

}

int main(int argc,char** argv){
    int* X = malloc(sizeof(int)*4);
    int* Y = malloc(sizeof(int)*4);
    
    for(int i=0; i<4; i++){
      X[i]=i;
    }
    
    monty_str monty;
    monty_init(&monty,5,8,3,2,3);
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

// op count: fromList [(CBinary CAdd,8),(CBinary CSubtract,4),(CBinary CMultiply,4)]
