#include <stdio.h>
#include <stdlib.h>
#include "../Util.h"
#include "../timer.h"
#include "../Monty.h"

void gen(int* X,int* Y,monty_str* monty){
    Y[0] = (X[0] + REDC(monty,(16 * X[8])));
    Y[8] = (X[0] + REDC(monty,(241 * X[8])));
    Y[1] = (X[1] + REDC(monty,(16 * X[9])));
    Y[9] = (X[1] + REDC(monty,(241 * X[9])));
    Y[2] = (X[2] + REDC(monty,(16 * X[10])));
    Y[10] = (X[2] + REDC(monty,(241 * X[10])));
    Y[3] = (X[3] + REDC(monty,(16 * X[11])));
    Y[11] = (X[3] + REDC(monty,(241 * X[11])));
    Y[4] = (X[4] + REDC(monty,(16 * X[12])));
    Y[12] = (X[4] + REDC(monty,(241 * X[12])));
    Y[5] = (X[5] + REDC(monty,(16 * X[13])));
    Y[13] = (X[5] + REDC(monty,(241 * X[13])));
    Y[6] = (X[6] + REDC(monty,(16 * X[14])));
    Y[14] = (X[6] + REDC(monty,(241 * X[14])));
    Y[7] = (X[7] + REDC(monty,(16 * X[15])));
    Y[15] = (X[7] + REDC(monty,(241 * X[15])));
    X[0] = Y[0];
    X[1] = REDC(monty,(239 * Y[1]));
    X[2] = REDC(monty,(95 * Y[2]));
    X[3] = REDC(monty,(84 * Y[3]));
    X[4] = REDC(monty,(242 * Y[4]));
    X[5] = REDC(monty,(122 * Y[5]));
    X[6] = REDC(monty,(70 * Y[6]));
    X[7] = REDC(monty,(116 * Y[7]));
    X[8] = Y[8];
    X[9] = REDC(monty,(144 * Y[9]));
    X[10] = REDC(monty,(169 * Y[10]));
    X[11] = REDC(monty,(168 * Y[11]));
    X[12] = REDC(monty,(240 * Y[12]));
    X[13] = REDC(monty,(196 * Y[13]));
    X[14] = REDC(monty,(23 * Y[14]));
    X[15] = REDC(monty,(143 * Y[15]));
    Y[0] = X[0];
    Y[1] = X[4];
    Y[2] = X[1];
    Y[3] = X[5];
    Y[4] = X[2];
    Y[5] = X[6];
    Y[6] = X[3];
    Y[7] = X[7];
    Y[8] = X[8];
    Y[9] = X[12];
    Y[10] = X[9];
    Y[11] = X[13];
    Y[12] = X[10];
    Y[13] = X[14];
    Y[14] = X[11];
    Y[15] = X[15];
    X[0] = (Y[0] + Y[1]);
    X[1] = (Y[0] - Y[1]);
    X[2] = (Y[2] + Y[3]);
    X[3] = (Y[2] - Y[3]);
    X[4] = (Y[4] + Y[5]);
    X[5] = (Y[4] - Y[5]);
    X[6] = (Y[6] + Y[7]);
    X[7] = (Y[6] - Y[7]);
    X[8] = (Y[8] + Y[9]);
    X[9] = (Y[8] - Y[9]);
    X[10] = (Y[10] + Y[11]);
    X[11] = (Y[10] - Y[11]);
    X[12] = (Y[12] + Y[13]);
    X[13] = (Y[12] - Y[13]);
    X[14] = (Y[14] + Y[15]);
    X[15] = (Y[14] - Y[15]);
    Y[0] = X[0];
    Y[1] = X[2];
    Y[2] = X[4];
    Y[3] = X[6];
    Y[4] = X[1];
    Y[5] = X[3];
    Y[6] = X[5];
    Y[7] = X[7];
    Y[8] = X[8];
    Y[9] = X[10];
    Y[10] = X[12];
    Y[11] = X[14];
    Y[12] = X[9];
    Y[13] = X[11];
    Y[14] = X[13];
    Y[15] = X[15];
    X[0] = Y[0];
    X[1] = Y[1];
    X[2] = Y[2];
    X[3] = Y[3];
    X[4] = Y[4];
    X[5] = Y[5];
    X[6] = Y[6];
    X[7] = Y[7];
    X[8] = Y[8];
    X[9] = Y[9];
    X[10] = Y[10];
    X[11] = Y[11];
    X[12] = Y[12];
    X[13] = Y[13];
    X[14] = Y[14];
    X[15] = Y[15];
    Y[0] = X[0];
    Y[1] = X[1];
    Y[2] = X[2];
    Y[3] = X[3];
    Y[4] = X[4];
    Y[5] = X[5];
    Y[6] = X[6];
    Y[7] = X[7];
    Y[8] = X[8];
    Y[9] = X[9];
    Y[10] = X[10];
    Y[11] = X[11];
    Y[12] = X[12];
    Y[13] = X[13];
    Y[14] = X[14];
    Y[15] = X[15];
    X[0] = Y[0];
    X[1] = Y[2];
    X[2] = Y[1];
    X[3] = Y[3];
    X[4] = Y[4];
    X[5] = Y[6];
    X[6] = Y[5];
    X[7] = Y[7];
    X[8] = Y[8];
    X[9] = Y[10];
    X[10] = Y[9];
    X[11] = Y[11];
    X[12] = Y[12];
    X[13] = Y[14];
    X[14] = Y[13];
    X[15] = Y[15];
    Y[0] = (X[0] + X[1]);
    Y[1] = (X[0] - X[1]);
    Y[2] = (X[2] + X[3]);
    Y[3] = (X[2] - X[3]);
    Y[4] = (X[4] + REDC(monty,(32 * X[5])));
    Y[5] = (X[4] + REDC(monty,(225 * X[5])));
    Y[6] = (X[6] + REDC(monty,(32 * X[7])));
    Y[7] = (X[6] + REDC(monty,(225 * X[7])));
    Y[8] = (X[8] + X[9]);
    Y[9] = (X[8] - X[9]);
    Y[10] = (X[10] + X[11]);
    Y[11] = (X[10] - X[11]);
    Y[12] = (X[12] + REDC(monty,(32 * X[13])));
    Y[13] = (X[12] + REDC(monty,(225 * X[13])));
    Y[14] = (X[14] + REDC(monty,(32 * X[15])));
    Y[15] = (X[14] + REDC(monty,(225 * X[15])));
    X[0] = Y[0];
    X[1] = Y[2];
    X[2] = Y[1];
    X[3] = Y[3];
    X[4] = Y[4];
    X[5] = Y[6];
    X[6] = Y[5];
    X[7] = Y[7];
    X[8] = Y[8];
    X[9] = Y[10];
    X[10] = Y[9];
    X[11] = Y[11];
    X[12] = Y[12];
    X[13] = Y[14];
    X[14] = Y[13];
    X[15] = Y[15];
    Y[0] = X[0];
    Y[1] = X[1];
    Y[2] = X[2];
    Y[3] = X[3];
    Y[4] = X[4];
    Y[5] = X[5];
    Y[6] = X[6];
    Y[7] = X[7];
    Y[8] = X[8];
    Y[9] = X[9];
    Y[10] = X[10];
    Y[11] = X[11];
    Y[12] = X[12];
    Y[13] = X[13];
    Y[14] = X[14];
    Y[15] = X[15];
    X[0] = Y[0];
    X[1] = Y[1];
    X[2] = Y[2];
    X[3] = Y[3];
    X[4] = Y[4];
    X[5] = Y[5];
    X[6] = Y[6];
    X[7] = Y[7];
    X[8] = Y[8];
    X[9] = Y[9];
    X[10] = Y[10];
    X[11] = Y[11];
    X[12] = Y[12];
    X[13] = Y[13];
    X[14] = Y[14];
    X[15] = Y[15];
    Y[0] = (X[0] + X[1]);
    Y[1] = (X[0] - X[1]);
    Y[2] = (X[2] + REDC(monty,(32 * X[3])));
    Y[3] = (X[2] + REDC(monty,(225 * X[3])));
    Y[4] = (X[4] + REDC(monty,(129 * X[5])));
    Y[5] = (X[4] + REDC(monty,(128 * X[5])));
    Y[6] = (X[6] + REDC(monty,(249 * X[7])));
    Y[7] = (X[6] + REDC(monty,(8 * X[7])));
    Y[8] = (X[8] + X[9]);
    Y[9] = (X[8] - X[9]);
    Y[10] = (X[10] + REDC(monty,(32 * X[11])));
    Y[11] = (X[10] + REDC(monty,(225 * X[11])));
    Y[12] = (X[12] + REDC(monty,(129 * X[13])));
    Y[13] = (X[12] + REDC(monty,(128 * X[13])));
    Y[14] = (X[14] + REDC(monty,(249 * X[15])));
    Y[15] = (X[14] + REDC(monty,(8 * X[15])));
    X[0] = Y[0];
    X[1] = Y[1];
    X[2] = Y[2];
    X[3] = Y[3];
    X[4] = Y[4];
    X[5] = Y[5];
    X[6] = Y[6];
    X[7] = Y[7];
    X[8] = Y[8];
    X[9] = Y[9];
    X[10] = Y[10];
    X[11] = Y[11];
    X[12] = Y[12];
    X[13] = Y[13];
    X[14] = Y[14];
    X[15] = Y[15];
    Y[0] = X[0];
    Y[1] = X[1];
    Y[2] = X[2];
    Y[3] = X[3];
    Y[4] = X[4];
    Y[5] = X[5];
    Y[6] = X[6];
    Y[7] = X[7];
    Y[8] = X[8];
    Y[9] = X[9];
    Y[10] = X[10];
    Y[11] = X[11];
    Y[12] = X[12];
    Y[13] = X[13];
    Y[14] = X[14];
    Y[15] = X[15];

}

int main(int argc,char** argv){
    int* X = malloc(sizeof(int)*16);
    int* Y = malloc(sizeof(int)*16);
    
    for(int i=0; i<16; i++){
      X[i]=i;
    }
    
    monty_str monty;
    monty_init(&monty,257,512,255,128,9);
    for(int i=0; i<16; i++){
      toResidue(&monty,X[i]);
    }
    
    initialize_timer();
    start_timer();
    
    gen(X,Y,&monty);
    
    stop_timer();
    printf("Elapsed time: %f\n",elapsed_time());
    
    for(int i=0; i<16; i++){
      fromResidue(&monty,Y[i]);
    }
    
    for(int i=0; i<16; i++){
      Y[i]=(((Y[i]+257)%257)+257)%257;
    }
    
    print_array("result",Y,16);
    
    free(X);
    free(Y);
    

}

// op count: fromList [(CBinary CAdd,50),(CBinary CSubtract,14),(CBinary CMultiply,50)]
