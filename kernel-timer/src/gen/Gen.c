#include <stdio.h>
#include <stdlib.h>
#include "../Util.h"
#include "../timer.h"

void gen(int* X,int *Y){
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
    int t24;
    int t25;
    int t26;
    int t27;
    int t28;
    int t29;
    int t30;
    int t31;
    int t32;
    int t33;
    int t34;
    int t35;
    int t36;
    int t37;
    int t38;
    int t39;
    int t40;
    int t41;
    int t42;
    int t43;
    int t44;
    int t45;
    int t46;
    int t47;
    int t48;
    int t49;
    int t50;
    int t51;
    int t52;
    int t53;
    int t54;
    int t55;
    int t56;
    int t57;
    int t58;
    int t59;
    int t60;
    int t61;
    int t62;
    int t63;
    t0 = ((X[0] + X[8]) % 17);
    t8 = ((X[0] - X[8]) % 17);
    t1 = ((X[1] + X[9]) % 17);
    t9 = ((X[1] - X[9]) % 17);
    t2 = ((X[2] + X[10]) % 17);
    t10 = ((X[2] - X[10]) % 17);
    t3 = ((X[3] + X[11]) % 17);
    t11 = ((X[3] - X[11]) % 17);
    t4 = ((X[4] + X[12]) % 17);
    t12 = ((X[4] - X[12]) % 17);
    t5 = ((X[5] + X[13]) % 17);
    t13 = ((X[5] - X[13]) % 17);
    t6 = ((X[6] + X[14]) % 17);
    t14 = ((X[6] - X[14]) % 17);
    t7 = ((X[7] + X[15]) % 17);
    t15 = ((X[7] - X[15]) % 17);
    t16 = ((t0 + t4) % 17);
    t20 = ((t0 - t4) % 17);
    t17 = ((t1 + t5) % 17);
    t21 = ((t1 - t5) % 17);
    t18 = ((t2 + t6) % 17);
    t22 = ((t2 - t6) % 17);
    t19 = ((t3 + t7) % 17);
    t23 = ((t3 - t7) % 17);
    t24 = ((t8 + ((13 * t12) % 17)) % 17);
    t28 = ((t8 + ((4 * t12) % 17)) % 17);
    t25 = ((t9 + ((13 * t13) % 17)) % 17);
    t29 = ((t9 + ((4 * t13) % 17)) % 17);
    t26 = ((t10 + ((13 * t14) % 17)) % 17);
    t30 = ((t10 + ((4 * t14) % 17)) % 17);
    t27 = ((t11 + ((13 * t15) % 17)) % 17);
    t31 = ((t11 + ((4 * t15) % 17)) % 17);
    t32 = ((t16 + t18) % 17);
    t34 = ((t16 - t18) % 17);
    t33 = ((t17 + t19) % 17);
    t35 = ((t17 - t19) % 17);
    t36 = ((t20 + ((13 * t22) % 17)) % 17);
    t38 = ((t20 + ((4 * t22) % 17)) % 17);
    t37 = ((t21 + ((13 * t23) % 17)) % 17);
    t39 = ((t21 + ((4 * t23) % 17)) % 17);
    t40 = ((t24 + ((9 * t26) % 17)) % 17);
    t42 = ((t24 + ((8 * t26) % 17)) % 17);
    t41 = ((t25 + ((9 * t27) % 17)) % 17);
    t43 = ((t25 + ((8 * t27) % 17)) % 17);
    t44 = ((t28 + ((15 * t30) % 17)) % 17);
    t46 = ((t28 + ((2 * t30) % 17)) % 17);
    t45 = ((t29 + ((15 * t31) % 17)) % 17);
    t47 = ((t29 + ((2 * t31) % 17)) % 17);
    t48 = ((t32 + t33) % 17);
    t49 = ((t32 - t33) % 17);
    t50 = ((t34 + ((13 * t35) % 17)) % 17);
    t51 = ((t34 + ((4 * t35) % 17)) % 17);
    t52 = ((t36 + ((9 * t37) % 17)) % 17);
    t53 = ((t36 + ((8 * t37) % 17)) % 17);
    t54 = ((t38 + ((15 * t39) % 17)) % 17);
    t55 = ((t38 + ((2 * t39) % 17)) % 17);
    t56 = ((t40 + ((3 * t41) % 17)) % 17);
    t57 = ((t40 + ((14 * t41) % 17)) % 17);
    t58 = ((t42 + ((5 * t43) % 17)) % 17);
    t59 = ((t42 + ((12 * t43) % 17)) % 17);
    t60 = ((t44 + ((10 * t45) % 17)) % 17);
    t61 = ((t44 + ((7 * t45) % 17)) % 17);
    t62 = ((t46 + ((11 * t47) % 17)) % 17);
    t63 = ((t46 + ((6 * t47) % 17)) % 17);
    Y[0] = t48;
    Y[1] = t49;
    Y[2] = t50;
    Y[3] = t51;
    Y[4] = t52;
    Y[5] = t53;
    Y[6] = t54;
    Y[7] = t55;
    Y[8] = t56;
    Y[9] = t57;
    Y[10] = t58;
    Y[11] = t59;
    Y[12] = t60;
    Y[13] = t61;
    Y[14] = t62;
    Y[15] = t63;

}

int main(int argc,char** argv){
    int* X = malloc(sizeof(int)*16);
    int* Y = malloc(sizeof(int)*16);
    
    for(int i=0; i<16; i++){
      X[i]=i;
    }
    
    initialize_timer();
    start_timer();
    
    gen(X,Y);
    
    stop_timer();
    printf("Elapsed time: %f\n",elapsed_time());
    
    for(int i=0; i<16; i++){
      Y[i]=(((Y[i]+17)%17)+17)%17;
    }
    
    print_array("result",Y,16);
    
    free(X);
    free(Y);
    

}

// op count: fromList [(CBinary CAdd,49),(CBinary CSubtract,15),(CBinary CMultiply,34),(CBinary CModulo,98)]
