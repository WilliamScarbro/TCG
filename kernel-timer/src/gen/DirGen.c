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
    int t64;
    int t65;
    int t66;
    int t67;
    int t68;
    int t69;
    int t70;
    int t71;
    int t72;
    int t73;
    int t74;
    int t75;
    int t76;
    int t77;
    int t78;
    int t79;
    t0 = (X[0] + REDC(monty,(16 * X[8])));
    t1 = (X[0] + REDC(monty,(241 * X[8])));
    t2 = (X[1] + REDC(monty,(16 * X[9])));
    t3 = (X[1] + REDC(monty,(241 * X[9])));
    t4 = (X[2] + REDC(monty,(16 * X[10])));
    t5 = (X[2] + REDC(monty,(241 * X[10])));
    t6 = (X[3] + REDC(monty,(16 * X[11])));
    t7 = (X[3] + REDC(monty,(241 * X[11])));
    t8 = (X[4] + REDC(monty,(16 * X[12])));
    t9 = (X[4] + REDC(monty,(241 * X[12])));
    t10 = (X[5] + REDC(monty,(16 * X[13])));
    t11 = (X[5] + REDC(monty,(241 * X[13])));
    t12 = (X[6] + REDC(monty,(16 * X[14])));
    t13 = (X[6] + REDC(monty,(241 * X[14])));
    t14 = (X[7] + REDC(monty,(16 * X[15])));
    t15 = (X[7] + REDC(monty,(241 * X[15])));
    t16 = t0;
    t17 = REDC(monty,(239 * t2));
    t18 = REDC(monty,(95 * t4));
    t19 = REDC(monty,(84 * t6));
    t20 = REDC(monty,(242 * t8));
    t21 = REDC(monty,(122 * t10));
    t22 = REDC(monty,(70 * t12));
    t23 = REDC(monty,(116 * t14));
    t24 = t1;
    t25 = REDC(monty,(144 * t3));
    t26 = REDC(monty,(169 * t5));
    t27 = REDC(monty,(168 * t7));
    t28 = REDC(monty,(240 * t9));
    t29 = REDC(monty,(196 * t11));
    t30 = REDC(monty,(23 * t13));
    t31 = REDC(monty,(143 * t15));
    t32 = (t16 + t20);
    t33 = (t16 - t20);
    t34 = (t17 + t21);
    t35 = (t17 - t21);
    t36 = (t18 + t22);
    t37 = (t18 - t22);
    t38 = (t19 + t23);
    t39 = (t19 - t23);
    t40 = (t24 + t28);
    t41 = (t24 - t28);
    t42 = (t25 + t29);
    t43 = (t25 - t29);
    t44 = (t26 + t30);
    t45 = (t26 - t30);
    t46 = (t27 + t31);
    t47 = (t27 - t31);
    t48 = (t32 + t36);
    t49 = (t32 - t36);
    t50 = (t34 + t38);
    t51 = (t34 - t38);
    t52 = (t33 + REDC(monty,(32 * t37)));
    t53 = (t33 + REDC(monty,(225 * t37)));
    t54 = (t35 + REDC(monty,(32 * t39)));
    t55 = (t35 + REDC(monty,(225 * t39)));
    t56 = (t40 + t44);
    t57 = (t40 - t44);
    t58 = (t42 + t46);
    t59 = (t42 - t46);
    t60 = (t41 + REDC(monty,(32 * t45)));
    t61 = (t41 + REDC(monty,(225 * t45)));
    t62 = (t43 + REDC(monty,(32 * t47)));
    t63 = (t43 + REDC(monty,(225 * t47)));
    t64 = (t48 + t50);
    t65 = (t48 - t50);
    t66 = (t49 + REDC(monty,(32 * t51)));
    t67 = (t49 + REDC(monty,(225 * t51)));
    t68 = (t52 + REDC(monty,(129 * t54)));
    t69 = (t52 + REDC(monty,(128 * t54)));
    t70 = (t53 + REDC(monty,(249 * t55)));
    t71 = (t53 + REDC(monty,(8 * t55)));
    t72 = (t56 + t58);
    t73 = (t56 - t58);
    t74 = (t57 + REDC(monty,(32 * t59)));
    t75 = (t57 + REDC(monty,(225 * t59)));
    t76 = (t60 + REDC(monty,(129 * t62)));
    t77 = (t60 + REDC(monty,(128 * t62)));
    t78 = (t61 + REDC(monty,(249 * t63)));
    t79 = (t61 + REDC(monty,(8 * t63)));
    Y[0] = t64;
    Y[1] = t65;
    Y[2] = t66;
    Y[3] = t67;
    Y[4] = t68;
    Y[5] = t69;
    Y[6] = t70;
    Y[7] = t71;
    Y[8] = t72;
    Y[9] = t73;
    Y[10] = t74;
    Y[11] = t75;
    Y[12] = t76;
    Y[13] = t77;
    Y[14] = t78;
    Y[15] = t79;

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
