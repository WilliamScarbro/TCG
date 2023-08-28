#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
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
    int t80;
    int t81;
    int t82;
    int t83;
    int t84;
    int t85;
    int t86;
    int t87;
    int t88;
    int t89;
    int t90;
    int t91;
    int t92;
    int t93;
    int t94;
    int t95;
    t0 = (X[0] + REDC(monty,(3 * X[8])));
    t8 = (X[0] + REDC(monty,(94 * X[8])));
    t1 = (X[1] + REDC(monty,(3 * X[9])));
    t9 = (X[1] + REDC(monty,(94 * X[9])));
    t2 = (X[2] + REDC(monty,(3 * X[10])));
    t10 = (X[2] + REDC(monty,(94 * X[10])));
    t3 = (X[3] + REDC(monty,(3 * X[11])));
    t11 = (X[3] + REDC(monty,(94 * X[11])));
    t4 = (X[4] + REDC(monty,(3 * X[12])));
    t12 = (X[4] + REDC(monty,(94 * X[12])));
    t5 = (X[5] + REDC(monty,(3 * X[13])));
    t13 = (X[5] + REDC(monty,(94 * X[13])));
    t6 = (X[6] + REDC(monty,(3 * X[14])));
    t14 = (X[6] + REDC(monty,(94 * X[14])));
    t7 = (X[7] + REDC(monty,(3 * X[15])));
    t15 = (X[7] + REDC(monty,(94 * X[15])));
    t16 = t0;
    t17 = t4;
    t18 = t1;
    t19 = t5;
    t20 = t2;
    t21 = t6;
    t22 = t3;
    t23 = t7;
    t24 = t8;
    t25 = t12;
    t26 = t9;
    t27 = t13;
    t28 = t10;
    t29 = t14;
    t30 = t11;
    t31 = t15;
    t32 = (t16 + REDC(monty,(44 * t17)));
    t33 = (t16 + REDC(monty,(53 * t17)));
    t34 = (t18 + REDC(monty,(44 * t19)));
    t35 = (t18 + REDC(monty,(53 * t19)));
    t36 = (t20 + REDC(monty,(44 * t21)));
    t37 = (t20 + REDC(monty,(53 * t21)));
    t38 = (t22 + REDC(monty,(44 * t23)));
    t39 = (t22 + REDC(monty,(53 * t23)));
    t40 = (t24 + REDC(monty,(95 * t25)));
    t41 = (t24 + REDC(monty,(2 * t25)));
    t42 = (t26 + REDC(monty,(95 * t27)));
    t43 = (t26 + REDC(monty,(2 * t27)));
    t44 = (t28 + REDC(monty,(95 * t29)));
    t45 = (t28 + REDC(monty,(2 * t29)));
    t46 = (t30 + REDC(monty,(95 * t31)));
    t47 = (t30 + REDC(monty,(2 * t31)));
    t48 = t32;
    t49 = t34;
    t50 = t36;
    t51 = t38;
    t52 = t33;
    t53 = t35;
    t54 = t37;
    t55 = t39;
    t56 = t40;
    t57 = t42;
    t58 = t44;
    t59 = t46;
    t60 = t41;
    t61 = t43;
    t62 = t45;
    t63 = t47;
    t64 = (t48 + REDC(monty,(54 * t50)));
    t66 = (t48 + REDC(monty,(43 * t50)));
    t65 = (t49 + REDC(monty,(54 * t51)));
    t67 = (t49 + REDC(monty,(43 * t51)));
    t68 = (t52 + REDC(monty,(24 * t54)));
    t70 = (t52 + REDC(monty,(73 * t54)));
    t69 = (t53 + REDC(monty,(24 * t55)));
    t71 = (t53 + REDC(monty,(73 * t55)));
    t72 = (t56 + REDC(monty,(61 * t58)));
    t74 = (t56 + REDC(monty,(36 * t58)));
    t73 = (t57 + REDC(monty,(61 * t59)));
    t75 = (t57 + REDC(monty,(36 * t59)));
    t76 = (t60 + REDC(monty,(81 * t62)));
    t78 = (t60 + REDC(monty,(16 * t62)));
    t77 = (t61 + REDC(monty,(81 * t63)));
    t79 = (t61 + REDC(monty,(16 * t63)));
    t80 = (t64 + REDC(monty,(92 * t65)));
    t81 = (t64 + REDC(monty,(5 * t65)));
    t82 = (t66 + REDC(monty,(84 * t67)));
    t83 = (t66 + REDC(monty,(13 * t67)));
    t84 = (t68 + REDC(monty,(68 * t69)));
    t85 = (t68 + REDC(monty,(29 * t69)));
    t86 = (t70 + REDC(monty,(41 * t71)));
    t87 = (t70 + REDC(monty,(56 * t71)));
    t88 = (t72 + REDC(monty,(57 * t73)));
    t89 = (t72 + REDC(monty,(40 * t73)));
    t90 = (t74 + REDC(monty,(90 * t75)));
    t91 = (t74 + REDC(monty,(7 * t75)));
    t92 = (t76 + REDC(monty,(59 * t77)));
    t93 = (t76 + REDC(monty,(38 * t77)));
    t94 = (t78 + REDC(monty,(37 * t79)));
    t95 = (t78 + REDC(monty,(60 * t79)));
    Y[0] = t80;
    Y[1] = t81;
    Y[2] = t82;
    Y[3] = t83;
    Y[4] = t84;
    Y[5] = t85;
    Y[6] = t86;
    Y[7] = t87;
    Y[8] = t88;
    Y[9] = t89;
    Y[10] = t90;
    Y[11] = t91;
    Y[12] = t92;
    Y[13] = t93;
    Y[14] = t94;
    Y[15] = t95;

}

int main(int argc,char** argv){
    int* X = malloc(sizeof(int)*16);
    int* Y = malloc(sizeof(int)*16);
    
    for(int i=0; i<16; i++){
      X[i]=i;
    }
    
    monty_str monty;
    monty_init(&monty,97,128,95,72,7);
    initialize_timer();
    start_timer();
    
    for(int i=0; i<16; i++){
    X[i] = toResidue(&monty,X[i]);
    }
    
    gen(X,Y,&monty);
    for(int i=0; i<16; i++){
    Y[i] = fromResidue(&monty,Y[i]);
    }
    
    
    stop_timer();
    printf("Elapsed time: %f\n",elapsed_time());
    
    for(int i=0; i<16; i++){
      Y[i]=(((Y[i]+97)%97)+97)%97;
    }
    
    print_array("result",Y,16);
    
    free(X);
    free(Y);
    

}

// op count: fromList [(CBinary CAdd,64),(CBinary CMultiply,64)]
