#include <stdio.h>
#include <stdlib.h>
#include "../Util.h"
#include "../timer.h"
#include "../Generic.h"
#include "../LPerm.h"

void init_pcc(int*** pccList_pointer, int**** pccMap_pointer){
    *pccList_pointer=(int**)malloc(sizeof(int*)*24);
    int** pccList=*pccList_pointer;
    int pcc0[4]={255,255,255,2};
    int *dypcc0=allocate(4,pcc0);
    pccList[0]=dypcc0;
    int pcc1[4]={255,32,255,225};
    int *dypcc1=allocate(4,pcc1);
    pccList[1]=dypcc1;
    int pcc2[4]={255,129,255,128};
    int *dypcc2=allocate(4,pcc2);
    pccList[2]=dypcc2;
    int pcc3[4]={255,249,255,8};
    int *dypcc3=allocate(4,pcc3);
    pccList[3]=dypcc3;
    int pcc4[4]={255,16,255,241};
    int *dypcc4=allocate(4,pcc4);
    pccList[4]=dypcc4;
    int pcc5[4]={255,1,255,256};
    int *dypcc5=allocate(4,pcc5);
    pccList[5]=dypcc5;
    int pcc6[4]={255,253,255,4};
    int *dypcc6=allocate(4,pcc6);
    pccList[6]=dypcc6;
    int pcc7[4]={255,64,255,193};
    int *dypcc7=allocate(4,pcc7);
    pccList[7]=dypcc7;
    int pcc8[2]={255,255};
    int *dypcc8=allocate(2,pcc8);
    pccList[8]=dypcc8;
    int pcc9[2]={255,242};
    int *dypcc9=allocate(2,pcc9);
    pccList[9]=dypcc9;
    int pcc10[2]={255,16};
    int *dypcc10=allocate(2,pcc10);
    pccList[10]=dypcc10;
    int pcc11[2]={255,120};
    int *dypcc11=allocate(2,pcc11);
    pccList[11]=dypcc11;
    int pcc12[2]={255,129};
    int *dypcc12=allocate(2,pcc12);
    pccList[12]=dypcc12;
    int pcc13[2]={255,68};
    int *dypcc13=allocate(2,pcc13);
    pccList[13]=dypcc13;
    int pcc14[2]={255,253};
    int *dypcc14=allocate(2,pcc14);
    pccList[14]=dypcc14;
    int pcc15[2]={255,227};
    int *dypcc15=allocate(2,pcc15);
    pccList[15]=dypcc15;
    int pcc16[2]={255,32};
    int *dypcc16=allocate(2,pcc16);
    pccList[16]=dypcc16;
    int pcc17[2]={255,240};
    int *dypcc17=allocate(2,pcc17);
    pccList[17]=dypcc17;
    int pcc18[2]={255,1};
    int *dypcc18=allocate(2,pcc18);
    pccList[18]=dypcc18;
    int pcc19[2]={255,136};
    int *dypcc19=allocate(2,pcc19);
    pccList[19]=dypcc19;
    int pcc20[2]={255,249};
    int *dypcc20=allocate(2,pcc20);
    pccList[20]=dypcc20;
    int pcc21[2]={255,197};
    int *dypcc21=allocate(2,pcc21);
    pccList[21]=dypcc21;
    int pcc22[2]={255,64};
    int *dypcc22=allocate(2,pcc22);
    pccList[22]=dypcc22;
    int pcc23[2]={255,223};
    int *dypcc23=allocate(2,pcc23);
    pccList[23]=dypcc23;
    *pccMap_pointer=(int***)malloc(sizeof(int**)*15);
    int*** pccMap=*pccMap_pointer;
    int** pccMap0=(int**)malloc(sizeof(int*)*0);
    pccMap[0]=pccMap0;
    int** pccMap1=(int**)malloc(sizeof(int*)*16);
    pccMap[1]=pccMap1;
    pccMap1[0]=pccList[0];
    pccMap1[1]=pccList[0];
    pccMap1[2]=pccList[0];
    pccMap1[3]=pccList[0];
    pccMap1[4]=pccList[0];
    pccMap1[5]=pccList[0];
    pccMap1[6]=pccList[0];
    pccMap1[7]=pccList[0];
    pccMap1[8]=pccList[0];
    pccMap1[9]=pccList[0];
    pccMap1[10]=pccList[0];
    pccMap1[11]=pccList[0];
    pccMap1[12]=pccList[0];
    pccMap1[13]=pccList[0];
    pccMap1[14]=pccList[0];
    pccMap1[15]=pccList[0];
    int** pccMap2=(int**)malloc(sizeof(int*)*0);
    pccMap[2]=pccMap2;
    int** pccMap3=(int**)malloc(sizeof(int*)*2);
    pccMap[3]=pccMap3;
    pccMap3[0]=pccList[0];
    pccMap3[1]=pccList[1];
    int** pccMap4=(int**)malloc(sizeof(int*)*4);
    pccMap[4]=pccMap4;
    pccMap4[0]=pccList[0];
    pccMap4[1]=pccList[1];
    pccMap4[2]=pccList[2];
    pccMap4[3]=pccList[3];
    int** pccMap5=(int**)malloc(sizeof(int*)*0);
    pccMap[5]=pccMap5;
    int** pccMap6=(int**)malloc(sizeof(int*)*16);
    pccMap[6]=pccMap6;
    pccMap6[0]=pccList[0];
    pccMap6[1]=pccList[0];
    pccMap6[2]=pccList[1];
    pccMap6[3]=pccList[1];
    pccMap6[4]=pccList[2];
    pccMap6[5]=pccList[2];
    pccMap6[6]=pccList[3];
    pccMap6[7]=pccList[3];
    pccMap6[8]=pccList[4];
    pccMap6[9]=pccList[4];
    pccMap6[10]=pccList[5];
    pccMap6[11]=pccList[5];
    pccMap6[12]=pccList[6];
    pccMap6[13]=pccList[6];
    pccMap6[14]=pccList[7];
    pccMap6[15]=pccList[7];
    int** pccMap7=(int**)malloc(sizeof(int*)*0);
    pccMap[7]=pccMap7;
    int** pccMap8=(int**)malloc(sizeof(int*)*0);
    pccMap[8]=pccMap8;
    int** pccMap9=(int**)malloc(sizeof(int*)*0);
    pccMap[9]=pccMap9;
    int** pccMap10=(int**)malloc(sizeof(int*)*0);
    pccMap[10]=pccMap10;
    int** pccMap11=(int**)malloc(sizeof(int*)*16);
    pccMap[11]=pccMap11;
    pccMap11[0]=pccList[8];
    pccMap11[1]=pccList[9];
    pccMap11[2]=pccList[10];
    pccMap11[3]=pccList[11];
    pccMap11[4]=pccList[12];
    pccMap11[5]=pccList[13];
    pccMap11[6]=pccList[14];
    pccMap11[7]=pccList[15];
    pccMap11[8]=pccList[16];
    pccMap11[9]=pccList[17];
    pccMap11[10]=pccList[18];
    pccMap11[11]=pccList[19];
    pccMap11[12]=pccList[20];
    pccMap11[13]=pccList[21];
    pccMap11[14]=pccList[22];
    pccMap11[15]=pccList[23];
    int** pccMap12=(int**)malloc(sizeof(int*)*16);
    pccMap[12]=pccMap12;
    pccMap12[0]=pccList[0];
    pccMap12[1]=pccList[0];
    pccMap12[2]=pccList[0];
    pccMap12[3]=pccList[0];
    pccMap12[4]=pccList[0];
    pccMap12[5]=pccList[0];
    pccMap12[6]=pccList[0];
    pccMap12[7]=pccList[0];
    pccMap12[8]=pccList[0];
    pccMap12[9]=pccList[0];
    pccMap12[10]=pccList[0];
    pccMap12[11]=pccList[0];
    pccMap12[12]=pccList[0];
    pccMap12[13]=pccList[0];
    pccMap12[14]=pccList[0];
    pccMap12[15]=pccList[0];
    int** pccMap13=(int**)malloc(sizeof(int*)*0);
    pccMap[13]=pccMap13;
    int** pccMap14=(int**)malloc(sizeof(int*)*0);
    pccMap[14]=pccMap14;

}
void dest_pcc(int** pccList,int*** pccMap){
    for (int i=0; i<24; i++){
      free(pccList[i]);
    }
    
    for (int i=0; i<15; i++){
      free(pccMap[i]);
    }
    
    free(pccList);
    free(pccMap);

}
void gen(int* X,int* Y,int*** pccMap,monty_str* monty){
    for (int i0=0; i0<1; i0++){
      LPerm(32,2,(X + (32 * i0)),(Y + (32 * i0)));
    }
    for (int i0=0; i0<16; i0++){
      square_monty(monty,2,(Y + (2 * i0)),(X + (2 * i0)),pccMap[1][i0]);
    }
    for (int i0=0; i0<1; i0++){
      TPerm(2,16,1,(X + (32 * i0)),(Y + (32 * i0)));
    }
    for (int i0=0; i0<2; i0++){
      itensor_monty(monty,2,8,(Y + (16 * i0)),(X + (16 * i0)),pccMap[3][i0]);
    }
    for (int i0=0; i0<4; i0++){
      itensor_monty(monty,2,4,(X + (8 * i0)),(Y + (8 * i0)),pccMap[4][i0]);
    }
    for (int i0=0; i0<8; i0++){
      LPerm(4,2,(Y + (4 * i0)),(X + (4 * i0)));
    }
    for (int i0=0; i0<16; i0++){
      square_monty(monty,2,(X + (2 * i0)),(Y + (2 * i0)),pccMap[6][i0]);
    }
    for (int i0=0; i0<1; i0++){
      TPerm(2,2,8,(Y + (32 * i0)),(X + (32 * i0)));
    }
    for (int i0=0; i0<1; i0++){
      TPerm(2,4,4,(X + (32 * i0)),(Y + (32 * i0)));
    }
    for (int i0=0; i0<8; i0++){
      TPerm(2,2,1,(Y + (4 * i0)),(X + (4 * i0)));
    }
    for (int i0=0; i0<1; i0++){
      TPerm(2,8,2,(X + (32 * i0)),(Y + (32 * i0)));
    }
    for (int i0=0; i0<16; i0++){
      diagonal_monty(monty,2,(Y + (2 * i0)),(X + (2 * i0)),pccMap[11][i0]);
    }
    for (int i0=0; i0<16; i0++){
      square_monty(monty,2,(X + (2 * i0)),(Y + (2 * i0)),pccMap[12][i0]);
    }
    for (int i0=0; i0<16; i0++){
      TPerm(2,1,1,(Y + (2 * i0)),(X + (2 * i0)));
    }
    for (int i0=0; i0<1; i0++){
      TPerm(2,16,1,(X + (32 * i0)),(Y + (32 * i0)));
    }

}
int main(int argc,char** argv){
    int* X = malloc(sizeof(int)*32);
    int* Y = malloc(sizeof(int)*32);
    
    for(int i=0; i<32; i++){
      X[i]=i;
    }
    
    int** pccList;
    int*** pccMap;
    init_pcc(&pccList,&pccMap);
    
    monty_str monty;
    monty_init(&monty,257,512,255,128,9);
    initialize_timer();
    start_timer();
    
    for(int i=0; i<32; i++){
    X[i] = toResidue(&monty,X[i]);
    }
    
    gen(X,Y,pccMap,&monty);
    
    for(int i=0; i<32; i++){
    Y[i] = fromResidue(&monty,Y[i]);
    }
    
    
    stop_timer();
    printf("Elapsed time: %f\n",elapsed_time());
    
    for(int i=0; i<32; i++){
      Y[i]=(((Y[i]+257)%257)+257)%257;
    }
    
    print_array("result",Y,32);
    
    dest_pcc(pccList,pccMap);
    
    free(X);
    free(Y);
    

}

