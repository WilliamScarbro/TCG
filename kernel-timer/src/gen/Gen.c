#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include "../Util.h"
#include "../timer.h"
#include "../Generic.h"
#include "../LPerm.h"

void init_pcc(int*** pccList_pointer, int**** pccMap_pointer){
    *pccList_pointer=(int**)malloc(sizeof(int*)*6);
    int** pccList=*pccList_pointer;
    int pcc0[4]={1,1,1,16};
    int *dypcc0=allocate(4,pcc0);
    pccList[0]=dypcc0;
    int pcc1[16]={1,1,1,13,1,1,1,13,1,1,1,13,1,1,1,13};
    int *dypcc1=allocate(16,pcc1);
    pccList[1]=dypcc1;
    int pcc2[4]={1,13,1,4};
    int *dypcc2=allocate(4,pcc2);
    pccList[2]=dypcc2;
    int pcc3[4]={1,9,1,8};
    int *dypcc3=allocate(4,pcc3);
    pccList[3]=dypcc3;
    int pcc4[4]={1,15,1,2};
    int *dypcc4=allocate(4,pcc4);
    pccList[4]=dypcc4;
    int pcc5[16]={1,1,1,9,1,3,1,10,1,13,1,15,1,5,1,11};
    int *dypcc5=allocate(16,pcc5);
    pccList[5]=dypcc5;
    *pccMap_pointer=(int***)malloc(sizeof(int**)*13);
    int*** pccMap=*pccMap_pointer;
    int** pccMap0=(int**)malloc(sizeof(int*)*0);
    pccMap[0]=pccMap0;
    int** pccMap1=(int**)malloc(sizeof(int*)*4);
    pccMap[1]=pccMap1;
    pccMap1[0]=pccList[0];
    pccMap1[1]=pccList[0];
    pccMap1[2]=pccList[0];
    pccMap1[3]=pccList[0];
    int** pccMap2=(int**)malloc(sizeof(int*)*1);
    pccMap[2]=pccMap2;
    pccMap2[0]=pccList[1];
    int** pccMap3=(int**)malloc(sizeof(int*)*8);
    pccMap[3]=pccMap3;
    pccMap3[0]=pccList[0];
    pccMap3[1]=pccList[0];
    pccMap3[2]=pccList[0];
    pccMap3[3]=pccList[0];
    pccMap3[4]=pccList[0];
    pccMap3[5]=pccList[0];
    pccMap3[6]=pccList[0];
    pccMap3[7]=pccList[0];
    int** pccMap4=(int**)malloc(sizeof(int*)*0);
    pccMap[4]=pccMap4;
    int** pccMap5=(int**)malloc(sizeof(int*)*0);
    pccMap[5]=pccMap5;
    int** pccMap6=(int**)malloc(sizeof(int*)*0);
    pccMap[6]=pccMap6;
    int** pccMap7=(int**)malloc(sizeof(int*)*8);
    pccMap[7]=pccMap7;
    pccMap7[0]=pccList[0];
    pccMap7[1]=pccList[0];
    pccMap7[2]=pccList[2];
    pccMap7[3]=pccList[2];
    pccMap7[4]=pccList[3];
    pccMap7[5]=pccList[3];
    pccMap7[6]=pccList[4];
    pccMap7[7]=pccList[4];
    int** pccMap8=(int**)malloc(sizeof(int*)*0);
    pccMap[8]=pccMap8;
    int** pccMap9=(int**)malloc(sizeof(int*)*0);
    pccMap[9]=pccMap9;
    int** pccMap10=(int**)malloc(sizeof(int*)*1);
    pccMap[10]=pccMap10;
    pccMap10[0]=pccList[5];
    int** pccMap11=(int**)malloc(sizeof(int*)*8);
    pccMap[11]=pccMap11;
    pccMap11[0]=pccList[0];
    pccMap11[1]=pccList[0];
    pccMap11[2]=pccList[0];
    pccMap11[3]=pccList[0];
    pccMap11[4]=pccList[0];
    pccMap11[5]=pccList[0];
    pccMap11[6]=pccList[0];
    pccMap11[7]=pccList[0];
    int** pccMap12=(int**)malloc(sizeof(int*)*0);
    pccMap[12]=pccMap12;

}
void dest_pcc(int** pccList,int*** pccMap){
    for (int i=0; i<6; i++){
      free(pccList[i]);
    }
    
    for (int i=0; i<13; i++){
      free(pccMap[i]);
    }
    
    free(pccList);
    free(pccMap);

}
void gen(int* X,int* Y,int*** pccMap){
    for (int i0=0; i0<1; i0++){
      LPerm(16,4,(X + (16 * i0)),(Y + (16 * i0)));
    }
    for (int i0=0; i0<4; i0++){
      itensor(17,2,2,(Y + (4 * i0)),(X + (4 * i0)),pccMap[1][i0]);
    }
    for (int i0=0; i0<1; i0++){
      diagonal(17,16,(X + (16 * i0)),(Y + (16 * i0)),pccMap[2][i0]);
    }
    for (int i0=0; i0<8; i0++){
      square(17,2,(Y + (2 * i0)),(X + (2 * i0)),pccMap[3][i0]);
    }
    for (int i0=0; i0<8; i0++){
      TPerm(2,1,1,(X + (2 * i0)),(Y + (2 * i0)));
    }
    for (int i0=0; i0<1; i0++){
      TPerm(4,4,1,(Y + (16 * i0)),(X + (16 * i0)));
    }
    for (int i0=0; i0<4; i0++){
      LPerm(4,2,(X + (4 * i0)),(Y + (4 * i0)));
    }
    for (int i0=0; i0<8; i0++){
      square(17,2,(Y + (2 * i0)),(X + (2 * i0)),pccMap[7][i0]);
    }
    for (int i0=0; i0<4; i0++){
      TPerm(2,2,1,(X + (4 * i0)),(Y + (4 * i0)));
    }
    for (int i0=0; i0<1; i0++){
      TPerm(2,4,2,(Y + (16 * i0)),(X + (16 * i0)));
    }
    for (int i0=0; i0<1; i0++){
      diagonal(17,16,(X + (16 * i0)),(Y + (16 * i0)),pccMap[10][i0]);
    }
    for (int i0=0; i0<8; i0++){
      square(17,2,(Y + (2 * i0)),(X + (2 * i0)),pccMap[11][i0]);
    }
    for (int i0=0; i0<8; i0++){
      TPerm(2,1,1,(X + (2 * i0)),(Y + (2 * i0)));
    }

}
int main(int argc,char** argv){
    int* X = malloc(sizeof(int)*16);
    int* Y = malloc(sizeof(int)*16);
    
    for(int i=0; i<16; i++){
      X[i]=i;
    }
    
    int** pccList;
    int*** pccMap;
    init_pcc(&pccList,&pccMap);
    
    initialize_timer();
    start_timer();
    
    gen(X,Y,pccMap);
    
    
    stop_timer();
    printf("Elapsed time: %f\n",elapsed_time());
    
    for(int i=0; i<16; i++){
      Y[i]=(((Y[i]+17)%17)+17)%17;
    }
    
    print_array("result",Y,16);
    
    dest_pcc(pccList,pccMap);
    
    free(X);
    free(Y);
    

}

