#include <stdio.h>
#include <stdlib.h>
#include "../Util.h"
#include "../timer.h"
#include "../Monty.h"
#include "../Generic.h"

void init_pcc(int*** pccList_pointer, int**** pccMap_pointer){
  int pcc0[4] = { 1,1,1,4 };
  int pcc1[4] = { 1,2,1,3 };  
    // should not be static
  

  int* dypcc0 = allocate(4,pcc0);
  int* dypcc1 = allocate(4096,pcc1);

  *pccList_pointer = (int **)malloc(2*sizeof(int*));
    int** pccList=*pccList_pointer;
    pccList[0] = dypcc0;
    pccList[1] = dypcc1;
    
    int** pccMap0 = (int**)malloc(1*sizeof(int*));
    pccMap0[0]=pccList[0];

    int** pccMap1 = (int**)malloc(2*sizeof(int*));
    pccMap1[0]=pccList[0];
    pccMap1[1]=pccList[1];
    
    *pccMap_pointer = (int***)malloc(2*sizeof(int**));
    int*** pccMap=*pccMap_pointer;
    pccMap[0]=pccMap0;
    pccMap[1]=pccMap1;
    
}

void dest_pcc(int** pccList, int*** pccMap){
  for (int i=0; i<2; i++){
    free(pccList[i]);
  }
  /* printf("pccMap at dest %p\n",(void *)pccMap); */
  /* printf("pccList at dest %p\n",(void *)pccList); */


  /* printf("pccMap0 at dest %p\n",(void *)pccMap[0]); */
  /* printf("pccMap1 at dest %p\n",(void *)pccMap[1]); */
    
  for (int i=0; i<2; i++){
    free(pccMap[i]);
  }
  free(pccList);
  free(pccMap);
}

// probably doesnt need pccList
void gen(int* X,int* Y,int*** pccMap){
  for (int i=0; i<1; i++)
    itensor(5,2,2,X,Y,pccMap[0][i]);

  for (int i=0; i<2; i++)
    square(5,2,X+2*i,Y+2*i,pccMap[1][i]);
}


int main(int argc,char** argv){
    int* X = malloc(sizeof(int)*4);
    int* Y = malloc(sizeof(int)*4);
    
    for(int i=0; i<4; i++){
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
        
    for(int i=0; i<4; i++){
      Y[i]=(((Y[i]+5)%5)+5)%5;
    }
    
    print_array("result",Y,4);

    dest_pcc(pccList,pccMap);
    
    free(X);
    free(Y);
    

}

// op count: fromList [(CBinary CAdd,8),(CBinary CSubtract,4),(CBinary CMultiply,4)]
