#include <stdlib.h>
#include "../timer.h"
#include "../NTLib.h"
#include "../Util.h"
#include "../LPerm.h"
#include "../Phi.h"
#include "../Gamma.h"

void Gen(int** X,int** Y){
  int w = Nth_root(17,generator(17),16);

  //Pre-Compute Constants
  int* W1 = malloc(4*sizeof(int));
  int* W2 = malloc(4*sizeof(int));
  int* W3 = malloc(4*sizeof(int));
  int* W4 = malloc(4*sizeof(int));
  int* W5 = malloc(4*sizeof(int));
  int* W6 = malloc(4*sizeof(int));
  int* W7 = malloc(4*sizeof(int));
  int* W8 = malloc(4*sizeof(int));
  int* W0 = malloc(64*sizeof(int));
  Phi_W(w,0,16,2,17,W1);
  Phi_W(w,2,16,2,17,W2);
  Phi_W(w,4,16,2,17,W3);
  Phi_W(w,6,16,2,17,W4);
  Phi_W(w,8,16,2,17,W5);
  Phi_W(w,10,16,2,17,W6);
  Phi_W(w,12,16,2,17,W7);
  Phi_W(w,14,16,2,17,W8);
  Phi_W(w,0,16,8,17,W0);

  initialize_timer();
  start_timer();

  //Computation
  Phi(16,8,0,16,17,*X+0,*Y+0,W0);
  swap(X,Y);
  Phi(2,2,0,16,17,*X+0,*Y+0,W1);
  Phi(2,2,2,16,17,*X+2,*Y+2,W2);
  Phi(2,2,4,16,17,*X+4,*Y+4,W3);
  Phi(2,2,6,16,17,*X+6,*Y+6,W4);
  Phi(2,2,8,16,17,*X+8,*Y+8,W5);
  Phi(2,2,10,16,17,*X+10,*Y+10,W6);
  Phi(2,2,12,16,17,*X+12,*Y+12,W7);
  Phi(2,2,14,16,17,*X+14,*Y+14,W8);

  stop_timer();
  printf("Elapsed time: %f\n",elapsed_time());

  //free Pre-Computed Constants
  free(W1);
  free(W2);
  free(W3);
  free(W4);
  free(W5);
  free(W6);
  free(W7);
  free(W8);
  free(W0);
}
int main(int argc, char** argv){
  int* X = malloc(sizeof(int)*16);
  int* Y = malloc(sizeof(int)*16);

  for(int i=0; i<16; i++){
    X[i]=i;
  }
  Gen(&X,&Y);

  print_array("result",Y,16);

  free(X);
  free(Y);
}
