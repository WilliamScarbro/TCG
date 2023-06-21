#include <stdlib.h>
#include "../timer.h"
#include "../NTLib.h"
#include "../Util.h"
#include "../LPerm.h"
#include "../Phi.h"
#include "../Gamma.h"

void Gen(int** X,int** Y){
  int w = Nth_root(257,generator(257),128);

  //Pre-Compute Constants
  int* W1 = malloc(16*sizeof(int));
  int* W2 = malloc(16*sizeof(int));
  int* W3 = malloc(16*sizeof(int));
  int* W4 = malloc(16*sizeof(int));
  int* W5 = malloc(16*sizeof(int));
  int* W6 = malloc(16*sizeof(int));
  int* W7 = malloc(16*sizeof(int));
  int* W8 = malloc(16*sizeof(int));
  int* W9 = malloc(16*sizeof(int));
  int* W10 = malloc(16*sizeof(int));
  int* W11 = malloc(16*sizeof(int));
  int* W12 = malloc(16*sizeof(int));
  int* W13 = malloc(16*sizeof(int));
  int* W14 = malloc(16*sizeof(int));
  int* W15 = malloc(16*sizeof(int));
  int* W16 = malloc(16*sizeof(int));
  int* W0 = malloc(256*sizeof(int));
  Phi_W(w,4,128,4,257,W1);
  Phi_W(w,12,128,4,257,W2);
  Phi_W(w,20,128,4,257,W3);
  Phi_W(w,28,128,4,257,W4);
  Phi_W(w,36,128,4,257,W5);
  Phi_W(w,44,128,4,257,W6);
  Phi_W(w,52,128,4,257,W7);
  Phi_W(w,60,128,4,257,W8);
  Phi_W(w,68,128,4,257,W9);
  Phi_W(w,76,128,4,257,W10);
  Phi_W(w,84,128,4,257,W11);
  Phi_W(w,92,128,4,257,W12);
  Phi_W(w,100,128,4,257,W13);
  Phi_W(w,108,128,4,257,W14);
  Phi_W(w,116,128,4,257,W15);
  Phi_W(w,124,128,4,257,W16);
  Phi_W(w,64,128,16,257,W0);

  initialize_timer();
  start_timer();

  //Computation
  Phi(64,16,64,128,257,*X+0,*Y+0,W0);
  swap(X,Y);
  Phi(4,4,4,128,257,*X+0,*Y+0,W1);
  Phi(4,4,12,128,257,*X+4,*Y+4,W2);
  Phi(4,4,20,128,257,*X+8,*Y+8,W3);
  Phi(4,4,28,128,257,*X+12,*Y+12,W4);
  Phi(4,4,36,128,257,*X+16,*Y+16,W5);
  Phi(4,4,44,128,257,*X+20,*Y+20,W6);
  Phi(4,4,52,128,257,*X+24,*Y+24,W7);
  Phi(4,4,60,128,257,*X+28,*Y+28,W8);
  Phi(4,4,68,128,257,*X+32,*Y+32,W9);
  Phi(4,4,76,128,257,*X+36,*Y+36,W10);
  Phi(4,4,84,128,257,*X+40,*Y+40,W11);
  Phi(4,4,92,128,257,*X+44,*Y+44,W12);
  Phi(4,4,100,128,257,*X+48,*Y+48,W13);
  Phi(4,4,108,128,257,*X+52,*Y+52,W14);
  Phi(4,4,116,128,257,*X+56,*Y+56,W15);
  Phi(4,4,124,128,257,*X+60,*Y+60,W16);

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
  free(W9);
  free(W10);
  free(W11);
  free(W12);
  free(W13);
  free(W14);
  free(W15);
  free(W16);
  free(W0);
}
int main(int argc, char** argv){
  int* X = malloc(sizeof(int)*64);
  int* Y = malloc(sizeof(int)*64);

  for(int i=0; i<64; i++){
    X[i]=i;
  }
  Gen(&X,&Y);

  print_array("result",Y,64);

  free(X);
  free(Y);
}
