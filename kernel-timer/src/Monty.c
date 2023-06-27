#include <stdio.h>
#include "Monty.h"

monty_str monty_init(monty_str* monty,int P,int R,int P_prime,int R_inv){
  monty->P=P;
  monty->R=R;
  monty->P_prime=P_prime;
  monty->R_inv=R_inv;
}

int REDC(monty_str* monty,int T){
  int m=((T % (monty->R)) * (monty->P_prime)) % (monty->R);
  int t=(T + m * (monty->P)) / (monty->R);
  printf("Monty: %d %d %d %d : REDC T %d m %d t %d\n",
	 monty->P,monty->R,monty->P_prime,monty->R_inv,T,m,t);
  return t;
}
   
int toResidue(monty_str* monty,int x){
  return REDC(monty,x%monty->P*((monty->R*monty->R)%monty->P));
}

int fromResidue(monty_str* monty,int x){
  return REDC(monty,x);
}

int multiply(monty_str* monty,int x,int y){
  return REDC(monty,x*y*(monty->R_inv));
}
