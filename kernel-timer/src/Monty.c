#include <stdio.h>
#include "Monty.h"

monty_str monty_init(monty_str* monty,int P,int R,int P_prime,int R_inv,int R_log){
  monty->P=P;
  monty->R=R;
  monty->P_prime=P_prime;
  monty->R_inv=R_inv;
  monty->R_log=R_log;
}

int fast_mod(int x,int log_m){
  return x-(1<<log_m)*(x>>log_m);
}

int fast_div(int x,int log_m){
  return x>>log_m;
}

int REDC(monty_str* monty, int T){
  int m=fast_mod(fast_mod(T,monty->R_log)*(monty->P_prime),monty->R_log);
  int t=fast_div(T+m*(monty->P),monty->R_log);
  return t;
}

int REDC2(monty_str* monty,int T){
  int m=((T % (monty->R)) * (monty->P_prime)) % (monty->R);
  int t=(T + m * (monty->P)) / (monty->R);
  //printf("Monty: %d %d %d %d : REDC T %d m %d t %d\n",
  //	 monty->P,monty->R,monty->P_prime,monty->R_inv,T,m,t);
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
