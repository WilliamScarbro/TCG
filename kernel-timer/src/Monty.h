#ifndef MONTY_H
#define MONTY_H

typedef struct monty {
  int P;
  int R;
  int P_prime;
  int R_inv;
  int R_log;
} monty_str;

monty_str monty_init(monty_str*,int P, int R, int P_prime, int R_inv, int R_log);

int REDC(monty_str*,int T);

int toResidue(monty_str*,int x);

int fromResidue(monty_str*,int x);

int multiply(monty_str*,int x,int y);

#endif
