#ifndef GENERIC_H
#define GENERIC_H

#include "Monty.h"

void square(int,int,int*,int*,int*);

void diagonal(int,int,int*,int*,int*);

void itensor(int,int,int,int*,int*,int*);

void square_monty(monty_str*,int,int*,int*,int*);

void diagonal_monty(monty_str*,int,int*,int*,int*);

void itensor_monty(monty_str*,int,int,int*,int*,int*);


#endif

// we will need montgomery versions of these at some point

