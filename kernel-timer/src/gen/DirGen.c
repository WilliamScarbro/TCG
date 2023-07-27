#include <stdio.h>
#include <stdlib.h>
#include "../Util.h"
#include "../timer.h"
#include "../Monty.h"
#include "../Multiply.h"

void forward_path(int* X,int* Y,monty_str* monty){
    Y[0] = (X[0] + (REDC(monty,(54 * X[3])) + (REDC(monty,(44 * X[6])) + (REDC(monty,(61 * X[9])) + (REDC(monty,(3 * X[12])) + (REDC(monty,(24 * X[15])) + (REDC(monty,(95 * X[18])) + REDC(monty,(81 * X[21])))))))));
    Y[3] = (X[0] + (REDC(monty,(61 * X[3])) + (REDC(monty,(95 * X[6])) + (REDC(monty,(43 * X[9])) + (REDC(monty,(94 * X[12])) + (REDC(monty,(16 * X[15])) + (REDC(monty,(44 * X[18])) + REDC(monty,(24 * X[21])))))))));
    Y[6] = (X[0] + (REDC(monty,(24 * X[3])) + (REDC(monty,(53 * X[6])) + (REDC(monty,(16 * X[9])) + (REDC(monty,(3 * X[12])) + (REDC(monty,(43 * X[15])) + (REDC(monty,(2 * X[18])) + REDC(monty,(61 * X[21])))))))));
    Y[9] = (X[0] + (REDC(monty,(81 * X[3])) + (REDC(monty,(2 * X[6])) + (REDC(monty,(24 * X[9])) + (REDC(monty,(94 * X[12])) + (REDC(monty,(61 * X[15])) + (REDC(monty,(53 * X[18])) + REDC(monty,(54 * X[21])))))))));
    Y[12] = (X[0] + (REDC(monty,(43 * X[3])) + (REDC(monty,(44 * X[6])) + (REDC(monty,(36 * X[9])) + (REDC(monty,(3 * X[12])) + (REDC(monty,(73 * X[15])) + (REDC(monty,(95 * X[18])) + REDC(monty,(16 * X[21])))))))));
    Y[15] = (X[0] + (REDC(monty,(36 * X[3])) + (REDC(monty,(95 * X[6])) + (REDC(monty,(54 * X[9])) + (REDC(monty,(94 * X[12])) + (REDC(monty,(81 * X[15])) + (REDC(monty,(44 * X[18])) + REDC(monty,(73 * X[21])))))))));
    Y[18] = (X[0] + (REDC(monty,(73 * X[3])) + (REDC(monty,(53 * X[6])) + (REDC(monty,(81 * X[9])) + (REDC(monty,(3 * X[12])) + (REDC(monty,(54 * X[15])) + (REDC(monty,(2 * X[18])) + REDC(monty,(36 * X[21])))))))));
    Y[21] = (X[0] + (REDC(monty,(16 * X[3])) + (REDC(monty,(2 * X[6])) + (REDC(monty,(73 * X[9])) + (REDC(monty,(94 * X[12])) + (REDC(monty,(36 * X[15])) + (REDC(monty,(53 * X[18])) + REDC(monty,(43 * X[21])))))))));
    Y[1] = (X[1] + (REDC(monty,(54 * X[4])) + (REDC(monty,(44 * X[7])) + (REDC(monty,(61 * X[10])) + (REDC(monty,(3 * X[13])) + (REDC(monty,(24 * X[16])) + (REDC(monty,(95 * X[19])) + REDC(monty,(81 * X[22])))))))));
    Y[4] = (X[1] + (REDC(monty,(61 * X[4])) + (REDC(monty,(95 * X[7])) + (REDC(monty,(43 * X[10])) + (REDC(monty,(94 * X[13])) + (REDC(monty,(16 * X[16])) + (REDC(monty,(44 * X[19])) + REDC(monty,(24 * X[22])))))))));
    Y[7] = (X[1] + (REDC(monty,(24 * X[4])) + (REDC(monty,(53 * X[7])) + (REDC(monty,(16 * X[10])) + (REDC(monty,(3 * X[13])) + (REDC(monty,(43 * X[16])) + (REDC(monty,(2 * X[19])) + REDC(monty,(61 * X[22])))))))));
    Y[10] = (X[1] + (REDC(monty,(81 * X[4])) + (REDC(monty,(2 * X[7])) + (REDC(monty,(24 * X[10])) + (REDC(monty,(94 * X[13])) + (REDC(monty,(61 * X[16])) + (REDC(monty,(53 * X[19])) + REDC(monty,(54 * X[22])))))))));
    Y[13] = (X[1] + (REDC(monty,(43 * X[4])) + (REDC(monty,(44 * X[7])) + (REDC(monty,(36 * X[10])) + (REDC(monty,(3 * X[13])) + (REDC(monty,(73 * X[16])) + (REDC(monty,(95 * X[19])) + REDC(monty,(16 * X[22])))))))));
    Y[16] = (X[1] + (REDC(monty,(36 * X[4])) + (REDC(monty,(95 * X[7])) + (REDC(monty,(54 * X[10])) + (REDC(monty,(94 * X[13])) + (REDC(monty,(81 * X[16])) + (REDC(monty,(44 * X[19])) + REDC(monty,(73 * X[22])))))))));
    Y[19] = (X[1] + (REDC(monty,(73 * X[4])) + (REDC(monty,(53 * X[7])) + (REDC(monty,(81 * X[10])) + (REDC(monty,(3 * X[13])) + (REDC(monty,(54 * X[16])) + (REDC(monty,(2 * X[19])) + REDC(monty,(36 * X[22])))))))));
    Y[22] = (X[1] + (REDC(monty,(16 * X[4])) + (REDC(monty,(2 * X[7])) + (REDC(monty,(73 * X[10])) + (REDC(monty,(94 * X[13])) + (REDC(monty,(36 * X[16])) + (REDC(monty,(53 * X[19])) + REDC(monty,(43 * X[22])))))))));
    Y[2] = (X[2] + (REDC(monty,(54 * X[5])) + (REDC(monty,(44 * X[8])) + (REDC(monty,(61 * X[11])) + (REDC(monty,(3 * X[14])) + (REDC(monty,(24 * X[17])) + (REDC(monty,(95 * X[20])) + REDC(monty,(81 * X[23])))))))));
    Y[5] = (X[2] + (REDC(monty,(61 * X[5])) + (REDC(monty,(95 * X[8])) + (REDC(monty,(43 * X[11])) + (REDC(monty,(94 * X[14])) + (REDC(monty,(16 * X[17])) + (REDC(monty,(44 * X[20])) + REDC(monty,(24 * X[23])))))))));
    Y[8] = (X[2] + (REDC(monty,(24 * X[5])) + (REDC(monty,(53 * X[8])) + (REDC(monty,(16 * X[11])) + (REDC(monty,(3 * X[14])) + (REDC(monty,(43 * X[17])) + (REDC(monty,(2 * X[20])) + REDC(monty,(61 * X[23])))))))));
    Y[11] = (X[2] + (REDC(monty,(81 * X[5])) + (REDC(monty,(2 * X[8])) + (REDC(monty,(24 * X[11])) + (REDC(monty,(94 * X[14])) + (REDC(monty,(61 * X[17])) + (REDC(monty,(53 * X[20])) + REDC(monty,(54 * X[23])))))))));
    Y[14] = (X[2] + (REDC(monty,(43 * X[5])) + (REDC(monty,(44 * X[8])) + (REDC(monty,(36 * X[11])) + (REDC(monty,(3 * X[14])) + (REDC(monty,(73 * X[17])) + (REDC(monty,(95 * X[20])) + REDC(monty,(16 * X[23])))))))));
    Y[17] = (X[2] + (REDC(monty,(36 * X[5])) + (REDC(monty,(95 * X[8])) + (REDC(monty,(54 * X[11])) + (REDC(monty,(94 * X[14])) + (REDC(monty,(81 * X[17])) + (REDC(monty,(44 * X[20])) + REDC(monty,(73 * X[23])))))))));
    Y[20] = (X[2] + (REDC(monty,(73 * X[5])) + (REDC(monty,(53 * X[8])) + (REDC(monty,(81 * X[11])) + (REDC(monty,(3 * X[14])) + (REDC(monty,(54 * X[17])) + (REDC(monty,(2 * X[20])) + REDC(monty,(36 * X[23])))))))));
    Y[23] = (X[2] + (REDC(monty,(16 * X[5])) + (REDC(monty,(2 * X[8])) + (REDC(monty,(73 * X[11])) + (REDC(monty,(94 * X[14])) + (REDC(monty,(36 * X[17])) + (REDC(monty,(53 * X[20])) + REDC(monty,(43 * X[23])))))))));
    X[0] = Y[0];
    X[1] = REDC(monty,(96 * Y[1]));
    X[2] = REDC(monty,(72 * Y[2]));
    X[3] = Y[3];
    X[4] = REDC(monty,(54 * Y[4]));
    X[5] = REDC(monty,(44 * Y[5]));
    X[6] = Y[6];
    X[7] = REDC(monty,(91 * Y[7]));
    X[8] = REDC(monty,(70 * Y[8]));
    X[9] = Y[9];
    X[10] = REDC(monty,(33 * Y[10]));
    X[11] = REDC(monty,(32 * Y[11]));
    X[12] = Y[12];
    X[13] = REDC(monty,(61 * Y[13]));
    X[14] = REDC(monty,(95 * Y[14]));
    X[15] = Y[15];
    X[16] = REDC(monty,(4 * Y[16]));
    X[17] = REDC(monty,(85 * Y[17]));
    X[18] = Y[18];
    X[19] = REDC(monty,(75 * Y[19]));
    X[20] = REDC(monty,(25 * Y[20]));
    X[21] = Y[21];
    X[22] = REDC(monty,(24 * Y[22]));
    X[23] = REDC(monty,(53 * Y[23]));
    Y[0] = (X[0] + (X[1] + X[2]));
    Y[1] = (X[0] + (REDC(monty,(18 * X[1])) + REDC(monty,(48 * X[2]))));
    Y[2] = (X[0] + (REDC(monty,(48 * X[1])) + REDC(monty,(18 * X[2]))));
    Y[3] = (X[3] + (X[4] + X[5]));
    Y[4] = (X[3] + (REDC(monty,(18 * X[4])) + REDC(monty,(48 * X[5]))));
    Y[5] = (X[3] + (REDC(monty,(48 * X[4])) + REDC(monty,(18 * X[5]))));
    Y[6] = (X[6] + (X[7] + X[8]));
    Y[7] = (X[6] + (REDC(monty,(18 * X[7])) + REDC(monty,(48 * X[8]))));
    Y[8] = (X[6] + (REDC(monty,(48 * X[7])) + REDC(monty,(18 * X[8]))));
    Y[9] = (X[9] + (X[10] + X[11]));
    Y[10] = (X[9] + (REDC(monty,(18 * X[10])) + REDC(monty,(48 * X[11]))));
    Y[11] = (X[9] + (REDC(monty,(48 * X[10])) + REDC(monty,(18 * X[11]))));
    Y[12] = (X[12] + (X[13] + X[14]));
    Y[13] = (X[12] + (REDC(monty,(18 * X[13])) + REDC(monty,(48 * X[14]))));
    Y[14] = (X[12] + (REDC(monty,(48 * X[13])) + REDC(monty,(18 * X[14]))));
    Y[15] = (X[15] + (X[16] + X[17]));
    Y[16] = (X[15] + (REDC(monty,(18 * X[16])) + REDC(monty,(48 * X[17]))));
    Y[17] = (X[15] + (REDC(monty,(48 * X[16])) + REDC(monty,(18 * X[17]))));
    Y[18] = (X[18] + (X[19] + X[20]));
    Y[19] = (X[18] + (REDC(monty,(18 * X[19])) + REDC(monty,(48 * X[20]))));
    Y[20] = (X[18] + (REDC(monty,(48 * X[19])) + REDC(monty,(18 * X[20]))));
    Y[21] = (X[21] + (X[22] + X[23]));
    Y[22] = (X[21] + (REDC(monty,(18 * X[22])) + REDC(monty,(48 * X[23]))));
    Y[23] = (X[21] + (REDC(monty,(48 * X[22])) + REDC(monty,(18 * X[23]))));
    X[0] = Y[0];
    X[1] = Y[1];
    X[2] = Y[2];
    X[3] = Y[3];
    X[4] = Y[4];
    X[5] = Y[5];
    X[6] = Y[6];
    X[7] = Y[7];
    X[8] = Y[8];
    X[9] = Y[9];
    X[10] = Y[10];
    X[11] = Y[11];
    X[12] = Y[12];
    X[13] = Y[13];
    X[14] = Y[14];
    X[15] = Y[15];
    X[16] = Y[16];
    X[17] = Y[17];
    X[18] = Y[18];
    X[19] = Y[19];
    X[20] = Y[20];
    X[21] = Y[21];
    X[22] = Y[22];
    X[23] = Y[23];
    Y[0] = X[0];
    Y[1] = X[1];
    Y[2] = X[2];
    Y[3] = X[3];
    Y[4] = X[4];
    Y[5] = X[5];
    Y[6] = X[6];
    Y[7] = X[7];
    Y[8] = X[8];
    Y[9] = X[9];
    Y[10] = X[10];
    Y[11] = X[11];
    Y[12] = X[12];
    Y[13] = X[13];
    Y[14] = X[14];
    Y[15] = X[15];
    Y[16] = X[16];
    Y[17] = X[17];
    Y[18] = X[18];
    Y[19] = X[19];
    Y[20] = X[20];
    Y[21] = X[21];
    Y[22] = X[22];
    Y[23] = X[23];

}

void inverse_path(int* X,int* Y,monty_str* monty){
    Y[0] = X[0];
    Y[1] = X[1];
    Y[2] = X[2];
    Y[3] = X[3];
    Y[4] = X[4];
    Y[5] = X[5];
    Y[6] = X[6];
    Y[7] = X[7];
    Y[8] = X[8];
    Y[9] = X[9];
    Y[10] = X[10];
    Y[11] = X[11];
    Y[12] = X[12];
    Y[13] = X[13];
    Y[14] = X[14];
    Y[15] = X[15];
    Y[16] = X[16];
    Y[17] = X[17];
    Y[18] = X[18];
    Y[19] = X[19];
    Y[20] = X[20];
    Y[21] = X[21];
    Y[22] = X[22];
    Y[23] = X[23];
    X[0] = Y[0];
    X[1] = Y[1];
    X[2] = Y[2];
    X[3] = Y[3];
    X[4] = Y[4];
    X[5] = Y[5];
    X[6] = Y[6];
    X[7] = Y[7];
    X[8] = Y[8];
    X[9] = Y[9];
    X[10] = Y[10];
    X[11] = Y[11];
    X[12] = Y[12];
    X[13] = Y[13];
    X[14] = Y[14];
    X[15] = Y[15];
    X[16] = Y[16];
    X[17] = Y[17];
    X[18] = Y[18];
    X[19] = Y[19];
    X[20] = Y[20];
    X[21] = Y[21];
    X[22] = Y[22];
    X[23] = Y[23];
    Y[0] = X[0];
    Y[1] = X[1];
    Y[2] = X[2];
    Y[3] = X[3];
    Y[4] = X[4];
    Y[5] = X[5];
    Y[6] = X[6];
    Y[7] = X[7];
    Y[8] = X[8];
    Y[9] = X[9];
    Y[10] = X[10];
    Y[11] = X[11];
    Y[12] = X[12];
    Y[13] = X[13];
    Y[14] = X[14];
    Y[15] = X[15];
    Y[16] = X[16];
    Y[17] = X[17];
    Y[18] = X[18];
    Y[19] = X[19];
    Y[20] = X[20];
    Y[21] = X[21];
    Y[22] = X[22];
    Y[23] = X[23];
    X[0] = (REDC(monty,(75 * Y[0])) + (REDC(monty,(75 * Y[1])) + REDC(monty,(75 * Y[2]))));
    X[1] = (REDC(monty,(75 * Y[0])) + (REDC(monty,(16 * Y[1])) + REDC(monty,(6 * Y[2]))));
    X[2] = (REDC(monty,(75 * Y[0])) + (REDC(monty,(6 * Y[1])) + REDC(monty,(16 * Y[2]))));
    X[3] = (REDC(monty,(75 * Y[3])) + (REDC(monty,(75 * Y[4])) + REDC(monty,(75 * Y[5]))));
    X[4] = (REDC(monty,(75 * Y[3])) + (REDC(monty,(16 * Y[4])) + REDC(monty,(6 * Y[5]))));
    X[5] = (REDC(monty,(75 * Y[3])) + (REDC(monty,(6 * Y[4])) + REDC(monty,(16 * Y[5]))));
    X[6] = (REDC(monty,(75 * Y[6])) + (REDC(monty,(75 * Y[7])) + REDC(monty,(75 * Y[8]))));
    X[7] = (REDC(monty,(75 * Y[6])) + (REDC(monty,(16 * Y[7])) + REDC(monty,(6 * Y[8]))));
    X[8] = (REDC(monty,(75 * Y[6])) + (REDC(monty,(6 * Y[7])) + REDC(monty,(16 * Y[8]))));
    X[9] = (REDC(monty,(75 * Y[9])) + (REDC(monty,(75 * Y[10])) + REDC(monty,(75 * Y[11]))));
    X[10] = (REDC(monty,(75 * Y[9])) + (REDC(monty,(16 * Y[10])) + REDC(monty,(6 * Y[11]))));
    X[11] = (REDC(monty,(75 * Y[9])) + (REDC(monty,(6 * Y[10])) + REDC(monty,(16 * Y[11]))));
    X[12] = (REDC(monty,(75 * Y[12])) + (REDC(monty,(75 * Y[13])) + REDC(monty,(75 * Y[14]))));
    X[13] = (REDC(monty,(75 * Y[12])) + (REDC(monty,(16 * Y[13])) + REDC(monty,(6 * Y[14]))));
    X[14] = (REDC(monty,(75 * Y[12])) + (REDC(monty,(6 * Y[13])) + REDC(monty,(16 * Y[14]))));
    X[15] = (REDC(monty,(75 * Y[15])) + (REDC(monty,(75 * Y[16])) + REDC(monty,(75 * Y[17]))));
    X[16] = (REDC(monty,(75 * Y[15])) + (REDC(monty,(16 * Y[16])) + REDC(monty,(6 * Y[17]))));
    X[17] = (REDC(monty,(75 * Y[15])) + (REDC(monty,(6 * Y[16])) + REDC(monty,(16 * Y[17]))));
    X[18] = (REDC(monty,(75 * Y[18])) + (REDC(monty,(75 * Y[19])) + REDC(monty,(75 * Y[20]))));
    X[19] = (REDC(monty,(75 * Y[18])) + (REDC(monty,(16 * Y[19])) + REDC(monty,(6 * Y[20]))));
    X[20] = (REDC(monty,(75 * Y[18])) + (REDC(monty,(6 * Y[19])) + REDC(monty,(16 * Y[20]))));
    X[21] = (REDC(monty,(75 * Y[21])) + (REDC(monty,(75 * Y[22])) + REDC(monty,(75 * Y[23]))));
    X[22] = (REDC(monty,(75 * Y[21])) + (REDC(monty,(16 * Y[22])) + REDC(monty,(6 * Y[23]))));
    X[23] = (REDC(monty,(75 * Y[21])) + (REDC(monty,(6 * Y[22])) + REDC(monty,(16 * Y[23]))));
    Y[0] = X[0];
    Y[1] = REDC(monty,(9 * X[1]));
    Y[2] = REDC(monty,(12 * X[2]));
    Y[3] = X[3];
    Y[4] = REDC(monty,(16 * X[4]));
    Y[5] = REDC(monty,(2 * X[5]));
    Y[6] = X[6];
    Y[7] = REDC(monty,(50 * X[7]));
    Y[8] = REDC(monty,(65 * X[8]));
    Y[9] = X[9];
    Y[10] = REDC(monty,(35 * X[10]));
    Y[11] = REDC(monty,(27 * X[11]));
    Y[12] = X[12];
    Y[13] = REDC(monty,(73 * X[13]));
    Y[14] = REDC(monty,(53 * X[14]));
    Y[15] = X[15];
    Y[16] = REDC(monty,(22 * X[16]));
    Y[17] = REDC(monty,(25 * X[17]));
    Y[18] = X[18];
    Y[19] = REDC(monty,(93 * X[19]));
    Y[20] = REDC(monty,(85 * X[20]));
    Y[21] = X[21];
    Y[22] = REDC(monty,(36 * X[22]));
    Y[23] = REDC(monty,(95 * X[23]));
    X[0] = (REDC(monty,(16 * Y[0])) + (REDC(monty,(16 * Y[3])) + (REDC(monty,(16 * Y[6])) + (REDC(monty,(16 * Y[9])) + (REDC(monty,(16 * Y[12])) + (REDC(monty,(16 * Y[15])) + (REDC(monty,(16 * Y[18])) + REDC(monty,(16 * Y[21])))))))));
    X[3] = (REDC(monty,(2 * Y[0])) + (REDC(monty,(94 * Y[3])) + (REDC(monty,(53 * Y[6])) + ((REDC(monty,(95 * Y[12])) + (REDC(monty,(3 * Y[15])) + (REDC(monty,(44 * Y[18])) + Y[21]))) - Y[9]))));
    X[6] = (REDC(monty,(73 * Y[0])) + (REDC(monty,(43 * Y[3])) + (REDC(monty,(24 * Y[6])) + (REDC(monty,(54 * Y[9])) + (REDC(monty,(73 * Y[12])) + (REDC(monty,(43 * Y[15])) + (REDC(monty,(24 * Y[18])) + REDC(monty,(54 * Y[21])))))))));
    X[9] = (REDC(monty,(94 * Y[0])) + (REDC(monty,(95 * Y[3])) + (Y[6] + (REDC(monty,(53 * Y[9])) + (REDC(monty,(3 * Y[12])) + (REDC(monty,(2 * Y[15])) + (REDC(monty,(44 * Y[21])) - Y[18])))))));
    X[12] = (REDC(monty,(36 * Y[0])) + (REDC(monty,(61 * Y[3])) + (REDC(monty,(36 * Y[6])) + (REDC(monty,(61 * Y[9])) + (REDC(monty,(36 * Y[12])) + (REDC(monty,(61 * Y[15])) + (REDC(monty,(36 * Y[18])) + REDC(monty,(61 * Y[21])))))))));
    X[15] = (REDC(monty,(53 * Y[0])) + (Y[3] + (REDC(monty,(95 * Y[6])) + (REDC(monty,(94 * Y[9])) + (REDC(monty,(44 * Y[12])) + ((REDC(monty,(2 * Y[18])) + REDC(monty,(3 * Y[21]))) - Y[15]))))));
    X[18] = (REDC(monty,(43 * Y[0])) + (REDC(monty,(73 * Y[3])) + (REDC(monty,(54 * Y[6])) + (REDC(monty,(24 * Y[9])) + (REDC(monty,(43 * Y[12])) + (REDC(monty,(73 * Y[15])) + (REDC(monty,(54 * Y[18])) + REDC(monty,(24 * Y[21])))))))));
    X[21] = ((REDC(monty,(53 * Y[3])) + (REDC(monty,(94 * Y[6])) + (REDC(monty,(2 * Y[9])) + (Y[12] + (REDC(monty,(44 * Y[15])) + (REDC(monty,(3 * Y[18])) + REDC(monty,(95 * Y[21])))))))) - Y[0]);
    X[1] = (REDC(monty,(16 * Y[1])) + (REDC(monty,(16 * Y[4])) + (REDC(monty,(16 * Y[7])) + (REDC(monty,(16 * Y[10])) + (REDC(monty,(16 * Y[13])) + (REDC(monty,(16 * Y[16])) + (REDC(monty,(16 * Y[19])) + REDC(monty,(16 * Y[22])))))))));
    X[4] = (REDC(monty,(2 * Y[1])) + (REDC(monty,(94 * Y[4])) + (REDC(monty,(53 * Y[7])) + ((REDC(monty,(95 * Y[13])) + (REDC(monty,(3 * Y[16])) + (REDC(monty,(44 * Y[19])) + Y[22]))) - Y[10]))));
    X[7] = (REDC(monty,(73 * Y[1])) + (REDC(monty,(43 * Y[4])) + (REDC(monty,(24 * Y[7])) + (REDC(monty,(54 * Y[10])) + (REDC(monty,(73 * Y[13])) + (REDC(monty,(43 * Y[16])) + (REDC(monty,(24 * Y[19])) + REDC(monty,(54 * Y[22])))))))));
    X[10] = (REDC(monty,(94 * Y[1])) + (REDC(monty,(95 * Y[4])) + (Y[7] + (REDC(monty,(53 * Y[10])) + (REDC(monty,(3 * Y[13])) + (REDC(monty,(2 * Y[16])) + (REDC(monty,(44 * Y[22])) - Y[19])))))));
    X[13] = (REDC(monty,(36 * Y[1])) + (REDC(monty,(61 * Y[4])) + (REDC(monty,(36 * Y[7])) + (REDC(monty,(61 * Y[10])) + (REDC(monty,(36 * Y[13])) + (REDC(monty,(61 * Y[16])) + (REDC(monty,(36 * Y[19])) + REDC(monty,(61 * Y[22])))))))));
    X[16] = (REDC(monty,(53 * Y[1])) + (Y[4] + (REDC(monty,(95 * Y[7])) + (REDC(monty,(94 * Y[10])) + (REDC(monty,(44 * Y[13])) + ((REDC(monty,(2 * Y[19])) + REDC(monty,(3 * Y[22]))) - Y[16]))))));
    X[19] = (REDC(monty,(43 * Y[1])) + (REDC(monty,(73 * Y[4])) + (REDC(monty,(54 * Y[7])) + (REDC(monty,(24 * Y[10])) + (REDC(monty,(43 * Y[13])) + (REDC(monty,(73 * Y[16])) + (REDC(monty,(54 * Y[19])) + REDC(monty,(24 * Y[22])))))))));
    X[22] = ((REDC(monty,(53 * Y[4])) + (REDC(monty,(94 * Y[7])) + (REDC(monty,(2 * Y[10])) + (Y[13] + (REDC(monty,(44 * Y[16])) + (REDC(monty,(3 * Y[19])) + REDC(monty,(95 * Y[22])))))))) - Y[1]);
    X[2] = (REDC(monty,(16 * Y[2])) + (REDC(monty,(16 * Y[5])) + (REDC(monty,(16 * Y[8])) + (REDC(monty,(16 * Y[11])) + (REDC(monty,(16 * Y[14])) + (REDC(monty,(16 * Y[17])) + (REDC(monty,(16 * Y[20])) + REDC(monty,(16 * Y[23])))))))));
    X[5] = (REDC(monty,(2 * Y[2])) + (REDC(monty,(94 * Y[5])) + (REDC(monty,(53 * Y[8])) + ((REDC(monty,(95 * Y[14])) + (REDC(monty,(3 * Y[17])) + (REDC(monty,(44 * Y[20])) + Y[23]))) - Y[11]))));
    X[8] = (REDC(monty,(73 * Y[2])) + (REDC(monty,(43 * Y[5])) + (REDC(monty,(24 * Y[8])) + (REDC(monty,(54 * Y[11])) + (REDC(monty,(73 * Y[14])) + (REDC(monty,(43 * Y[17])) + (REDC(monty,(24 * Y[20])) + REDC(monty,(54 * Y[23])))))))));
    X[11] = (REDC(monty,(94 * Y[2])) + (REDC(monty,(95 * Y[5])) + (Y[8] + (REDC(monty,(53 * Y[11])) + (REDC(monty,(3 * Y[14])) + (REDC(monty,(2 * Y[17])) + (REDC(monty,(44 * Y[23])) - Y[20])))))));
    X[14] = (REDC(monty,(36 * Y[2])) + (REDC(monty,(61 * Y[5])) + (REDC(monty,(36 * Y[8])) + (REDC(monty,(61 * Y[11])) + (REDC(monty,(36 * Y[14])) + (REDC(monty,(61 * Y[17])) + (REDC(monty,(36 * Y[20])) + REDC(monty,(61 * Y[23])))))))));
    X[17] = (REDC(monty,(53 * Y[2])) + (Y[5] + (REDC(monty,(95 * Y[8])) + (REDC(monty,(94 * Y[11])) + (REDC(monty,(44 * Y[14])) + ((REDC(monty,(2 * Y[20])) + REDC(monty,(3 * Y[23]))) - Y[17]))))));
    X[20] = (REDC(monty,(43 * Y[2])) + (REDC(monty,(73 * Y[5])) + (REDC(monty,(54 * Y[8])) + (REDC(monty,(24 * Y[11])) + (REDC(monty,(43 * Y[14])) + (REDC(monty,(73 * Y[17])) + (REDC(monty,(54 * Y[20])) + REDC(monty,(24 * Y[23])))))))));
    X[23] = ((REDC(monty,(53 * Y[5])) + (REDC(monty,(94 * Y[8])) + (REDC(monty,(2 * Y[11])) + (Y[14] + (REDC(monty,(44 * Y[17])) + (REDC(monty,(3 * Y[20])) + REDC(monty,(95 * Y[23])))))))) - Y[2]);
    Y[0] = X[0];
    Y[1] = X[1];
    Y[2] = X[2];
    Y[3] = X[3];
    Y[4] = X[4];
    Y[5] = X[5];
    Y[6] = X[6];
    Y[7] = X[7];
    Y[8] = X[8];
    Y[9] = X[9];
    Y[10] = X[10];
    Y[11] = X[11];
    Y[12] = X[12];
    Y[13] = X[13];
    Y[14] = X[14];
    Y[15] = X[15];
    Y[16] = X[16];
    Y[17] = X[17];
    Y[18] = X[18];
    Y[19] = X[19];
    Y[20] = X[20];
    Y[21] = X[21];
    Y[22] = X[22];
    Y[23] = X[23];

}

void polymult(int* X,int* X_t,int* Y,int* Y_t,int* Z,int* Z_t,monty_str* monty){
  forward_path(X,X_t,monty);
  forward_path(Y,Y_t,monty);
  point_multiply_monty(24,X_t,Y_t,Z_t,monty);
  inverse_path(Z_t,Z,monty);
}

int main(int argc,char** argv){
    int* X = malloc(sizeof(int)*24);
    int* Y = malloc(sizeof(int)*24);
    int* Z = malloc(sizeof(int)*24);
    int* X_t = malloc(sizeof(int)*24);
    int* Y_t = malloc(sizeof(int)*24);
    int* Z_t = malloc(sizeof(int)*24);
    
    for(int i=0; i<24; i++){
      X[i]=i;
      Y[i]=i;
    }
    
    monty_str monty;
    monty_init(&monty,97,128,95,72,7);
    initialize_timer();
    start_timer();
    
    for(int i=0; i<24; i++){
    X[i] = toResidue(&monty,X[i]);
    Y[i] = toResidue(&monty,Y[i]);
    }
    
    polymult(X,X_t,Y,Y_t,Z,Z_t,&monty);
    
    for(int i=0; i<24; i++){
    Z[i] = fromResidue(&monty,Z[i]);
    }
    
    
    stop_timer();
    printf("Elapsed time: %f\n",elapsed_time());
    
    for(int i=0; i<24; i++){
      Z[i]=(((Z[i]+97)%97)+97)%97;
    }
    
    print_array("result",Z,24);
    
    free(X);
    free(Y);
    free(Z);
    free(X_t);
    free(Y_t);
    free(Z_t);
    

}

