
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FLOOR_UF_H
#define FLOOR_UF_H
; 
float floor_uf(float x){
    return floor(x);; 
}

#endif
 ; 
void floor(float * v_initial_param_1524_300, float * & v_user_func_1526_301, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1526_301 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_299 = 0;(v_i_299 <= (-1 + v_N_190)); (++v_i_299)){
        v_user_func_1526_301[v_i_299] = floor_uf(v_initial_param_1524_300[v_i_299]); 
    }
}
}; 