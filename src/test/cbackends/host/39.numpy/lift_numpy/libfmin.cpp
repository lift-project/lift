
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMIN_UF_H
#define FMIN_UF_H
; 
float fmin_uf(float x, float y){
    { return min(x,y); }; 
}

#endif
 ; 
void fmin(float * v_initial_param_950_415, float * v_initial_param_951_416, float * & v_user_func_957_418, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_957_418 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_414 = 0;(v_i_414 <= (-1 + v_N_0)); (++v_i_414)){
        v_user_func_957_418[v_i_414] = fmin_uf(v_initial_param_950_415[v_i_414], v_initial_param_951_416[v_i_414]); 
    }
}
}; 