
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void add(float * v_initial_param_583_247, float * v_initial_param_584_248, float * & v_user_func_590_250, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_590_250 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_246 = 0;(v_i_246 <= (-1 + v_N_0)); (++v_i_246)){
        v_user_func_590_250[v_i_246] = add(v_initial_param_583_247[v_i_246], v_initial_param_584_248[v_i_246]); 
    }
}
}; 