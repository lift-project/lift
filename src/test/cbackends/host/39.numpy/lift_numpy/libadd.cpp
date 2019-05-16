
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
void add(float * v_initial_param_578_242, float * v_initial_param_579_243, float * & v_user_func_585_245, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_585_245 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_241 = 0;(v_i_241 <= (-1 + v_N_0)); (++v_i_241)){
        v_user_func_585_245[v_i_241] = add(v_initial_param_578_242[v_i_241], v_initial_param_579_243[v_i_241]); 
    }
}
}; 