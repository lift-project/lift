
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
void add(float * v_initial_param_584_248, float * v_initial_param_585_249, float * & v_user_func_591_251, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_591_251 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_247 = 0;(v_i_247 <= (-1 + v_N_0)); (++v_i_247)){
        v_user_func_591_251[v_i_247] = add(v_initial_param_584_248[v_i_247], v_initial_param_585_249[v_i_247]); 
    }
}
}; 