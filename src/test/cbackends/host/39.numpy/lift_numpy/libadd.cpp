
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
void add(float * v_initial_param_580_244, float * v_initial_param_581_245, float * & v_user_func_587_247, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_587_247 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_243 = 0;(v_i_243 <= (-1 + v_N_0)); (++v_i_243)){
        v_user_func_587_247[v_i_243] = add(v_initial_param_580_244[v_i_243], v_initial_param_581_245[v_i_243]); 
    }
}
}; 