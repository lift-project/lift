
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ID_H
#define ID_H
; 
float id(float x){
    { return x; }; 
}

#endif
 ; 
void positive(float * v_initial_param_624_277, float * & v_user_func_626_278, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_626_278 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_276 = 0;(v_i_276 <= (-1 + v_N_0)); (++v_i_276)){
        v_user_func_626_278[v_i_276] = id(v_initial_param_624_277[v_i_276]); 
    }
}
}; 