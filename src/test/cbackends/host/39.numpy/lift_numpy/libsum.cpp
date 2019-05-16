
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
void sum(float * v_initial_param_276_132, float * & v_user_func_279_133, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_279_133 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_279_133[0] = 0.0f; 
    for (int v_i_131 = 0;(v_i_131 <= (-1 + v_N_0)); (++v_i_131)){
        v_user_func_279_133[0] = add(v_user_func_279_133[0], v_initial_param_276_132[v_i_131]); 
    }
}
}; 