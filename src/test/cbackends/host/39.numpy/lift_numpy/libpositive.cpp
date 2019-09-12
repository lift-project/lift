
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
void positive(float * v_initial_param_7596_3058, float * & v_user_func_7598_3059, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7598_3059 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3057 = 0;(v_i_3057 <= (-1 + v_N_2763)); (++v_i_3057)){
        v_user_func_7598_3059[v_i_3057] = id(v_initial_param_7596_3058[v_i_3057]); 
    }
}
}; 