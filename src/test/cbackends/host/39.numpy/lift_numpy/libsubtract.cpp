
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SUBTRACT_H
#define SUBTRACT_H
; 
float subtract(float l, float r){
    { return l - r; }; 
}

#endif
 ; 
void subtract(float * v_initial_param_659_275, float * v_initial_param_660_276, float * & v_user_func_666_278, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_666_278 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_274 = 0;(v_i_274 <= (-1 + v_N_0)); (++v_i_274)){
        v_user_func_666_278[v_i_274] = subtract(v_initial_param_659_275[v_i_274], v_initial_param_660_276[v_i_274]); 
    }
}
}; 