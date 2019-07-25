
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
void nansum(float * v_initial_param_3306_546, float * & v_user_func_3309_547, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3309_547 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_3309_547[0] = 0.0f; 
    for (int v_i_545 = 0;(v_i_545 <= (-1 + v_N_352)); (++v_i_545)){
        v_user_func_3309_547[0] = add(v_user_func_3309_547[0], v_initial_param_3306_546[v_i_545]); 
    }
}
}; 