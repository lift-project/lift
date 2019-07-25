
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
void sum(float * v_initial_param_3306_524, float * & v_user_func_3309_525, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3309_525 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_3309_525[0] = 0.0f; 
    for (int v_i_523 = 0;(v_i_523 <= (-1 + v_N_352)); (++v_i_523)){
        v_user_func_3309_525[0] = add(v_user_func_3309_525[0], v_initial_param_3306_524[v_i_523]); 
    }
}
}; 