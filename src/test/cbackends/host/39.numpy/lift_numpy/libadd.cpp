
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
void add(float * v_initial_param_576_239, float * v_initial_param_577_240, float * & v_user_func_583_242, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_583_242 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_238 = 0;(v_i_238 <= (-1 + v_N_0)); (++v_i_238)){
        v_user_func_583_242[v_i_238] = add(v_initial_param_576_239[v_i_238], v_initial_param_577_240[v_i_238]); 
    }
}
}; 