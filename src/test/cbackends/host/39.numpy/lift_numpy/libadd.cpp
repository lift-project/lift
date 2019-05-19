
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
void add(float * v_initial_param_603_269, float * v_initial_param_604_270, float * & v_user_func_610_272, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_610_272 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_268 = 0;(v_i_268 <= (-1 + v_N_0)); (++v_i_268)){
        v_user_func_610_272[v_i_268] = add(v_initial_param_603_269[v_i_268], v_initial_param_604_270[v_i_268]); 
    }
}
}; 