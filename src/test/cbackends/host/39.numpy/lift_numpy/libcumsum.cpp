
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
void cumsum(float * v_initial_param_303_147, float * & v_user_func_306_148, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_306_148 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_313 = 0.0f;
    for (int v_i_146 = 0;(v_i_146 <= (-1 + v_N_0)); (++v_i_146)){
        scan_acc_313 = add(scan_acc_313, v_initial_param_303_147[v_i_146]); 
        v_user_func_306_148[v_i_146] = scan_acc_313; 
    }
}
}; 