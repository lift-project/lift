
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
void cumsum(float * v_initial_param_294_138, float * & v_user_func_297_139, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_297_139 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_304 = 0.0f;
    for (int v_i_137 = 0;(v_i_137 <= (-1 + v_N_0)); (++v_i_137)){
        scan_acc_304 = add(scan_acc_304, v_initial_param_294_138[v_i_137]); 
        v_user_func_297_139[v_i_137] = scan_acc_304; 
    }
}
}; 