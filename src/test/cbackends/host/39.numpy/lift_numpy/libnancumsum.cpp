
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
void nancumsum(float * v_initial_param_322_174, float * & v_user_func_325_175, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_325_175 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_332 = 0.0f;
    for (int v_i_173 = 0;(v_i_173 <= (-1 + v_N_0)); (++v_i_173)){
        scan_acc_332 = add(scan_acc_332, v_initial_param_322_174[v_i_173]); 
        v_user_func_325_175[v_i_173] = scan_acc_332; 
    }
}
}; 