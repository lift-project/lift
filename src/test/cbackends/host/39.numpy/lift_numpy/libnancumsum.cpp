
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
void nancumsum(float * v_initial_param_366_208, float * & v_user_func_369_209, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_369_209 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_376 = 0.0f;
    for (int v_i_207 = 0;(v_i_207 <= (-1 + v_N_0)); (++v_i_207)){
        scan_acc_376 = add(scan_acc_376, v_initial_param_366_208[v_i_207]); 
        v_user_func_369_209[v_i_207] = scan_acc_376; 
    }
}
}; 