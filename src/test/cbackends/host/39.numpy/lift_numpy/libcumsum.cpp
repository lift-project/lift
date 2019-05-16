
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
void cumsum(float * v_initial_param_305_149, float * & v_user_func_308_150, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_308_150 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_315 = 0.0f;
    for (int v_i_148 = 0;(v_i_148 <= (-1 + v_N_0)); (++v_i_148)){
        scan_acc_315 = add(scan_acc_315, v_initial_param_305_149[v_i_148]); 
        v_user_func_308_150[v_i_148] = scan_acc_315; 
    }
}
}; 