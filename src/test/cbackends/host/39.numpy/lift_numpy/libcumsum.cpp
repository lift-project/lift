
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
void cumsum(float * v_initial_param_310_156, float * & v_user_func_313_157, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_313_157 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_320 = 0.0f;
    for (int v_i_155 = 0;(v_i_155 <= (-1 + v_N_0)); (++v_i_155)){
        scan_acc_320 = add(scan_acc_320, v_initial_param_310_156[v_i_155]); 
        v_user_func_313_157[v_i_155] = scan_acc_320; 
    }
}
}; 