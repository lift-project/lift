
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
void cumsum(float * v_initial_param_299_143, float * & v_user_func_302_144, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_302_144 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_309 = 0.0f;
    for (int v_i_142 = 0;(v_i_142 <= (-1 + v_N_0)); (++v_i_142)){
        scan_acc_309 = add(scan_acc_309, v_initial_param_299_143[v_i_142]); 
        v_user_func_302_144[v_i_142] = scan_acc_309; 
    }
}
}; 