
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
void nancumsum(float * v_initial_param_310_162, float * & v_user_func_313_163, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_313_163 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_320 = 0.0f;
    for (int v_i_161 = 0;(v_i_161 <= (-1 + v_N_0)); (++v_i_161)){
        scan_acc_320 = add(scan_acc_320, v_initial_param_310_162[v_i_161]); 
        v_user_func_313_163[v_i_161] = scan_acc_320; 
    }
}
}; 