
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
void nancumsum(float * v_initial_param_316_168, float * & v_user_func_319_169, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_319_169 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_326 = 0.0f;
    for (int v_i_167 = 0;(v_i_167 <= (-1 + v_N_0)); (++v_i_167)){
        scan_acc_326 = add(scan_acc_326, v_initial_param_316_168[v_i_167]); 
        v_user_func_319_169[v_i_167] = scan_acc_326; 
    }
}
}; 