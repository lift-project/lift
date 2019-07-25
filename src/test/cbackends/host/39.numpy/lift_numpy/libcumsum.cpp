
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
void cumsum(float * v_initial_param_3362_552, float * & v_user_func_3365_553, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3365_553 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_3372 = 0.0f;
    for (int v_i_551 = 0;(v_i_551 <= (-1 + v_N_352)); (++v_i_551)){
        scan_acc_3372 = add(scan_acc_3372, v_initial_param_3362_552[v_i_551]); 
        v_user_func_3365_553[v_i_551] = scan_acc_3372; 
    }
}
}; 