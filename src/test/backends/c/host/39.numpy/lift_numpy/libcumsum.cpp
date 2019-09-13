
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
void cumsum(float * v_initial_param_7306_2963, float * & v_user_func_7309_2964, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7309_2964 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_7316 = 0.0f;
    for (int v_i_2962 = 0;(v_i_2962 <= (-1 + v_N_2763)); (++v_i_2962)){
        scan_acc_7316 = add(scan_acc_7316, v_initial_param_7306_2963[v_i_2962]); 
        v_user_func_7309_2964[v_i_2962] = scan_acc_7316; 
    }
}
}; 