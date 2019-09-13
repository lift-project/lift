
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
void nancumsum(float * v_initial_param_7306_2969, float * & v_user_func_7309_2970, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7309_2970 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_7316 = 0.0f;
    for (int v_i_2968 = 0;(v_i_2968 <= (-1 + v_N_2763)); (++v_i_2968)){
        scan_acc_7316 = add(scan_acc_7316, v_initial_param_7306_2969[v_i_2968]); 
        v_user_func_7309_2970[v_i_2968] = scan_acc_7316; 
    }
}
}; 