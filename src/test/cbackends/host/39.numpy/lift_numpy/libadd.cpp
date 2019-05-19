
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
void add(float * v_initial_param_603_267, float * v_initial_param_604_268, float * & v_user_func_610_270, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_610_270 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_266 = 0;(v_i_266 <= (-1 + v_N_0)); (++v_i_266)){
        v_user_func_610_270[v_i_266] = add(v_initial_param_603_267[v_i_266], v_initial_param_604_268[v_i_266]); 
    }
}
}; 