
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
void sum_axis_0(float * v_initial_param_322_176, float * & v_user_func_325_177, int v_N_0, int v_M_1){
    // Allocate memory for output pointers
    v_user_func_325_177 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_174 = 0;(v_i_174 <= (-1 + v_N_0)); (++v_i_174)){
        // For each element reduced sequentially
        v_user_func_325_177[v_i_174] = 0.0f; 
        for (int v_i_175 = 0;(v_i_175 <= (-1 + v_M_1)); (++v_i_175)){
            v_user_func_325_177[v_i_174] = add(v_user_func_325_177[v_i_174], v_initial_param_322_176[(v_i_174 + (v_N_0 * v_i_175))]); 
        }
    }
}
}; 