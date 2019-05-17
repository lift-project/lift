
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif
 ; 
void diff(float * v_initial_param_325_169, float * & v_user_func_328_170, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_328_170 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_167 = 0;(v_i_167 <= (-2 + v_N_0)); (++v_i_167)){
        // For each element reduced sequentially
        v_user_func_328_170[v_i_167] = 0.0f; 
        for (int v_i_168 = 0;(v_i_168 <= 1); (++v_i_168)){
            v_user_func_328_170[v_i_167] = diff2(v_user_func_328_170[v_i_167], v_initial_param_325_169[(v_i_167 + v_i_168)]); 
        }
    }
}
}; 