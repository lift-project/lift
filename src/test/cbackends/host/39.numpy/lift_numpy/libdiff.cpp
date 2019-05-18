
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
void diff(float * v_initial_param_331_175, float * & v_user_func_334_176, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_334_176 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_173 = 0;(v_i_173 <= (-2 + v_N_0)); (++v_i_173)){
        // For each element reduced sequentially
        v_user_func_334_176[v_i_173] = 0.0f; 
        for (int v_i_174 = 0;(v_i_174 <= 1); (++v_i_174)){
            v_user_func_334_176[v_i_173] = diff2(v_user_func_334_176[v_i_173], v_initial_param_331_175[(v_i_173 + v_i_174)]); 
        }
    }
}
}; 