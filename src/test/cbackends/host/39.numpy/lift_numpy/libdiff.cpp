
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
void diff(float * v_initial_param_320_163, float * & v_user_func_323_164, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_323_164 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_161 = 0;(v_i_161 <= (-2 + v_N_0)); (++v_i_161)){
        // For each element reduced sequentially
        v_user_func_323_164[v_i_161] = 0.0f; 
        for (int v_i_162 = 0;(v_i_162 <= 1); (++v_i_162)){
            v_user_func_323_164[v_i_161] = diff2(v_user_func_323_164[v_i_161], v_initial_param_320_163[(v_i_161 + v_i_162)]); 
        }
    }
}
}; 