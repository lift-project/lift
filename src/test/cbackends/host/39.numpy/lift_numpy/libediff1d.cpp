
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
void ediff1d(float * v_initial_param_310_156, float * & v_user_func_313_157, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_313_157 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_154 = 0;(v_i_154 <= (-2 + v_N_0)); (++v_i_154)){
        // For each element reduced sequentially
        v_user_func_313_157[v_i_154] = 0.0f; 
        for (int v_i_155 = 0;(v_i_155 <= 1); (++v_i_155)){
            v_user_func_313_157[v_i_154] = diff2(v_user_func_313_157[v_i_154], v_initial_param_310_156[(v_i_154 + v_i_155)]); 
        }
    }
}
}; 