
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif
 ; 
void ediff1d(float * v_initial_param_1593_338, float * & v_user_func_1596_339, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1596_339 = reinterpret_cast<float *>(malloc(((-1 + v_N_190) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_336 = 0;(v_i_336 <= (-2 + v_N_190)); (++v_i_336)){
        // For each element reduced sequentially
        v_user_func_1596_339[v_i_336] = 0.0f; 
        for (int v_i_337 = 0;(v_i_337 <= 1); (++v_i_337)){
            v_user_func_1596_339[v_i_336] = diff2(v_user_func_1596_339[v_i_336], v_initial_param_1593_338[(v_i_336 + v_i_337)]); 
        }
    }
}
}; 