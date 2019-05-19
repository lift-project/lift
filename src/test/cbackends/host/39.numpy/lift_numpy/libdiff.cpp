
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
void diff(float * v_initial_param_346_190, float * & v_user_func_349_191, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_349_191 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_188 = 0;(v_i_188 <= (-2 + v_N_0)); (++v_i_188)){
        // For each element reduced sequentially
        v_user_func_349_191[v_i_188] = 0.0f; 
        for (int v_i_189 = 0;(v_i_189 <= 1); (++v_i_189)){
            v_user_func_349_191[v_i_188] = diff2(v_user_func_349_191[v_i_188], v_initial_param_346_190[(v_i_188 + v_i_189)]); 
        }
    }
}
}; 