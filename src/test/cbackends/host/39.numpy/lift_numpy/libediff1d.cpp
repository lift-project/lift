
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
void ediff1d(float * v_initial_param_3374_566, float * & v_user_func_3377_567, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3377_567 = reinterpret_cast<float *>(malloc(((-1 + v_N_352) * sizeof(float)))); 
    {
        
    }
    // For each element processed sequentially
    for (int v_i_564 = 0;(v_i_564 <= (-2 + v_N_352)); (++v_i_564)){
        // For each element reduced sequentially
        v_user_func_3377_567[v_i_564] = 0.0f; 
        for (int v_i_565 = 0;(v_i_565 <= 1); (++v_i_565)){
            v_user_func_3377_567[v_i_564] = diff2(v_user_func_3377_567[v_i_564], v_initial_param_3374_566[(v_i_564 + v_i_565)]); 
        }
    }
}
}; 