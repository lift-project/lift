
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
void diff(float * v_initial_param_306_148, float * & v_user_func_309_149, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_309_149 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_146 = 0;(v_i_146 <= (-2 + v_N_0)); (++v_i_146)){
        // For each element reduced sequentially
        v_user_func_309_149[v_i_146] = 0.0f; 
        for (int v_i_147 = 0;(v_i_147 <= 1); (++v_i_147)){
            v_user_func_309_149[v_i_146] = diff2(v_user_func_309_149[v_i_146], v_initial_param_306_148[(v_i_146 + v_i_147)]); 
        }
    }
}
}; 