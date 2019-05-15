
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif
 ; 
void hypot(float * v_initial_param_121_56, float * v_initial_param_122_57, float * & v_user_func_128_59, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_128_59 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_55 = 0;(v_i_55 <= (-1 + v_N_0)); (++v_i_55)){
        v_user_func_128_59[v_i_55] = hypot_uf(v_initial_param_121_56[v_i_55], v_initial_param_122_57[v_i_55]); 
    }
}
}; 