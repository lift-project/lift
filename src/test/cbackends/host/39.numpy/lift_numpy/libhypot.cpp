
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif
 ; 
void hypot(float * v_initial_param_137_75, float * v_initial_param_138_76, float * & v_user_func_144_78, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_144_78 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_74 = 0;(v_i_74 <= (-1 + v_N_0)); (++v_i_74)){
        v_user_func_144_78[v_i_74] = hypot_uf(v_initial_param_137_75[v_i_74], v_initial_param_138_76[v_i_74]); 
    }
}
}; 