
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
void hypot(float * v_initial_param_132_69, float * v_initial_param_133_70, float * & v_user_func_139_72, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_139_72 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_68 = 0;(v_i_68 <= (-1 + v_N_0)); (++v_i_68)){
        v_user_func_139_72[v_i_68] = hypot_uf(v_initial_param_132_69[v_i_68], v_initial_param_133_70[v_i_68]); 
    }
}
}; 