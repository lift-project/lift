
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
void hypot(float * v_initial_param_133_70, float * v_initial_param_134_71, float * & v_user_func_140_73, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_140_73 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_69 = 0;(v_i_69 <= (-1 + v_N_0)); (++v_i_69)){
        v_user_func_140_73[v_i_69] = hypot_uf(v_initial_param_133_70[v_i_69], v_initial_param_134_71[v_i_69]); 
    }
}
}; 