
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
void hypot(float * v_initial_param_147_86, float * v_initial_param_148_87, float * & v_user_func_154_89, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_154_89 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_85 = 0;(v_i_85 <= (-1 + v_N_0)); (++v_i_85)){
        v_user_func_154_89[v_i_85] = hypot_uf(v_initial_param_147_86[v_i_85], v_initial_param_148_87[v_i_85]); 
    }
}
}; 