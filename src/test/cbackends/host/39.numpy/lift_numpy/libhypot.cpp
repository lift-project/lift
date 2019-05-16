
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
void hypot(float * v_initial_param_136_73, float * v_initial_param_137_74, float * & v_user_func_143_76, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_143_76 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_72 = 0;(v_i_72 <= (-1 + v_N_0)); (++v_i_72)){
        v_user_func_143_76[v_i_72] = hypot_uf(v_initial_param_136_73[v_i_72], v_initial_param_137_74[v_i_72]); 
    }
}
}; 