
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
void hypot(float * v_initial_param_135_72, float * v_initial_param_136_73, float * & v_user_func_142_75, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_142_75 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_71 = 0;(v_i_71 <= (-1 + v_N_0)); (++v_i_71)){
        v_user_func_142_75[v_i_71] = hypot_uf(v_initial_param_135_72[v_i_71], v_initial_param_136_73[v_i_71]); 
    }
}
}; 