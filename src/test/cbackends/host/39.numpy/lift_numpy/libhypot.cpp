
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
void hypot(float * v_initial_param_126_63, float * v_initial_param_127_64, float * & v_user_func_133_66, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_133_66 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_62 = 0;(v_i_62 <= (-1 + v_N_0)); (++v_i_62)){
        v_user_func_133_66[v_i_62] = hypot_uf(v_initial_param_126_63[v_i_62], v_initial_param_127_64[v_i_62]); 
    }
}
}; 