
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
void hypot(float * v_initial_param_3159_458, float * v_initial_param_3160_459, float * & v_user_func_3166_461, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3166_461 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_457 = 0;(v_i_457 <= (-1 + v_N_352)); (++v_i_457)){
        v_user_func_3166_461[v_i_457] = hypot_uf(v_initial_param_3159_458[v_i_457], v_initial_param_3160_459[v_i_457]); 
    }
}
}; 