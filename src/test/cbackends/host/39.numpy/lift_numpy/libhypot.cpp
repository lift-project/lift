
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
void hypot(float * v_initial_param_127_64, float * v_initial_param_128_65, float * & v_user_func_134_67, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_134_67 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_63 = 0;(v_i_63 <= (-1 + v_N_0)); (++v_i_63)){
        v_user_func_134_67[v_i_63] = hypot_uf(v_initial_param_127_64[v_i_63], v_initial_param_128_65[v_i_63]); 
    }
}
}; 