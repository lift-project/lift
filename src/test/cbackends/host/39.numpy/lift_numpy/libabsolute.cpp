
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ABSOLUTE_UF_H
#define ABSOLUTE_UF_H
; 
float absolute_uf(float x){
    { return abs(x); }; 
}

#endif
 ; 
void absolute(float * v_initial_param_894_391, float * & v_user_func_896_392, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_896_392 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_390 = 0;(v_i_390 <= (-1 + v_N_0)); (++v_i_390)){
        v_user_func_896_392[v_i_390] = absolute_uf(v_initial_param_894_391[v_i_390]); 
    }
}
}; 