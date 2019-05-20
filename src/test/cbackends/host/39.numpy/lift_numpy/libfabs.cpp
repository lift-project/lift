
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
void fabs(float * v_initial_param_894_394, float * & v_user_func_896_395, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_896_395 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_393 = 0;(v_i_393 <= (-1 + v_N_0)); (++v_i_393)){
        v_user_func_896_395[v_i_393] = absolute_uf(v_initial_param_894_394[v_i_393]); 
    }
}
}; 