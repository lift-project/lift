
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
void fabs(float * v_initial_param_881_388, float * & v_user_func_883_389, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_883_389 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_387 = 0;(v_i_387 <= (-1 + v_N_0)); (++v_i_387)){
        v_user_func_883_389[v_i_387] = absolute_uf(v_initial_param_881_388[v_i_387]); 
    }
}
}; 