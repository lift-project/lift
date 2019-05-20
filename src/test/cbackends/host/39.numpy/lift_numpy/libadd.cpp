
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void add(float * v_initial_param_622_283, float * v_initial_param_623_284, float * & v_user_func_629_286, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_629_286 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_282 = 0;(v_i_282 <= (-1 + v_N_0)); (++v_i_282)){
        v_user_func_629_286[v_i_282] = add(v_initial_param_622_283[v_i_282], v_initial_param_623_284[v_i_282]); 
    }
}
}; 