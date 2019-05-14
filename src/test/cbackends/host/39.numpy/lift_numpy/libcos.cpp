
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float cos_uf(float x){
    { return cos(x); }; 
}
void cos(float * v_initial_param_68_16, float * & v_user_func_70_17, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_70_17 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_15 = 0;(v_i_15 <= (-1 + v_N_0)); (++v_i_15)){
        v_user_func_70_17[v_i_15] = cos_uf(v_initial_param_68_16[v_i_15]); 
    }
}
}; 