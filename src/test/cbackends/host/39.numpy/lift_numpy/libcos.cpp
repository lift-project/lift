
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float cos_uf(float x){
    { return cos(x); }; 
}
void cos(float * v_initial_param_64_12, float * & v_user_func_66_13, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_66_13 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_11 = 0;(v_i_11 <= (-1 + v_N_0)); (++v_i_11)){
        v_user_func_66_13[v_i_11] = cos_uf(v_initial_param_64_12[v_i_11]); 
    }
}
}; 