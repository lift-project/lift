
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float arccos_uf(float x){
    { return acos(x); }; 
}
void arccos(float * v_initial_param_90_27, float * & v_user_func_92_28, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_92_28 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_26 = 0;(v_i_26 <= (-1 + v_N_0)); (++v_i_26)){
        v_user_func_92_28[v_i_26] = arccos_uf(v_initial_param_90_27[v_i_26]); 
    }
}
}; 