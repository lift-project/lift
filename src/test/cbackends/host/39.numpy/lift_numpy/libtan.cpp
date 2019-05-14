
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float tan_uf(float x){
    { return tan(x); }; 
}
void tan(float * v_initial_param_76_21, float * & v_user_func_78_22, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_78_22 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_20 = 0;(v_i_20 <= (-1 + v_N_0)); (++v_i_20)){
        v_user_func_78_22[v_i_20] = tan_uf(v_initial_param_76_21[v_i_20]); 
    }
}
}; 