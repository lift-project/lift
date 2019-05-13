
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float tan_uf(float x){
    { return tan(x); }; 
}
void tan(float * v_initial_param_73_17, float * & v_user_func_75_18, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_75_18 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_16 = 0;(v_i_16 <= (-1 + v_N_0)); (++v_i_16)){
        v_user_func_75_18[v_i_16] = tan_uf(v_initial_param_73_17[v_i_16]); 
    }
}
}; 