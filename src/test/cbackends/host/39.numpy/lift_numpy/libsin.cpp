
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float sin_uf(float x){
    { return sin(x); }; 
}
void sin(float * v_initial_param_62_14, float * & v_user_func_64_15, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_64_15 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_13 = 0;(v_i_13 <= (-1 + v_N_0)); (++v_i_13)){
        v_user_func_64_15[v_i_13] = sin_uf(v_initial_param_62_14[v_i_13]); 
    }
}
}; 