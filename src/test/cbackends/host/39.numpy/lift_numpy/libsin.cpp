
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float sin_uf(float x){
    { return sin(x); }; 
}
void sin(float * v_initial_param_57_8, float * & v_user_func_59_9, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_59_9 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_7 = 0;(v_i_7 <= (-1 + v_N_0)); (++v_i_7)){
        v_user_func_59_9[v_i_7] = sin_uf(v_initial_param_57_8[v_i_7]); 
    }
}
}; 