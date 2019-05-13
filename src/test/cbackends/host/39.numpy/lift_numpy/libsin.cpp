
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float sin_uf(float x){
    { return sin(x); }; 
}
void sin(float * v_initial_param_59_10, float * & v_user_func_61_11, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_61_11 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_9 = 0;(v_i_9 <= (-1 + v_N_0)); (++v_i_9)){
        v_user_func_61_11[v_i_9] = sin_uf(v_initial_param_59_10[v_i_9]); 
    }
}
}; 