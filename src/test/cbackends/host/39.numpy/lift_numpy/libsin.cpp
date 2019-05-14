
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float sin_uf(float x){
    { return sin(x); }; 
}
void sin(float * v_initial_param_61_12, float * & v_user_func_63_13, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_63_13 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_11 = 0;(v_i_11 <= (-1 + v_N_0)); (++v_i_11)){
        v_user_func_63_13[v_i_11] = sin_uf(v_initial_param_61_12[v_i_11]); 
    }
}
}; 