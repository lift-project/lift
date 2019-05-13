
#include <bits/stdc++.h>

using namespace std;

    
float sin_uf(float x){
    { return sin(x); }; 
}
void execute(float * v_initial_param_56_7, float * & v_user_func_58_8, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_58_8 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_6 = 0;(v_i_6 <= (-1 + v_N_0)); (++v_i_6)){
        v_user_func_58_8[v_i_6] = sin_uf(v_initial_param_56_7[v_i_6]); 
    }
}