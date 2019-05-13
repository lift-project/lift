
#include <bits/stdc++.h>

using namespace std;


namespace lift {
    
float cos_uf(float x){
    { return cos(x); }; 
}
void cos(float * v_initial_param_56_7, float * & v_user_func_58_8, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_58_8 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_6 = 0;(v_i_6 <= (-1 + v_N_0)); (++v_i_6)){
        v_user_func_58_8[v_i_6] = cos_uf(v_initial_param_56_7[v_i_6]); 
    }
}

}
