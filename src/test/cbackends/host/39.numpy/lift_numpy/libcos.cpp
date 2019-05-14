
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float cos_uf(float x){
    { return cos(x); }; 
}
void cos(float * v_initial_param_69_18, float * & v_user_func_71_19, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_71_19 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_17 = 0;(v_i_17 <= (-1 + v_N_0)); (++v_i_17)){
        v_user_func_71_19[v_i_17] = cos_uf(v_initial_param_69_18[v_i_17]); 
    }
}
}; 