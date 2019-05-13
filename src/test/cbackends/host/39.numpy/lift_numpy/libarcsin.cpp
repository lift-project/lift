
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float arcsin_uf(float x){
    { return asin(x); }; 
}
void arcsin(float * v_initial_param_80_20, float * & v_user_func_82_21, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_82_21 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_19 = 0;(v_i_19 <= (-1 + v_N_0)); (++v_i_19)){
        v_user_func_82_21[v_i_19] = arcsin_uf(v_initial_param_80_20[v_i_19]); 
    }
}
}; 