
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float arcsin_uf(float x){
    { return asin(x); }; 
}
void arcsin(float * v_initial_param_82_22, float * & v_user_func_84_23, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_84_23 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_21 = 0;(v_i_21 <= (-1 + v_N_0)); (++v_i_21)){
        v_user_func_84_23[v_i_21] = arcsin_uf(v_initial_param_82_22[v_i_21]); 
    }
}
}; 