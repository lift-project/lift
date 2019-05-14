
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float arcsin_uf(float x){
    { return asin(x); }; 
}
void arcsin(float * v_initial_param_83_24, float * & v_user_func_85_25, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_85_25 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_23 = 0;(v_i_23 <= (-1 + v_N_0)); (++v_i_23)){
        v_user_func_85_25[v_i_23] = arcsin_uf(v_initial_param_83_24[v_i_23]); 
    }
}
}; 