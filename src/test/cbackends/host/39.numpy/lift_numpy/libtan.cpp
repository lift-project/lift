
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float tan_uf(float x){
    { return tan(x); }; 
}
void tan(float * v_initial_param_75_19, float * & v_user_func_77_20, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_77_20 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_18 = 0;(v_i_18 <= (-1 + v_N_0)); (++v_i_18)){
        v_user_func_77_20[v_i_18] = tan_uf(v_initial_param_75_19[v_i_18]); 
    }
}
}; 