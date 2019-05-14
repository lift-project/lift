
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float arctan_uf(float x){
    { return atan(x); }; 
}
void arctan(float * v_initial_param_96_28, float * & v_user_func_98_29, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_98_29 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_27 = 0;(v_i_27 <= (-1 + v_N_0)); (++v_i_27)){
        v_user_func_98_29[v_i_27] = arctan_uf(v_initial_param_96_28[v_i_27]); 
    }
}
}; 