
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SQUARE_UF_H
#define SQUARE_UF_H
; 
float square_uf(float x){
    { return x*x; }; 
}

#endif
 ; 
void square(float * v_initial_param_7827_3149, float * & v_user_func_7829_3150, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7829_3150 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3148 = 0;(v_i_3148 <= (-1 + v_N_2763)); (++v_i_3148)){
        v_user_func_7829_3150[v_i_3148] = square_uf(v_initial_param_7827_3149[v_i_3148]); 
    }
}
}; 