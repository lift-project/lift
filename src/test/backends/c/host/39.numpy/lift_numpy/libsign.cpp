
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIGN_UF_H
#define SIGN_UF_H
; 
float sign_uf(float x){
    { return x==0? 0: ( x< 0 ? -1 : 1 ); }; 
}

#endif
 ; 
void sign(float * v_initial_param_7841_3158, float * & v_user_func_7843_3159, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7843_3159 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3157 = 0;(v_i_3157 <= (-1 + v_N_2763)); (++v_i_3157)){
        v_user_func_7843_3159[v_i_3157] = sign_uf(v_initial_param_7841_3158[v_i_3157]); 
    }
}
}; 