
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COS_UF_H
#define COS_UF_H
; 
float cos_uf(float x){
    { return cos(x); }; 
}

#endif
 ; 
void cos(float * v_initial_param_87_44, float * & v_user_func_89_45, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_89_45 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_43 = 0;(v_i_43 <= (-1 + v_N_0)); (++v_i_43)){
        v_user_func_89_45[v_i_43] = cos_uf(v_initial_param_87_44[v_i_43]); 
    }
}
}; 