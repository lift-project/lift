
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef NEGATIVE_UF_H
#define NEGATIVE_UF_H
; 
float negative_uf(float x){
    return (-1.0f)*x; 
}

#endif
 ; 
void negative(float * v_initial_param_3659_650, float * & v_user_func_3661_651, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3661_651 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_649 = 0;(v_i_649 <= (-1 + v_N_352)); (++v_i_649)){
        v_user_func_3661_651[v_i_649] = negative_uf(v_initial_param_3659_650[v_i_649]); 
    }
}
}; 