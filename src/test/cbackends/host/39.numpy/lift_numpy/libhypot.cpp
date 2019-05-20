
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif
 ; 
void hypot(float * v_initial_param_163_108, float * v_initial_param_164_109, float * & v_user_func_170_111, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_170_111 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_107 = 0;(v_i_107 <= (-1 + v_N_0)); (++v_i_107)){
        v_user_func_170_111[v_i_107] = hypot_uf(v_initial_param_163_108[v_i_107], v_initial_param_164_109[v_i_107]); 
    }
}
}; 