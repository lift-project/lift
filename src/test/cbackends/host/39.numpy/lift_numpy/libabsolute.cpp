
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ABSOLUTE_UF_H
#define ABSOLUTE_UF_H
; 
float absolute_uf(float x){
    { return abs(x); }; 
}

#endif
 ; 
void absolute(float * v_initial_param_3890_741, float * & v_user_func_3892_742, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3892_742 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_740 = 0;(v_i_740 <= (-1 + v_N_352)); (++v_i_740)){
        v_user_func_3892_742[v_i_740] = absolute_uf(v_initial_param_3890_741[v_i_740]); 
    }
}
}; 