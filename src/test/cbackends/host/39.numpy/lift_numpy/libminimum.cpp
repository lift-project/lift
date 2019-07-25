
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MINIMUM_UF_H
#define MINIMUM_UF_H
; 
float minimum_uf(float x, float y){
    { return min(x,y); }; 
}

#endif
 ; 
void minimum(float * v_initial_param_3918_755, float * v_initial_param_3919_756, float * & v_user_func_3925_758, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3925_758 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_754 = 0;(v_i_754 <= (-1 + v_N_352)); (++v_i_754)){
        v_user_func_3925_758[v_i_754] = minimum_uf(v_initial_param_3918_755[v_i_754], v_initial_param_3919_756[v_i_754]); 
    }
}
}; 