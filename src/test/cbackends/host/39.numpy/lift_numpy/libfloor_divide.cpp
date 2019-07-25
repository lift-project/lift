
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FLOOR_DIV_UF_H
#define FLOOR_DIV_UF_H
; 
float floor_div_uf(float x, float y){
    return floor(x/y);; 
}

#endif
 ; 
void floor_divide(float * v_initial_param_3722_678, float * v_initial_param_3723_679, float * & v_user_func_3729_681, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3729_681 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_677 = 0;(v_i_677 <= (-1 + v_N_352)); (++v_i_677)){
        v_user_func_3729_681[v_i_677] = floor_div_uf(v_initial_param_3722_678[v_i_677], v_initial_param_3723_679[v_i_677]); 
    }
}
}; 