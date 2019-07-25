
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
void square(float * v_initial_param_3883_738, float * & v_user_func_3885_739, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3885_739 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_737 = 0;(v_i_737 <= (-1 + v_N_352)); (++v_i_737)){
        v_user_func_3885_739[v_i_737] = square_uf(v_initial_param_3883_738[v_i_737]); 
    }
}
}; 