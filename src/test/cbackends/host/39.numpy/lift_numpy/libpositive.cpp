
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ID_H
#define ID_H
; 
float id(float x){
    { return x; }; 
}

#endif
 ; 
void positive(float * v_initial_param_613_264, float * & v_user_func_615_265, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_615_265 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_263 = 0;(v_i_263 <= (-1 + v_N_0)); (++v_i_263)){
        v_user_func_615_265[v_i_263] = id(v_initial_param_613_264[v_i_263]); 
    }
}
}; 