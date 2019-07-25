
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
void sign(float * v_initial_param_3897_747, float * & v_user_func_3899_748, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3899_748 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_746 = 0;(v_i_746 <= (-1 + v_N_352)); (++v_i_746)){
        v_user_func_3899_748[v_i_746] = sign_uf(v_initial_param_3897_747[v_i_746]); 
    }
}
}; 