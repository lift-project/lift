
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    


#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
float arctan_uf(float x){
    { return atan(x); }; 
}
#endif
void arctan(float * v_initial_param_97_30, float * & v_user_func_99_31, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_99_31 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_29 = 0;(v_i_29 <= (-1 + v_N_0)); (++v_i_29)){
        v_user_func_99_31[v_i_29] = arctan_uf(v_initial_param_97_30[v_i_29]); 
    }
}
}; 
