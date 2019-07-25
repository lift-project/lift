
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef REAL_UF_H
#define REAL_UF_H
; 
float real_uf(float x, float y){
    { return x; }; 
}

#endif
 ; 
void real(Tuple2_float_float * v_initial_param_3807_717, float * & v_user_func_3813_718, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3813_718 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_716 = 0;(v_i_716 <= (-1 + v_N_352)); (++v_i_716)){
        v_user_func_3813_718[v_i_716] = real_uf(v_initial_param_3807_717[v_i_716]._0, v_initial_param_3807_717[v_i_716]._1); 
    }
}
}; 