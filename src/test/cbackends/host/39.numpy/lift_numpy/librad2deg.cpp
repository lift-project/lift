
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef R2D_UF_H
#define R2D_UF_H
; 
float r2d_uf(float x){
    { return x*180/M_PI; }; 
}

#endif
 ; 
void rad2deg(float * v_initial_param_3196_479, float * & v_user_func_3198_480, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3198_480 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_478 = 0;(v_i_478 <= (-1 + v_N_352)); (++v_i_478)){
        v_user_func_3198_480[v_i_478] = r2d_uf(v_initial_param_3196_479[v_i_478]); 
    }
}
}; 