
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOGADDEXP2_UF_H
#define LOGADDEXP2_UF_H
; 
float logaddexp2_uf(float x1, float x2){
    { return log2(pow(2,x1) + pow(2,x2)); }; 
}

#endif
 ; 
void logaddexp2(float * v_initial_param_3554_610, float * v_initial_param_3555_611, float * & v_user_func_3561_613, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3561_613 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_609 = 0;(v_i_609 <= (-1 + v_N_352)); (++v_i_609)){
        v_user_func_3561_613[v_i_609] = logaddexp2_uf(v_initial_param_3554_610[v_i_609], v_initial_param_3555_611[v_i_609]); 
    }
}
}; 