
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef POWER_UF_H
#define POWER_UF_H
; 
float power_uf(float x, float y){
    return pow(x, y);; 
}

#endif
 ; 
void power(float * v_initial_param_685_307, float * v_initial_param_686_308, float * & v_user_func_692_310, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_692_310 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_306 = 0;(v_i_306 <= (-1 + v_N_0)); (++v_i_306)){
        v_user_func_692_310[v_i_306] = power_uf(v_initial_param_685_307[v_i_306], v_initial_param_686_308[v_i_306]); 
    }
}
}; 