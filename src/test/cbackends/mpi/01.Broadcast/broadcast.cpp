
#include <bits/stdc++.h>
#include <mpi.h>
using namespace std;

void execute(float * v_initial_param_40_2, float * & v_initial_param_40_2, int v_N_0){
    {
        MPI_Bcast(v_initial_param_40_2, v_N_0, MPI_Float, 0, MPI_COMM_WORLD); 
    }
}