
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdint.h>
#include <string.h>
#include <bits/stdc++.h>

using namespace std;

#include "util.hpp"

#define VEC_START_ADDR SHARED_SPACE_START_ADDR

void* trans_alloc(unsigned int size){
 static unsigned int current_pos = 0;
 void *memory_allocated = reinterpret_cast<void*>(VEC_START_ADDR + current_pos);
 current_pos += size;
 return memory_allocated;
 }

 char *syncSpmStartAddr = (char *)SYNC_SPM_START_ADDR;
 void barrier_wait(unsigned n) {
 // Create pointers to a global cv, mutex and a barrier.
 pthread_cond_t *condPtr = (pthread_cond_t *)(syncSpmStartAddr);
 pthread_barrier_t *barrierPtr = (pthread_barrier_t *)(condPtr + 1);
 pthread_mutex_t *mutexPtr = (pthread_mutex_t *)(barrierPtr + 1);
 // Start is used by LCP[0] with the condition variable to signal other cores to "go".
 bool *start = (bool *)(mutexPtr + 1);
 // TODO_SDH_10_31_18: creating a new barrier object for every instance: not a very clean/scalable approach.
 syncSpmStartAddr += sizeof(pthread_cond_t) + sizeof(pthread_barrier_t) + sizeof(pthread_mutex_t) + sizeof(uint32_t);
 if(LCP_TILE_ID() == 0) {
 // Initialize the barrier with the number of participants.
 pthread_barrier_init(barrierPtr, nullptr, n);
 // Signal "go" and broadcast to all cores waiting.
 STORE_BYTE(start, 1);
 // LCP_PRINTF("--> Signaling all cores to start -->\n");
 pthread_cond_broadcast(condPtr);
 } else {
 // Need to grab a lock before sleeping with a cv.
 pthread_mutex_lock(mutexPtr);
 while(*start == 0) {
 // Release the lock and sleep until signaled.
 pthread_cond_wait(condPtr, mutexPtr);
 }
 // Unlock and wait on barrier until GPEs are done.
 pthread_mutex_unlock(mutexPtr);
 }

 pthread_barrier_wait(barrierPtr);
 }

    ; 
void execute(float * v_initial_param_1_14, float * v_initial_param_2_15, float * & v_user_func_53_18, int v_K_3, int v_M_2, int v_N_1){
    // Allocate memory for output pointers
    v_user_func_53_18 = reinterpret_cast<float *>(trans_alloc(((v_M_2 * v_N_1) * sizeof(float)))); 
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_107 = 0;(gpe_loop_cvar_107 < 4); (++gpe_loop_cvar_107)){
        GPEQ_PUSH(gpe_loop_cvar_107, reinterpret_cast<uint32_t>(v_initial_param_1_14)); 
        GPEQ_PUSH(gpe_loop_cvar_107, reinterpret_cast<uint32_t>(v_initial_param_2_15)); 
        GPEQ_PUSH(gpe_loop_cvar_107, reinterpret_cast<uint32_t>(v_user_func_53_18)); 
        GPEQ_PUSH(gpe_loop_cvar_107, v_K_3); 
        GPEQ_PUSH(gpe_loop_cvar_107, v_M_2); 
        GPEQ_PUSH(gpe_loop_cvar_107, v_N_1); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    // For each transmuter chip
    for (int v_i_10 = 0;(v_i_10 <= (-1 + ((v_M_2)/(2)))); (++v_i_10)){
        {
            // For each GPE
            for (int v_i_12 = 0;(v_i_12 <= (-1 + v_N_1)); (++v_i_12)){
                GPEQ_PUSH(v_i_12, v_i_12); 
                {
                    
                }
                {
                    
                }
            }
            // Sync all GPEs
            for (int i_74 = 0;(i_74 < 4); (++i_74)){
                LCPQ_POP(i_74); 
            }
        }
        {
            
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}
