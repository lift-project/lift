
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
void execute(float * v_initial_param_1_12, float * v_initial_param_2_13, float * & v_user_func_46_78, int v_M_2, int v_K_3, int v_N_1){
    // Allocate memory for output pointers
    v_user_func_46_78 = reinterpret_cast<float *>(trans_alloc(((v_N_1 * v_M_2) * sizeof(float)))); 
    // Push all pointers and sizes to GPEs
    for (int gpe_loop_cvar_78 = 0;(gpe_loop_cvar_78 < 4); (++gpe_loop_cvar_78)){
        GPEQ_PUSH(gpe_loop_cvar_78, reinterpret_cast<uint32_t>(v_initial_param_1_12)); 
        GPEQ_PUSH(gpe_loop_cvar_78, reinterpret_cast<uint32_t>(v_initial_param_2_13)); 
        GPEQ_PUSH(gpe_loop_cvar_78, reinterpret_cast<uint32_t>(v_user_func_46_78)); 
        GPEQ_PUSH(gpe_loop_cvar_78, v_M_2); 
        GPEQ_PUSH(gpe_loop_cvar_78, v_K_3); 
        GPEQ_PUSH(gpe_loop_cvar_78, v_N_1); 
    }
    // ToGPE
    CACHE_FLUSH(); 
    for (int v_tile_batch_79 = 0;(v_tile_batch_79 <= (v_M_2 / 2)); (++v_tile_batch_79)){
        int v_virtual_tile_id_80 = (LCP_TILE_ID() + (v_tile_batch_79 * 2));
        if ((v_virtual_tile_id_80 < v_M_2))for (int v_gpe_batch_81 = 0;(v_gpe_batch_81 <= (v_N_1 / 4)); (++v_gpe_batch_81)){
            for (int v_gpe_82 = 0;(v_gpe_82 < 4); (++v_gpe_82)){
                GPEQ_PUSH(v_gpe_82, (v_gpe_82 + (4 * v_gpe_batch_81))); 
            }
            {
                
            }
            for (int v_gpe_83 = 0;(v_gpe_83 < 4); (++v_gpe_83)){
                
                __asm__ __volatile__ (
                "dmb\n\t"
                ); 
                LCPQ_POP(v_gpe_83); 
            }
        }
    }
    // ToLCP
    CACHE_FLUSH(); 
}