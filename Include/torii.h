
#ifndef TORII_H
#define TORII_H

#include <stddef.h>
#include "stc/cvec.h"

typedef struct {
    int pid;
    void (*run)(void*);
    void* data;
} ToriiProcess;

typedef struct {
    cvec_ToriiProcess queue;
    size_t active_idx;
} ToriiQueue;

void torii_queue_init(ToriiQueue* queue);
void torii_queue_push(ToriiQueue* queue, ToriiProcess* process);
size_t torii_queue_next(ToriiQueue* queue);

#endif