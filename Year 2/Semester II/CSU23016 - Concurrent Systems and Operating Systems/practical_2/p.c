
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "cond.c"


int pnum;  // number updated when producer runs.
int csum;  // sum computed using pnum when consumer runs.

int (*pred)(int); // predicate indicating if pnum is to be consumed

struct buffer_t {
    int occupied;
    pthread_mutex_t mutex;
    pthread_cond_t more;
    pthread_cond_t less;
};

struct buffer_t* buffer;

int produceT() {
    pthread_mutex_lock(&buffer->mutex); // lock mutex

    while (buffer->occupied >= 1) {
        pthread_cond_wait(&buffer->less, &buffer->mutex); // while the buffer is still occupied, wait
    }

    assert(buffer->occupied == 0); // assert that the buffer is not occupied

    scanf("%d", &pnum); // scan in a nubmer
    buffer->occupied++; // set buffer as occupied

    pthread_cond_signal(&buffer->more); // tell the consumer that the buffer is occupied 

    pthread_mutex_unlock(&buffer->mutex); // unlock mutex

    return pnum;
}

void* Produce(void* a) {
    int p;

    p = 1;
    while (p) {
        printf("@P-READY\n");
        p = produceT();
        printf("@PRODUCED %d\n", p);
    }
    printf("@P-EXIT\n");
    pthread_exit(NULL);
}


int consumeT() {
    pthread_mutex_lock(&buffer->mutex); // lock mutex

    while (buffer->occupied <= 0) {
        // while the buffer is still occupied, wait
        pthread_cond_wait(&buffer->more, &buffer->mutex); 
    }

    assert(buffer->occupied > 0); // assert that the buffer is not occupied

    if (pred(pnum)) { csum += pnum; } // if pnum can be consumed, add it to csum
    buffer->occupied--; // unset buffer to unoccupied

    pthread_cond_signal(&buffer->less); // tell the producer that the buffer is unoccupied

    pthread_mutex_unlock(&buffer->mutex); // unlock mutex

    return pnum;
}

void* Consume(void* a) {
    int p;

    p = 1;
    while (p) {
        printf("@C-READY\n");
        p = consumeT();
        printf("@CONSUMED %d\n", csum);
    }
    printf("@C-EXIT\n");
    pthread_exit(NULL);
}


int main(int argc, const char* argv[]) {
    // the current number predicate
    static pthread_t prod, cons;
    long rc;
    buffer = malloc(sizeof(struct buffer_t));
    buffer->occupied = 0;
    rc = pthread_mutex_init(&buffer->mutex, NULL);
    rc = pthread_cond_init(&buffer->more, NULL);
    rc = pthread_cond_init(&buffer->less, NULL);


    pred = &cond1;
    if (argc > 1) {
        if (!strncmp(argv[1], "2", 10)) { pred = &cond2; }
        else if (!strncmp(argv[1], "3", 10)) { pred = &cond3; }
    }


    pnum = 999;
    csum = 0;
    srand(time(0));

    printf("@P-CREATE\n");
    rc = pthread_create(&prod, NULL, Produce, (void*)0);
    if (rc) {
        printf("@P-ERROR %ld\n", rc);
        exit(-1);
    }
    printf("@C-CREATE\n");
    rc = pthread_create(&cons, NULL, Consume, (void*)0);
    if (rc) {
        printf("@C-ERROR %ld\n", rc);
        exit(-1);
    }

    printf("@P-JOIN\n");
    pthread_join(prod, NULL);
    printf("@C-JOIN\n");
    pthread_join(cons, NULL);


    printf("@CSUM=%d.\n", csum);

    return 0;
}