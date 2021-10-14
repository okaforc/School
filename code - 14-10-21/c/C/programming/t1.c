#include <assert.h>
#include <malloc.h>

struct double_stack {
    int top;
    int size;
    double *items;
};
const int max_size = 1000;

// create new empty stack
struct double_stack *double_stack_new() {
    const int max_size = 1000;
    struct double_stack *result;
    result = malloc(sizeof(struct double_stack));
    result->top = 0;
    result->size = max_size;
    result->items = malloc(sizeof(double) * max_size);
    return result;
};

void double_stack_free(struct double_stack *this) {
    free(this->items);
    free(this);
}

// push an item onto the stack
void double_stack_push(struct double_stack *this, double item) {
    assert(this->top < max_size);
    this->items[this->top] = item;
    this->top++;
}

int main() {
    // s
    return 0;
}