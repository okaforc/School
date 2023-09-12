#include "bench-calc/bench-calc/postfix.h"
#include "bench-calc/bench-calc/stack.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int isOperator(char str[]);
static int isOperand(char str[]);
static double calculate(double a, double b, char op[]);
// peek at the top of a given stack
static double peek(struct double_stack *a);

static char plu[] = "+", min[] = "-", mul[] = "X", divi[] = "/", ind[] = "^";

int main(int nargs, char const **args) {
    struct double_stack *values = double_stack_new(20);
    

    unsigned y = 10010;
    unsigned b = y >> 1;
    printf("%d\n", b);
    

    return 0;
}

static int isOperand(char str[]) {
    if (strcmp(str, "-") == 0) {
        return 0;
    } else {
        for (int i = 0; i < strlen(str); i++) {
            if (!(str[i] >= '0' && str[i] <= '9') && str[i] != '.' && str[i] != '-') {
                return 0;
            }
        }
    }

    return 1;
}

// evaluate expression stored as an array of string tokens
double evaluate_postfix_expression(char **args, int nargs) {
    struct double_stack *values = double_stack_new(nargs - 1);
    memset(values->items, 0, (nargs - 1) * sizeof(double));

    for (int i = 0; i < nargs; i++) {
        char *cur = args[i];
        if (isOperator(cur) == 1) {
            double x = double_stack_pop(values);
            double y = double_stack_pop(values);
            double_stack_push(values, calculate(y, x, cur));
        } else if (isOperand(cur) == 1) {
            double_stack_push(values, atof(cur));
        }
    }

    return double_stack_pop(values);
}

static int isOperator(char str[]) {
    if (strcmp(str, plu) == 0 || strcmp(str, min) == 0 || strcmp(str, mul) == 0 || strcmp(str, divi) == 0 || strcmp(str, ind) == 0) {
        return 1;
    }
    return 0;
}



static double calculate(double a, double b, char op[]) {
    if (strcmp(op, plu) == 0) {
        return a + b;
    } else if (strcmp(op, min) == 0) {
        return a - b;
    } else if (strcmp(op, mul) == 0) {
        return a * b;
    } else if (strcmp(op, divi) == 0) {
        return a / b;
    } else {
        return pow(a, b);
    }
}

struct double_stack *double_stack_new(int max_size) {
    struct double_stack *result;

    // allocate space for the stack header
    result = malloc(sizeof(struct double_stack));
    result->max_size = max_size;
    result->top = 0;
    // allocate space for the data stored in the stack
    result->items = malloc(sizeof(double) * max_size);
    // return a pointer to the newly-allocated stack
    return result;
}

// push a value onto the stack
void double_stack_push(struct double_stack *this, double value) {
    assert(this->top < this->max_size);
    this->items[++this->top] = value;
}

// pop a value from the stack
double double_stack_pop(struct double_stack *this) {
    assert(this->top > 0);
    return this->items[this->top--];
}

// peek at the top of a given stack
static double peek(struct double_stack *a) { return a->items[a->top]; }