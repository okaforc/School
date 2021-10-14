#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bench-calc/bench-calc/postfix.h"
#include "bench-calc/bench-calc/stack.h"

static int isOperator(char str[]);
static int isOperand(char str[]);
static double calculate(double a, double b, char op[]);
static int isOperatorInt(int a);
static int calcPrec(char str[]);
static int calcPrecInt(int a);
static double peek(struct double_stack *a);

static char plu[] = "+", min[] = "-", mul[] = "X", divi[] = "/", ind[] = "^", delim[] = " ";

int main(int argc, char const *argv[]) {
    // char d = '(';
    // int x = 40;
    // if (scanf("%d", d) == 1) {
    //     printf("dddddd");
    // }

    // char q[] = {'(', '\0'}; // convert double to string
    // printf("%d\n", strcmp(q, "("));

    // split string by "," and add each value to the final array size = strlen(output);
    char output[] = {"3 7 8 + X 22 -"};
    char *delim = " ";

    char *arr[50];
    char *z = strtok(output, delim);
    int j = 0;

    while (z != NULL) {
        arr[j] = z;
        z = strtok(NULL, delim);
        j++;
    }

    int size = j;
    char **farr = arr;
    double s = evaluate_postfix_expression(farr, size);
    printf("%f", s);
    return 0;
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
        } else {
            printf("Error with ");
        }
    }

    return double_stack_pop(values);
}

// evaluate expression stored as an array of string tokens
double evaluate_infix_expression(char **args, int nargs) {
    struct double_stack *values = double_stack_new(nargs - 1);
    memset(values->items, 0, (nargs - 1) * sizeof(double));
    char output[100];

    for (int i = 0; i < nargs; i++) {
        char *cur = args[i];
        if (isOperand(cur) == 1) {
            strcat(output, cur);   // concatenate output string and current string
            strcat(output, delim); // add delimiter
        } else if (strcmp(cur, "(") == 0) {
            double_stack_push(values, cur[0]); // get char of left bracket
        } else if (isOperator(cur) == 1) {
            while (isOperatorInt((int)(peek(values))) == 1 && calcPrecInt((int)(peek(values))) >= calcPrec(cur)) {
                char q[] = {(char)(double_stack_pop(values)), '\0'}; // convert double to string
                strcat(output, q);
                strcat(output, delim); // add delimiter
            }
            double_stack_push(values, cur[0]); // get char of left bracket
        } else if (strcmp(cur, ")") == 0) {
            while ((int)(peek(values)) != '(') {
                char q[] = {(char)(double_stack_pop(values)), '\0'};
                strcat(output, q);
                strcat(output, delim); // add delimiter
            }
            double_stack_pop(values);
        }
    }

    while (values->top > 0) {
        char q[2] = {(char)(double_stack_pop(values)), '\0'}; // convert double to string
        strcat(output, q);
        strcat(output, delim); // add gap
    }

    // split string by "," and add each value to the final array size = strlen(output);
    printf("%s", output);
    char *arr[50];
    char *z = strtok(output, delim);
    int j = 0;

    while (z != NULL) {
        arr[j] = z;
        z = strtok(NULL, delim);
        j++;
    }

    int size = j;
    char **farr = arr;
    double s = evaluate_postfix_expression(farr, size);
    return s;
}

static int isOperator(char str[]) {
    if (strcmp(str, plu) == 0 || strcmp(str, min) == 0 || strcmp(str, mul) == 0 || strcmp(str, divi) == 0 || strcmp(str, ind) == 0) {
        return 1;
    }
    return 0;
}

// check is a string can be parsed as a number
static int isOperand(char str[]) {
    for (int i = 0; i < strlen(str); i++) {
        if (!(str[i] >= '0' && str[i] <= '9') && str[i] != '.') {
            return 0;
        }
    }
    return 1;
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

// check is a char is an operator
static int isOperatorInt(int a) {
    if (a == '+' || a == '-' || a == '*' || a == '/' || a == '^') {
        return 1;
    }
    return 0;
}

// determine the precedence of a string operator
static int calcPrec(char str[]) {
    if (isOperator(str)) {
        if (strcmp(str, plu) == 0) {
            return 1;
        } else if (strcmp(str, min) == 0) {
            return 1;
        } else if (strcmp(str, mul) == 0) {
            return 2;
        } else if (strcmp(str, divi) == 0) {
            return 2;
        } else {
            return 3;
        }
    } else {
        return -1;
    }
}

// determine the precedence of a char operator
static int calcPrecInt(int a) {
    if (isOperatorInt(a) == 1) {
        if (a == '+') {
            return 1;
        } else if (a == '-') {
            return 1;
        } else if (a == '*') {
            return 2;
        } else if (a == '/') {
            return 2;
        } else {
            return 3;
        }
    } else {
        return -1;
    }
}

// peek at the top of a given stack
static double peek(struct double_stack *a) { return a->items[a->top]; }