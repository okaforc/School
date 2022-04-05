#include "infix.h"

static char plu[] = "+", min[] = "-", mul[] = "X", divi[] = "/", ind[] = "^", delim[] = " ";

static int isOperand(char str[]);
static int isOperator(char str[]);
static int calcPrec(int a);
static double peek(struct double_stack *a);
static int isEmpty(struct double_stack *a);

// evaluate expression stored as an array of string tokens
double evaluate_infix_expression(char **args, int nargs) {
    // convert input to postfix to evaluate expression
    struct double_stack *values = double_stack_new(nargs - 1); // create stack
    char output[100]; // create output string

    for (int i = 0; i < nargs; i++) {
        char *cur = args[i];
        
        if (isOperand(cur)) {
            strcat(output, cur);   // concatenate output string and current string
            strcat(output, delim); // add delimiter
        } else if (strcmp(cur, "(") == 0) {
            double_stack_push(values, cur[0]); // get char of left bracket
        } else if (isOperator(cur)) {
            while (!isEmpty(values) && calcPrec((int)(peek(values))) >= calcPrec(cur[0])) {
                char q[] = {(char)(double_stack_pop(values)), '\0'}; // convert double to string
                strcat(output, q);
                strcat(output, delim); // add delimiter
            }
            double_stack_push(values, cur[0]);
        } else if (strcmp(cur, ")") == 0) {
            while ((int)(peek(values)) != '(' && !isEmpty(values)) {
                char q[] = {(char)(double_stack_pop(values)), '\0'};
                strcat(output, q);
                strcat(output, delim); // add delimiter
            }
            double_stack_pop(values);
        }
    }

    while (!isEmpty(values)) {
        char q[] = {(char)(double_stack_pop(values)), '\0'}; // convert double to string
        strcat(output, q);
        strcat(output, delim); // add gap
    }

    // split string by "," and add each value to the final array
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

// check is a string is an operator
static int isOperator(char str[]) {
    if (strcmp(str, plu) == 0 || strcmp(str, min) == 0 || strcmp(str, mul) == 0 || strcmp(str, divi) == 0 || strcmp(str, ind) == 0) {
        return 1;
    }
    return 0;
}

// check if a string is a valid number
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

// determine the precedence of an operator
static int calcPrec(int a) {
    char b[] = {a, '\0'}; // convert char to string
    if (isOperator(b) == 1) {
        if (a == '+') {
            return 1;
        } else if (a == '-') {
            return 1;
        } else if (a == 'X') {
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

// check if a given stack is empty
static int isEmpty(struct double_stack *a) { return a->top == 0; }