#include "postfix.h"

int isOperator(char str[]);
int isOperand(char str[]);
static double calculate(double a, double b, char op[]);

static char plu[] = "+", min[] = "-", mul[] = "X", divi[] = "/", ind[] = "^";

// evaluate expression stored as an array of string tokens
double evaluate_postfix_expression(char **args, int nargs) {
    struct double_stack *values = double_stack_new(nargs - 1);

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

// check is a string is a valid operator
int isOperator(char str[]) {
    if (strcmp(str, plu) == 0 || strcmp(str, min) == 0 || strcmp(str, mul) == 0 || strcmp(str, divi) == 0 || strcmp(str, ind) == 0) {
        return 1;
    }
    return 0;
}

// check is a string is a valid number
int isOperand(char str[]) {
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

// perform a calculation given two numbers and an operator
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