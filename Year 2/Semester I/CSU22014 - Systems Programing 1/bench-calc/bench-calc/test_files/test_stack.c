#include "../infix.h"
#include "../postfix.h"
#include "../stack.h"
#include <string.h>


static char plu[] = "+", min[] = "-", mul[] = "X", divi[] = "/", ind[] = "^";
static int size;

static int isOperator(char str[]);
static int isOperand(char str[]);
static int calcPrec(char str[]);
double dothis(int argc, char **argv);
static int isOperatorInt(int a);
static int calcPrecInt(int a);
double peek(struct double_stack *a);

int main(int argc, char **argv) {
    double v = dothis(argc, argv);
    printf("%f\n", v);
    return 0;
}

double dothis(int nargs, char **args) {
    struct double_stack *values = double_stack_new(nargs - 1);
    memset(values->items, 0, (nargs - 1) * sizeof(double));
    int i;
    char output[] = "";

    for (i = 0; i < nargs; i++) {
        char *cur = args[i];
        if (isOperand(cur) == 1) {
            strcat(output, cur); // concatenate output string and current string
            strcat(output, ","); // add gap
        } else if (strcmp(cur, "(") == 0) {
            double_stack_push(values, atof(cur));
        } else if (isOperator(cur) == 1) {
            // char op[2] = {(char)(peek(values)), '\0'}; // convert operator (as a double) at the top of the stack to ascii
            while (isOperatorInt((int)(peek(values))) && calcPrecInt((int)(peek(values))) >= calcPrec(cur)) {
                char q[2] = {(char)(double_stack_pop(values)), '\0'}; // convert double to string
                strcat(output, q);
                strcat(output, ","); // add gap
            }
            double_stack_push(values, atof(cur));
        } else if (strcmp(cur, ")") == 0) {
            while ((int)(peek(values)) != '(') {
                char q[2] = {(char)(double_stack_pop(values)), '\0'};
                strcat(output, q);
                strcat(output, ","); // add gap
            }
            double_stack_pop(values);
        }
    }

    while (values->top > 0) {
        char q[2] = {(char)(double_stack_pop(values)), '\0'}; // convert double to string
        strcat(output, q);
        strcat(output, ","); // add gap
    }

    size = strlen(output);
    char *arr[size];

    for (int i = 0; i < strlen(output) - 1; i++) {
        // split array on "," and add to **arr
        arr[i] = strtok(output, ",");
        size--;
    }

    return evaluate_postfix_expression(arr, size);
    // return 0;
}


static int isOperator(char str[]) {
    if (strcmp(str, plu) == 0 || strcmp(str, min) == 0 || strcmp(str, mul) == 0 || strcmp(str, divi) == 0 || strcmp(str, ind) == 0) {
        return 1;
    }
    return 0;
}

static int isOperand(char str[]) {
    for (int i = 0; i < strlen(str); i++) {
        if (!(str[i] >= '0' && str[i] <= '9')) {
            return 0;
        }
    }
    return 1;
}

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

static int isOperatorInt(int a) {
    if (a == '+' || a == '-' || a == '*' || a == '/' || a == '^') {
        return 1;
    }
    return 0;
}