#include "../infix.h"
#include "../postfix.c"

// i put this file in my test_stack.c file to compile it there. the `../` should be removed if 
// on the same level as postfix.c
/* #include "infix.h"
#include "postfix.c" */

// put here to work on my system. ignore
static int is_operand(char str[]);
static int is_operator(char str[]);
static double peek(struct double_stack *a);
static int isEmpty(struct double_stack *a);

// order was changed, with + being lowest and ^ being the highest.
int check_precedence(char operator) {
    printf("reach line 6");
    if (operator== '^')
        return 3;
    else if (operator== 'x' || operator== '/')
        return 2;
    else if (operator== '+' || operator== '-')
        return 1;
    else
        return 99;
}

/* int is_of_higher_precedence(char current_operator, struct double_stack *operators, char **infix) {
    printf("reach line 15");
    int most_recent_operator_index;
    if (operators->top > 0)
        most_recent_operator_index = (int)double_stack_pop(operators);
    char most_recent_operator = infix[most_recent_operator_index][0];
    int current_op_pre, most_recent_op_pre;
    printf("reach line 19");
    current_op_pre = check_precedence(current_operator);
    most_recent_op_pre = check_precedence(most_recent_operator);
    double_stack_push(operators, most_recent_operator_index);
    if (current_op_pre == 99 || most_recent_op_pre == 99)
        return 99; // ONE OR MORE OF THE OPERATORS IS NOT A VALID OPERATOR
    else if (current_op_pre < most_recent_op_pre)
        return 1; // 1 MEANS THAT THE CURRENT OPERATOR THAT ISNT ON THE STACK YET IS OF HIGHER PRECEDENCE THAN THE MOST RECENT ONE ON THE STACK
    else if (current_op_pre >= most_recent_op_pre)
        return 0; // 0 MEANS THAT THE CURRENT OPERATOR IS NOT OF HIGHER PRECEDENCE THAN THE MOST RECENT ONE ON THE STACK
    else
        return -1; // fail
} */

// also put here to work on my system. also ignore.
// check is a string is an operator
static int is_operator(char str[]) {
    if (strcmp(str, plu) == 0 || strcmp(str, min) == 0 || strcmp(str, mul) == 0 || strcmp(str, divi) == 0 || strcmp(str, ind) == 0) {
        return 1;
    }
    return 0;
}

// check if a string is a valid number
static int is_operand(char str[]) {
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
double evaluate_infix_expression(char **args, int nargs) {
    printf("got to line 30\n");
    struct double_stack *stack = double_stack_new(nargs - 1);
    char **postfix = malloc(sizeof(char **) * nargs);
    int current_postfix_index = 0;
    printf("reach line 32\n");
    for (int i = 0; i < nargs; i++) {
        if (is_operand(args[i])) {
            postfix[current_postfix_index] = args[i];
            current_postfix_index++;
            printf("reach line 37\n");
        } else if (args[i][0] == '(')
            double_stack_push(stack, args[i][0]);
        else if (args[i][0] == ')') {
            while (!isEmpty(stack) && (int)(peek(stack)) != '(') {
                char q[] = {(char)(double_stack_pop(stack)), '\0'};
                postfix[current_postfix_index] = q;
                current_postfix_index++;
            }
            double_stack_pop(stack);
        } else if (is_operator(args[i])) {
            while (!isEmpty(stack) && check_precedence((int)(peek(stack))) >= check_precedence(args[i][0])) {
                char q[] = {(char)(double_stack_pop(stack)), '\0'};
                postfix[current_postfix_index] = q;
                current_postfix_index++;
            }
            double_stack_push(stack, args[i][0]);
        }
    }

    while (!isEmpty(stack)) {
        char q[] = {(char)(double_stack_pop(stack)), '\0'};
        postfix[current_postfix_index] = q;
        current_postfix_index++;
    }

    return evaluate_postfix_expression(postfix, current_postfix_index);
}

int main(int argc, char **argv) {
    int i;
    double fullTotal;
    if (argc == 1) {
        printf("Please try adding some command-line parameters\n");
        printf("Usage: %s <param1> <param2> ...\n", argv[0]);
        exit(1);
    }
    printf("The parameters are:\n");
    printf("%d\n", argc);
    for (i = 1; i < argc; i++) {
        printf("%d: %s\n", i, argv[i]);
    }
    printf("doing infix\n");
    fullTotal = evaluate_infix_expression(argv + 1, argc - 1);
    printf("The total is: %.0lf\n", fullTotal);
    return 0;
}

// peek at the top of a given stack
static double peek(struct double_stack *a) { return a->items[a->top]; }

// check if a given stack is empty
static int isEmpty(struct double_stack *a) { return a->top == 0; }


/*
( 2 ^ ( 2 + 3 ) - 1 ) X 3
#include "infix.h"
#include "postfix.c"

int check_precedence(char operator){
  printf("reach line 6");
  if(operator == '(' || operator == ')') return 0;
  else if(operator == '^') return 1;
  else if(operator == 'x' || operator == '/') return 2;
  else if(operator == '+' || operator == '-') return 3;
  else return 99;
}

int is_of_higher_precedence(char current_operator, struct double_stack * operators, char** infix){
  printf("reach line 15");
  int most_recent_operator_index;
  if(operators -> top > 0) most_recent_operator_index = (int)double_stack_pop(operators);
  char most_recent_operator = infix[most_recent_operator_index][0];
  int current_op_pre, most_recent_op_pre;
  printf("reach line 19");
  current_op_pre = check_precedence(current_operator);
  most_recent_op_pre = check_precedence(most_recent_operator);
  double_stack_push(operators, most_recent_operator_index);
  if(current_op_pre == 99 || most_recent_op_pre == 99) return 99; // ONE OR MORE OF THE OPERATORS IS NOT A VALID OPERATOR
  else if(current_op_pre < most_recent_op_pre) return 1; //1 MEANS THAT THE CURRENT OPERATOR THAT ISNT ON THE STACK YET IS OF HIGHER PRECEDENCE THAN THE MOST RECENT ONE ON THE STACK
  else if(current_op_pre >= most_recent_op_pre) return 0; //0 MEANS THAT THE CURRENT OPERATOR IS NOT OF HIGHER PRECEDENCE THAN THE MOST RECENT ONE ON THE STACK
}

// evaluate expression stored as an array of string tokens
double evaluate_infix_expression(char ** args, int nargs) {
  printf("got to line 30");
  struct double_stack * operator_indexes = double_stack_new(nargs);
  char** postfix = malloc(sizeof(char**) * nargs);
  int current_postfix_index = 0;
  int index_of_left_bracket;
  char char_off_operator_stack;
  printf("reach line 32");
  for(int i = 0; i < nargs; i++){
    if(is_operand(args[i])){
      postfix[current_postfix_index] = args[i];
      current_postfix_index++;
      printf("reach line 37");
    }
    else if(args[i][0] == '(') double_stack_push(operator_indexes, i);
    else if(args[i][0] == ')'){
      if(operator_indexes -> top > 0) index_of_left_bracket = (int)double_stack_pop(operator_indexes);
      char_off_operator_stack = args[index_of_left_bracket][0];
      while(char_off_operator_stack != '('){
        if(operator_indexes -> top > 0) char_off_operator_stack = args[(int)double_stack_pop(operator_indexes)][0];
        postfix[current_postfix_index][0] = char_off_operator_stack;
        current_postfix_index++;
      }
      double_stack_push(operator_indexes, index_of_left_bracket);
    }
    else if(is_operator(args[i])){
      if(is_of_higher_precedence(args[i][0], operator_indexes, args) == 99){
        printf("reach line 55");
        postfix[current_postfix_index] = args[i];
        current_postfix_index++;
      }
      else if(is_of_higher_precedence(args[i][0], operator_indexes, args) == 1){
        double_stack_push(operator_indexes, i);
        printf("reach line 61");
      }
      else if(is_of_higher_precedence(args[i][0], operator_indexes, args) == 0){
        printf("reach line 64");
        if(operator_indexes -> top > 0) postfix[current_postfix_index] = args[(int)double_stack_pop(operator_indexes)];
        current_postfix_index++;
        printf("reach line 67");
      }
    }
  }
}

int main(int argc, char ** argv) {
   int i;
   double fullTotal;
   if ( argc == 1 ) {
     printf("Please try adding some command-line parameters\n");
     printf("Usage: %s <param1> <param2> ...\n", argv[0]);
     exit(1);
   }
   printf("The parameters are:\n");
   for ( i = 1; i < argc; i++ ) {
     printf("%s\n", argv[i]);
   }
   fullTotal = evaluate_infix_expression(argv, argc);
   printf("The total is: %.0lf\n", fullTotal);
   return 0;
}
 */