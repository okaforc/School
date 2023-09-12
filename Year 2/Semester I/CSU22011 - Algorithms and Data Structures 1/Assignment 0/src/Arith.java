// -------------------------------------------------------------------------
/**
 *  Utility class containing validation/evaluation/conversion operations
 *  for infix arithmetic expressions.
 *
 *  @author  
 *  @version 1/12/15 13:03:48
 */

public class Arith 
{


  //~ Validation methods ..........................................................


  /**
   * Validation method for infix notation.
   *
   * @param infixLiterals : an array containing the string literals hopefully in infix order.
   * The method assumes that each of these literals can be one of:
   * - "+", "-", "*", or "/"
   * - or a valid string representation of an integer "0", "1" , "2", ..., "-1", "-2", ...
   *
   * @return true if the parameter is indeed in infix notation, and false otherwise.
   **/
  public static boolean validateInfixOrder(String infixLiterals[])
  {
    //TODO
  }


  //~ Evaluation  methods ..........................................................


  /**
   * Evaluation method for infix notation.
   *
   * @param infixLiterals : an array containing the string literals in infix order.
   * The method assumes that each of these literals can be one of:
   * - "+", "-", "*", or "/"
   * - or a valid string representation of an integer.
   *
   * @return the integer result of evaluating the expression
   **/
  public static int evaluateInfixOrder(String infixLiterals[])
  {
    //TODO
  }

  //~ Conversion  methods ..........................................................


  /**
   * Converts infix to postfix.
   *
   * @param infixLiterals : an array containing the string literals in infix order.
   * The method assumes that each of these literals can be one of:
   * - "+", "-", "*", or "/"
   * - or a valid string representation of an integer.
   *
   * @return the expression in postfix order.
   **/
  public static String[] convertInfixToPostfix(String infixLiterals[])
  {
    //TODO
  }


  /**
   * Converts postfix to infix.
   *
   * @param postfixLiterals : an array containing the string literals in postfix order.
   * The method assumes that each of these literals can be one of:
   * - "+", "-", "*", or "/"
   * - or a valid string representation of an integer.
   *
   * @return the expression in infix order.
   **/
  public static String[] convertPostfixToInfix(String postfixLiterals[])
  {
    //TODO
  }

}
