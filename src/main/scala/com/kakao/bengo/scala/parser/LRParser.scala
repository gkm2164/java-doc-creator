package com.kakao.bengo.scala.parser

import com.kakao.bengo.godoc.exceptions.TokenNotAcceptedException

import scala.annotation.tailrec

object LRParser {


  def main(args: Array[String]): Unit = {
    println(parse( // !x + y * z / k == !a;
      Not :: Token("x") :: Plus :: Token("y") :: Mult :: Token("z") :: Div :: Token("k") :: Equal :: Not :: Token("a") :: Semicolon :: Nil, Nil))
  }

  @tailrec
  def parse(tokens: List[LRToken], stack: List[LRSymbol]): String = {
    println(s"tokens: ${tokens.mkString("[", ", ", "]")}," +
      s" stack: ${stack.mkString("[", ", ", "]")}")
    (tokens, stack) match {
      case (_, E(y) :: (op@(Plus | Minus)) :: E(x) :: t) =>
        println("reduce => E -> E (+ | -) E")
        parse(tokens, E(s"($x ${op.value} $y)") :: t)

      case (op :: _, E(v) :: t) if !op.isOneOf(Plus, Minus) =>
        println(s"reduce => C -> E (lookahead 1 token, $op")
        parse(tokens, C(v) :: t)

      case (_, T(y) :: (op@(Mult | Div)) :: T(x) :: t) =>
        println("reduce => T -> T (* | /) T")
        parse(tokens, T(s"($x ${op.value} $y)") :: t)

      case (op :: _, T(v) :: t) if !op.isOneOf(Mult, Div) =>
        println(s"reduce => E -> T (lookahead 1 token, $op)")
        parse(tokens, E(v) :: t)

      case (_, F(v) :: Not :: t) =>
        println(s"reduce => F -> ! F")
        parse(tokens, F(s"!$v") :: t)

      case (_, F(v) :: t) =>
        println("reduce => T -> F")
        parse(tokens, T(v) :: t)

      case (_, Token(v) :: t) =>
        println(s"""reduce => F -> Token("$v")""")
        parse(tokens, F(v) :: t)

      case (_, RBracket :: E(x) :: LBracket :: t) =>
        println("reduce => F -> ( E )")
        parse(tokens, F(s"$x ") :: t)

      case (_, C(y) :: (op: CondOp) :: C(x) :: t) =>
        println("reduce => C -> C (==, ||, &&, ...) C")
        parse(tokens, C(s"($x ${op.value} $y)") :: t)

      case (op :: _, C(v) :: t) if !op.isOneOf(Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual, Or, And) =>
        println(s"reduce => S -> C (lookahead 1 token, $op)")
        parse(tokens, S(v) :: t)

      case (_, Semicolon :: S(v) :: t) =>
        println("reduce => S -> S ;")
        parse(tokens, S(v) :: t)

      case (Nil, S(v) :: Nil) => v

      case _ => tokens match {
        case Nil => throw new TokenNotAcceptedException(s"error, $stack")
        case h :: t => println("shift"); parse(t, h :: stack)
      }
    }
  }
}

object LRParserRunner {

}
