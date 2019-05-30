package com.kakao.bengo.scala.parser

import com.kakao.bengo.godoc.exceptions.TokenNotAcceptedException

import scala.annotation.tailrec

object LRParser {
  def main(args: Array[String]): Unit = {
    println(parse( // !x + y * z / k == !a;
      Not :: Token("x") :: Plus :: Token("y") :: Mult :: Token("z") :: Div :: Token("k") :: Equal :: Not :: Token("a") :: Semicolon :: Nil, Nil))
  }


  /*
    (C ~ B ~ A) :: D  = A :: B :: C
  *
  */
  //  def ~[A](tail: List[A], head: A): List[A] = head :: tail

  object ~ {
    def unapply[A](list: List[A]): Option[(List[A], A)] = if (list.nonEmpty) Some((list.tail, list.head)) else None
  }

  @tailrec
  def parse(tokens: List[LRToken], stack: List[LRSymbol]): String = {
    println(s"tokens: ${tokens.mkString("[", ", ", "]")}," +
      s" stack: ${stack.mkString("[", ", ", "]")}")
    (tokens, stack) match {
      case (_, t ~ E(x) ~ (op@(Plus | Minus)) ~ E(y)) =>
        println("reduce => E -> E (+ | -) E")
        parse(tokens, E(s"($x ${op.value} $y)") :: t)

      case (op :: _, t ~ E(v)) if !op.isOneOf(Plus, Minus) =>
        println(s"reduce => C -> E (lookahead 1 token, $op")
        parse(tokens, C(v) :: t)

      case (_, t ~ T(x) ~ (op@(Mult | Div)) ~ T(y)) =>
        println("reduce => T -> T (* | /) T")
        parse(tokens, T(s"($x ${op.value} $y)") :: t)

      case (op :: _, t ~ T(v)) if !op.isOneOf(Mult, Div) =>
        println(s"reduce => E -> T (lookahead 1 token, $op)")
        parse(tokens, E(v) :: t)

      case (_, t ~ Not ~ F(v)) =>
        println(s"reduce => F -> ! F")
        parse(tokens, F(s"!$v") :: t)

      case (_, t ~ F(v)) =>
        println("reduce => T -> F")
        parse(tokens, T(v) :: t)

      case (_, t ~ Token(v)) =>
        println(s"""reduce => F -> Token("$v")""")
        parse(tokens, F(v) :: t)

      case (_, t ~ LBracket ~ E(x) ~ RBracket) =>
        println("reduce => F -> ( E )")
        parse(tokens, F(s"$x ") :: t)

      case (_, t ~ C(x) ~ (op: CondOp) ~ C(y)) =>
        println("reduce => C -> C (==, ||, &&, ...) C")
        parse(tokens, C(s"($x ${op.value} $y)") :: t)

      case (op :: _, t ~ C(v)) if !op.isOneOf(Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual, Or, And) =>
        println(s"reduce => S -> C (lookahead 1 token, $op)")
        parse(tokens, S(v) :: t)

      case (_, t ~ S(v) ~ Semicolon) =>
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