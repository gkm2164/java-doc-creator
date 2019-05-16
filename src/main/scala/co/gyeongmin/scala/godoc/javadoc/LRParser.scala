package co.gyeongmin.scala.godoc.javadoc

import co.gyeongmin.scala.godoc.exceptions.TokenNotAcceptedException
import co.gyeongmin.scala.godoc.javadoc.LRParser._

import scala.annotation.tailrec

object LRParser {

  def main(args: Array[String]): Unit = {
    println(LRParserRunner.matchStack(
      Token("x") :: Plus :: Token("y") :: Mult :: Token("z") :: Div :: Token("k") :: Semicolon :: Nil, Nil))
  }

  trait LRSymbol {
    def value: String
  }

  trait LRToken extends LRSymbol {
    def isOneOf(tokens: LRToken*): Boolean = tokens.contains(this)
  }

  /*
  S -> E ;
  E -> E (+, -) E
     | T
  T -> T (*, /) T
     | F
  F -> ( E )
     | Token

   */

  case class S(value: String) extends LRSymbol
  case class E(value: String) extends LRSymbol
  case class T(value: String) extends LRSymbol
  case class F(value: String) extends LRSymbol

  case class Token(value: String) extends LRToken

  abstract class Op extends LRToken

  case object Plus extends Op {
    override def value: String = "+"
  }
  case object Minus extends Op {
    override def value: String = "-"
  }
  case object Mult extends Op {
    override def value: String = "*"
  }
  case object Div extends Op {
    override def value: String = "/"
  }

  case object LBracket extends LRToken {
    override def value: String = "("
  }
  case object RBracket extends LRToken {
    override def value: String = ")"
  }
  case object Semicolon extends LRToken {
    override def value: String = ";"
  }
}

object LRParserRunner {
  @tailrec
  def matchStack(tokens: List[LRToken], stack: List[LRSymbol]): String = {
    println(s"tokens: ${tokens.mkString("[", ", ", "]")}," +
      s" stack: ${stack.mkString("[", ", ", "]")}")
    (tokens, stack) match {
      case (_, E(y) :: (op@(Plus | Minus)) :: E(x) :: t) =>
        println("reduce => E -> E (+ | -) E")
        matchStack(tokens, E(s"($x ${op.value} $y)") :: t)

      case (_, T(y) :: (op@(Mult | Div)) :: T(x) :: t) =>
        println("reduce => F -> F (* | /) F")
        matchStack(tokens, T(s"($x ${op.value} $y)") :: t)

      case (_, F(v) :: t) =>
        println("reduce => F -> T")
        matchStack(tokens, T(v) :: t)

      case (op :: _, T(v) :: t) if !op.isOneOf(Mult, Div)=>
        println(s"reduce => E -> T (lookahead 1 token, $op)")
        matchStack(tokens, E(v) :: t)

      case (_, Token(v) :: t) =>
        println(s"""reduce => F -> Token("$v")""")
        matchStack(tokens, F(v) :: t)

      case (_, RBracket :: E(x) :: LBracket :: t) =>
        println("reduce => F -> ( E )")
        matchStack(tokens, F(s"$x ") :: t)

      case (_, Semicolon :: E(v) :: t) if t.isEmpty =>
        println("reduce => S -> E ;")
        matchStack(tokens, S(v) :: t)

      case (Nil, S(v) :: Nil) => v

      case _ => tokens match {
        case Nil => throw new TokenNotAcceptedException(s"error, $stack")
        case h :: t =>
          println("shift")
          matchStack(t, h :: stack)
      }
    }
  }
}
