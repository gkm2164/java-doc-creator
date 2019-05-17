package com.kakao.bengo.scala

package object parser {

  trait LRSymbol {
    def value: String
  }

  trait LRToken extends LRSymbol {
    def isOneOf(tokens: LRToken*): Boolean = tokens.contains(this)
  }

  /*
  S -> C ;
  C -> C (==, ||, &&) C
     | E
  E -> E (+, -) E
     | T
  T -> T (*, /) T
     | F
  F -> ( E )
     | Token
     | ! F

   */

  abstract class Op extends LRToken

  abstract class CondOp extends Op

  case class S(value: String) extends LRSymbol

  case class C(value: String) extends LRSymbol

  case class E(value: String) extends LRSymbol

  case class T(value: String) extends LRSymbol

  case class F(value: String) extends LRSymbol

  case class Token(value: String) extends LRToken

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

  case object Equal extends CondOp {
    override def value: String = "=="
  }

  case object NotEqual extends CondOp {
    override def value: String = "!="
  }

  case object Less extends CondOp {
    override def value: String = "<"
  }

  case object Greater extends CondOp {
    override def value: String = ">"
  }

  case object LessEqual extends CondOp {
    override def value: String = "<="
  }

  case object GreaterEqual extends CondOp {
    override def value: String = ">="
  }

  case object Or extends CondOp {
    override def value: String = "||"
  }

  case object And extends CondOp {
    override def value: String = "&&"
  }

  case object Not extends Op {
    override def value: String = "!"
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
