package com.kakao.bengo.scala.javadoc.codeformatter

import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.javalang.JavaTokenEnum.{GT, RIGHT_SHIFT, U_RIGHT_SHIFT}
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.codeformatter.exceptions.{ParseFailException, TokenListEmptyException, TokenNotAllowedException}
import com.kakao.bengo.scala.javadoc.codeformatter.monad.CodeWriter

object Helper {
  import syntax._

  def fail[T](reason: String): CodeWriter[T] = CodeWriter { tks => (tks, Left(new ParseFailException(reason))) }

  def none: CodeWriter[Unit] = CodeWriter.pure(())

  def unrollingRightShift: CodeWriter[Unit] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (JavaSToken(shiftType@(RIGHT_SHIFT | U_RIGHT_SHIFT), _), idx) :: t => shiftType match {
      case RIGHT_SHIFT => ((JavaSToken(GT, ">"), idx) :: t, Right())
      case U_RIGHT_SHIFT => ((JavaSToken(RIGHT_SHIFT, ">>"), idx) :: t, Right())
      case _ => (Nil, Left(new ParseFailException("unexpected")))
    }
    case tokenList => (tokenList, Left(new TokenNotAllowedException("token is not '>>'", tokenList)))
  }, "unrollingRightShift")

  def consumeTokens(enums: List[JavaTokenEnum]): CodeWriter[JavaSToken] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (st@JavaSToken(v, _), _) :: t if enums.contains(v) => (t, Right(st))
    case tokenList@(JavaSToken(v, _), _) :: _ =>
      (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but should one of [${enums.map(_.toString).mkString(", ")}]", tokenList)))
  }

  implicit class EnumSyntax(thisToken: JavaTokenEnum) {
    def |(elem: JavaTokenEnum): List[JavaTokenEnum] = List(thisToken, elem)
  }

  implicit class EnumsSyntax(thisTokens: List[JavaTokenEnum]) {
    def |(elem: JavaTokenEnum): List[JavaTokenEnum] = thisTokens :+ elem
  }

  def takeToken(enum: JavaTokenEnum): CodeWriter[String] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (JavaSToken(v, str), _) :: t if v == enum => (t, Right(str))
    case tokenList@(JavaSToken(v, _), _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but $enum", tokenList)))
  }, s"takeToken($enum)")

  def takeTokens(enums: List[JavaTokenEnum], converter: JavaSToken => String = _.value): CodeWriter[String] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (token@JavaSToken(v, str), _) :: t if enums.contains(v) => (t, Right(converter(token)))
    case tokenList@(JavaSToken(v, _), _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but should be one of ${enums.mkString(", ")}", tokenList)))
  }, s"takeTokens(${enums.mkString(" | ")})").hint(enums: _*)

  def tag[A](writer: CodeWriter[A], t: String): CodeWriter[A] = writer.pushTag(t)

  def assertToken(enum: JavaTokenEnum): CodeWriter[Unit] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (JavaSToken(v, _), _) :: t if v == enum => (t, Right())
    case tokenList@(JavaSToken(v, _), _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but $enum", tokenList)))
  }, s"assertToken($enum)")

  def assertTokens(enums: List[JavaTokenEnum]): CodeWriter[Unit] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (JavaSToken(v, _), _) :: t if enums.contains(v) => (t, Right())
    case tokenList@(JavaSToken(v, _), _) :: _ => (tokenList,
      Left(new TokenNotAllowedException(s"not allow $v, but one of [${enums.mkString(", ")}]", tokenList)))
  }, s"assertTokens(${enums.mkString(" | ")})")

  def css(keyword: String, cssString: String*): String = s"""<span style="${cssString.mkString(" ")}">$keyword</span>"""

  def color(keyword: String, colorValue: String): String = css(keyword, s"color: $colorValue;")

  def keyword(tag: String): String = color(tag, "#D27428")

  def block(lit: String): String = color(lit, "#00b800")

  def typeNameCss: String => String = css(_, "font-weight: bold;", "color: #e8e4d6")

}
