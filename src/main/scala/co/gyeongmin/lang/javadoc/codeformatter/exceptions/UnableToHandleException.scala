package co.gyeongmin.lang.javadoc.codeformatter.exceptions

import co.gyeongmin.lang.javadoc.JavaSToken


abstract class ParseException(str: String) extends Throwable(str)

abstract class UnrecoverableException(msg: String) extends ParseException(msg)
abstract class RecoverableException(msg: String) extends ParseException(msg)

class TokenListEmptyException() extends RecoverableException("token list is empty")

class TokenNotAllowedException(msg: String, tokens: List[(JavaSToken, Int)])
  extends RecoverableException(s"expected token has not arrived ${tokens.take(5).map(x => s"${x._2}: ${x._1.tokenType}").mkString(", ")}.") {
}

class ParseFailException(reason: String) extends UnrecoverableException(reason)
