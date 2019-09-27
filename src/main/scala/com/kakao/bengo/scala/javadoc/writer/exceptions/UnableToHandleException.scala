package com.kakao.bengo.scala.javadoc.writer.exceptions

import com.kakao.bengo.scala.javadoc.JavaSToken

abstract class ParseException(str: String) extends Throwable(str)

abstract class UnrecoverableException(msg: String) extends ParseException(msg)
abstract class RecoverableException(msg: String) extends ParseException(msg)

class TokenListEmptyException() extends RecoverableException("token list is empty")

class TokenNotAllowedException(msg: String, tokens: List[JavaSToken])
  extends RecoverableException(s"expected token has not arrived ${tokens.take(5).map(x => x.tokenType.toString).mkString(", ")}.") {
}

class ParseFailException(reason: String) extends UnrecoverableException(reason)
