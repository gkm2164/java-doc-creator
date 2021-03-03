package co.gyeongmin.lang.javadoc.codeformatter.exceptions

import co.gyeongmin.lang.javadoc.JavaSToken

abstract class FormatterError(msg: String) {
  def asJavaException: RuntimeException = throw new RuntimeException(msg)
}

abstract class UnrecoverableError(msg: String) extends FormatterError(msg)
abstract class RecoverableError(msg: String) extends FormatterError(msg)

final case class TokenListEmptyError()
    extends RecoverableError("token list is empty")

case object TokenListIsNotEmptyError
    extends RecoverableError("token list must be empty")

final case class TokenNotAllowedError(
    msg: String,
    tokens: List[(JavaSToken, Int)]
) extends RecoverableError(
      s"expected token has not arrived ${tokens.take(5).map(x => s"${x._2}: ${x._1.tokenType}").mkString(", ")}."
    ) {}

final case class ParseFailError(reason: String)
    extends UnrecoverableError(reason)
