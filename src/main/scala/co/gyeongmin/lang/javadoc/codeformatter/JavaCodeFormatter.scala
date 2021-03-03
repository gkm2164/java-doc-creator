package co.gyeongmin.lang.javadoc.codeformatter

import co.gyeongmin.lang.javadoc.JavaSToken
import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javadoc.config.DebugOption
import co.gyeongmin.lang.javalang.JavaTokenEnum
import co.gyeongmin.lang.javalang.JavaTokenEnum._
import com.typesafe.scalalogging.Logger

object JavaCodeFormatter {

  import syntax._

  private val CommentTokens: Set[JavaTokenEnum] =
    Set(
      COMMENT_BLOCK,
      COMMENT,
      COMMENT_MACRO_EXPLAIN,
      COMMENT_MACRO_CODE,
      COMMENT_MACRO_NAME
    )

  def printCodeBlock(
    codeName: String,
    tokens: Vector[JavaSToken],
    debugOption: DebugOption
  )(implicit log: Logger): Either[ParseError, String] =
    printCodeCommon(codeName, tokens, JavaParser.blockStmt, debugOption)

  def printCode(
    codeName: String,
    tokens: Vector[JavaSToken],
    debugOption: DebugOption
  )(implicit log: Logger): Either[ParseError, String] =
    printCodeCommon(codeName, tokens, JavaParser.javaCode, debugOption)

  private def printCodeCommon[A](
    codeName: String,
    tokens: Vector[JavaSToken],
    startFrom: CodeWriter[A],
    debugOption: DebugOption
  )(implicit log: Logger): Either[ParseError, String] = {
    val reformatTokens = tokens
      .filterNot(x => CommentTokens.contains(x.tokenType))
      .zipWithIndex
      .toList

    if (reformatTokens.nonEmpty) {
      startFrom
        .enter()
        .collect(
          reformatTokens,
          CodeWriterConfig(
            debug = debugOption,
            logger = Some(new CodeWriterLogger)
          )
        )
    } else {
      log.info("seems to be an interface/abstract method")
      Left(EmptyError)
    }
  }

  sealed trait ParseError {
    def shortMessage: String

    def longMessage: String

    final def message: String = shortMessage + "\n" + longMessage
  }

  case class ErrorMessage(shortMessage: String, longMessage: String)
      extends ParseError

  case object EmptyError extends ParseError {
    override val shortMessage: String =
      "there's nothing to do with this function"

    override val longMessage: String = ""
  }
}
