package co.gyeongmin.lang.javadoc.codeformatter

import cats._
import co.gyeongmin.lang.javadoc.JavaSToken
import co.gyeongmin.lang.javadoc.codeformatter.exceptions.FormatterError
import co.gyeongmin.lang.javadoc.config.DebugOption
import co.gyeongmin.lang.javalang.JavaTokenEnum

import scala.language.implicitConversions

package object monad {
  type CodeWriter[A] = CodeWriterState => CodeWriterValue[A]
  type CodeWriterValue[A] = (CodeWriterState, Either[FormatterError, A])

  class CodeWriterLogger {
    private val logPrinter: StringBuilder = new StringBuilder()
    def print(str: => String): Unit = logPrinter.append(s"$str")
    def println(str: => String): Unit = logPrinter.append(s"$str\n")
    def printConsole(): Unit = Predef.println(logPrinter.toString)
    def printString(): String = logPrinter.toString
  }

  case class CodeWriterConfig(
      debug: DebugOption,
      logger: Option[CodeWriterLogger] = None
  ) {
    def print(str: => String): Unit = logger.foreach(_.print(str))
    def println(str: => String): Unit = logger.foreach(_.println(str))
    def printConsole(): Unit = logger.foreach(_.printConsole())
    def printStacktraceString(): String =
      logger.map(_.printString()).getOrElse("")
  }

  case class CodeWriterStackElem(idx: Int, token: JavaSToken, context: String) {
    override def toString: String = {
      val JavaSToken(enum, value) = token

      s"""$context:$idx:$enum(${capWithDoubleQuote(value)})"""
    }

    private def capWithDoubleQuote(str: String): String =
      if (
        token.tokenType == JavaTokenEnum.STRING ||
        token.tokenType == JavaTokenEnum.CHAR
      ) str
      else s""""$str""""
  }

  case class CodeWriterState(
      tokens: List[(JavaSToken, Int)],
      stringBuilder: IndentAwareStringBuilder,
      stack: List[CodeWriterStackElem],
      config: CodeWriterConfig
  )

  object CodeWriter {

    def pure[A](value: A): CodeWriter[A] = state => {
      (state, Right(value))
    }

    def apply[A](
      f: List[(JavaSToken, Int)] => (
        List[(JavaSToken, Int)],
        Either[FormatterError, A]
      )
    ): CodeWriter[A] = { case CodeWriterState(tokenList, sb, stack, config) =>
      val (nextList, value) = f(tokenList)
      (CodeWriterState(nextList, sb, stack, config), value)
    }
  }

  implicit val codeWriterMonad: Monad[CodeWriter] = new Monad[CodeWriter] {
    override def flatMap[A, B](
      ma: CodeWriter[A]
    )(f: A => CodeWriter[B]): CodeWriter[B] = prevState => {
      val (newState, value) = ma(prevState)
      value match {
        case Right(v) => f(v)(newState)
        case Left(e)  => (newState, Left(e))
      }
    }

    override def pure[A](x: A): CodeWriter[A] = CodeWriter.pure(x)

    override def tailRecM[A, B](
      a: A
    )(f: A => CodeWriter[Either[A, B]]): CodeWriter[B] = {
      case CodeWriterState(tokenList, stringBuilder, syntaxStack, config) =>
        f(a)(
          CodeWriterState(tokenList, stringBuilder, syntaxStack, config)
        ) match {
          case (nextState, Right(Right(done))) => (nextState, Right(done))
          case (nextState, Right(Left(nextA))) => tailRecM(nextA)(f)(nextState)
          case (nextState, Left(e))            => (nextState, Left(e))
        }
    }
  }
}
