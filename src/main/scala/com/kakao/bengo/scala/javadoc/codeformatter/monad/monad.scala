package com.kakao.bengo.scala.javadoc.codeformatter

import cats.{Applicative, Functor, Monad}
import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.scala.javadoc.JavaSToken

import scala.language.{higherKinds, implicitConversions}

package object monad {
  type CodeWriter[A] = CodeWriterState => CodeWriterValue[A]
  type CodeWriterValue[A] = (CodeWriterState, Either[Throwable, A])
  type NextCodeWriterMonad[A] = List[(JavaSToken, Int)] => CodeWriter[A]

  case class DebugOption(stackTrace: Boolean, maxStackSize: Int = 1, onlySuccess: Boolean = true)

  case class CodeWriterConfig(debug: Option[DebugOption] = None)

  case class CodeWriterStackElem(idx: Int, token: JavaSToken, context: String) {
    private def capWithDoubleQuote(str: String): String =
      if (token.tokenType == JavaTokenEnum.STRING ||
          token.tokenType == JavaTokenEnum.CHAR) str
      else s""""$str""""

    override def toString: String = {
      val JavaSToken(enum, value) = token

      s"""$context:$idx=$enum(${capWithDoubleQuote(value)})"""
    }
  }

  case class CodeWriterState(tokens: List[(JavaSToken, Int)],
                             stringBuilder: IndentAwareStringBuilder,
                             stack: List[CodeWriterStackElem],
                             config: CodeWriterConfig)

  object CodeWriter {

    def pure[A](value: A): CodeWriter[A] = state => {
      (state, Right(value))
    }

    def apply[A](f: List[(JavaSToken, Int)] => (List[(JavaSToken, Int)], Either[Throwable, A])): CodeWriter[A] = {
      case CodeWriterState(tokenList, sb, stack, config) =>
        val (nextList, value) = f(tokenList)
        (CodeWriterState(nextList, sb, stack, config), value)
    }
  }

  implicit val codeWriterMonad: Monad[CodeWriter] = new Monad[CodeWriter] {
    override def flatMap[A, B](ma: CodeWriter[A])(f: A => CodeWriter[B]): CodeWriter[B] = prevState => {
      val (newState, value) = ma(prevState)
      value match {
        case Right(v) => f(v)(newState)
        case Left(e) => (newState, Left(e))
      }
    }

    override def pure[A](x: A): CodeWriter[A] = CodeWriter.pure(x)

    import syntax._

    override def tailRecM[A, B](a: A)(f: A => CodeWriter[Either[A, B]]): CodeWriter[B] = {
      case CodeWriterState(tokenList, stringBuilder, syntaxStack, config) =>
        f(a).run(tokenList, stringBuilder, syntaxStack, config) match {
          case (nextState, Right(Right(done))) =>(nextState, Right(done))
          case (nextState, Right(Left(nextA))) => tailRecM(nextA)(f)(nextState)
          case (nextState, Left(e)) => (nextState, Left(e))
        }
    }
  }

  implicit class MonadSyntax[F[_] : Monad, A](ma: F[A]) {
    private val monad = implicitly[Monad[F]]

    def flatMap[B](f: A => F[B]): F[B] = monad.flatMap(ma)(f)

    def map[B](f: A => B): F[B] = monad.map(ma)(f)
  }

}
