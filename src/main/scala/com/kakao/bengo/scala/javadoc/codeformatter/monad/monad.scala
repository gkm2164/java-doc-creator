package com.kakao.bengo.scala.javadoc.codeformatter

import cats.{Applicative, Functor, Monad}
import com.kakao.bengo.scala.javadoc.JavaSToken

import scala.language.{higherKinds, implicitConversions}

package object monad {
  type CodeWriter[A] = CodeWriterState => CodeWriterValue[A]
  type CodeWriterValue[A] = (CodeWriterState, Either[Throwable, A])
  type NextCodeWriterMonad[A] = List[JavaSToken] => CodeWriter[A]

  case class CodeWriterState(tokens: List[JavaSToken], stringBuilder: IndentAwareStringBuilder, stack: List[String])

  object CodeWriter {

    def pure[A](value: A): CodeWriter[A] = state => {
      (state, Right(value))
    }

    def apply[A](f: List[JavaSToken] => (List[JavaSToken], Either[Throwable, A])): CodeWriter[A] = {
      case CodeWriterState(tokenList, sb, stack) =>
        val (nextList, value) = f(tokenList)
        (CodeWriterState(nextList, sb, stack), value)
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
      case CodeWriterState(tokenList, stringBuilder, syntaxStack) =>
        f(a).run(tokenList, stringBuilder, syntaxStack) match {
          case (nextState, Right(Left(nextA))) => tailRecM(nextA)(f)(nextState)
          case (nextState, Right(Right(done))) =>(nextState, Right(done))
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
