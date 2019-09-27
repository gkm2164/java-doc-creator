package com.kakao.bengo.scala.javadoc.writer

import cats.data.State
import com.kakao.bengo.scala.functional.Monad
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.writer.exceptions._

import scala.language.{higherKinds, implicitConversions}

object CodeWriter {
  type CodeWriter[A] = List[JavaSToken] => IndentAwareStringBuilder => CodeWriterValue[A]
  type CodeWriterValue[A] = (List[JavaSToken], IndentAwareStringBuilder, Either[Throwable, A])
  type NextCodeWriterMonad[A] = List[JavaSToken] => CodeWriter[A]

  implicit val codeWriterMonad: Monad[CodeWriter] = new Monad[CodeWriter] {
    override def flatMap[A, B](ma: CodeWriter[A])(f: A => CodeWriter[B]): CodeWriter[B] = tokenList => sb => {
      val (nextStates, newSb, value) = ma(tokenList)(sb)
      value match {
        case Right(v) => f(v)(nextStates)(newSb)
        case Left(e) => (nextStates, newSb, Left(e))
      }
    }

    override def unit[A](x: A): CodeWriter[A] = CodeWriter.pure(x)
  }

  implicit class MonadDefaultExtension[F[_] : Monad, A](ma: F[A]) {
    private val m: Monad[F] = implicitly[Monad[F]]

    def flatMap[B](f: A => F[B]): F[B] = m.flatMap(ma)(f)

    def map[B](f: A => B): F[B] = m.map(ma)(f)

    def unit(value: A): F[A] = m.unit(value)
  }

  implicit class CodeWriterExt[A](thisWriter: CodeWriter[A]) {

    def doSomething(x: => Unit): CodeWriter[A] = {
      x
      thisWriter
    }

    def debug(msg: String): CodeWriter[A] = {
      println("[DEBUG]: " + msg)
      tokenList => thisWriter.run(tokenList)
    }

    def tell(something: String): CodeWriter[A] = {
      tokenList =>
        sb => {
          val (nextState, newSb, v) = thisWriter.run(tokenList)(sb)
          v match {
            case Right(_) => (nextState, newSb.append(something), v)
            case Left(_) => (nextState, newSb, v)
          }
        }
    }

    def run(state: List[JavaSToken]): IndentAwareStringBuilder => CodeWriterValue[A] = thisWriter(state)

    def tab(): CodeWriter[A] = {
      state =>
        sb => {
          val (nextState, newSb, v) = thisWriter.run(state)(sb)
          (nextState, newSb.tab, v)
        }
    }

    def untab(): CodeWriter[A] =
      tokens => oldSb => {
        val (nextState, sb, v) = thisWriter.run(tokens)(oldSb)
        (nextState, sb.untab, v)
      }

    def enter(): CodeWriter[A] = tokens => oldSb => {
      val (nextState, sb, v) = thisWriter.run(tokens)(oldSb)
      (nextState, sb.enter(), v)
    }

    def enterIf(cond: Boolean): CodeWriter[A] = tokenList => oldSb => {
      val (nextState, sb, v) = thisWriter.run(tokenList)(oldSb)
      val nextSb = if (cond) sb.enter() else sb
      (nextState, nextSb, v)
    }

    def forceIndent(indent: Int): CodeWriter[A] = tokenList => oldSb => {
      val (nextState, sb, v) = thisWriter.run(tokenList)(oldSb)
      (nextState, sb.set(indent), v)
    }

    def recoverIndent(): CodeWriter[A] =
      tokenList => oldSb => {
        val (nextState, sb, v) = thisWriter.run(tokenList)(oldSb)
        (nextState, sb.unset, v)
      }

    def ||(x: CodeWriter[A]): CodeWriter[A] = orElse(x)

    def orElse(otherWriter: CodeWriter[A]): CodeWriter[A] = {
      println("came to orElse()")
      tokenList =>
        sb => {
          val tmpStringBuilder = new IndentAwareStringBuilder(sb)
          val (nextTokenList, newSB, ret) = thisWriter.run(tokenList)(tmpStringBuilder)
          println("???")
          ret match {
            case Right(_) => (nextTokenList, newSB, ret)
            case Left(_: RecoverableException) => otherWriter.run(tokenList)(sb)
            case Left(e: UnrecoverableException) => throw e
            case _ => throw new RuntimeException()
          }
        }
    }

    def collect(tokens: List[JavaSToken]): String =
      thisWriter.run(tokens)(new IndentAwareStringBuilder(0))._2.toString

    def execute(tokens: List[JavaSToken]): IndentAwareStringBuilder => CodeWriterValue[A] = sb => {
      thisWriter.run(tokens)(sb)
    }

    def executeA(tokens: List[JavaSToken]): IndentAwareStringBuilder => A =
      sb => thisWriter.run(tokens)(sb)._3.right.get
  }

  def apply[A](f: List[JavaSToken] => (List[JavaSToken], Either[Throwable, A])): CodeWriter[A] = tokenList => sb => {
    val (nextList, value) = f(tokenList)
    (nextList, sb, value)
  }

  def pure[A](value: A): CodeWriter[A] = tokenList => sb => {
    (tokenList, sb, Right(value))
  }

  implicit def nextCodeWriterMonadConversion[A](nextCodeWriterMonad: NextCodeWriterMonad[A]): CodeWriter[A] =
    tokens => sb => nextCodeWriterMonad(tokens).run(tokens)(sb)

  object NextCodeWriterMonad {
    def apply[A](f: List[JavaSToken] => CodeWriter[A]): NextCodeWriterMonad[A] = f
  }

}