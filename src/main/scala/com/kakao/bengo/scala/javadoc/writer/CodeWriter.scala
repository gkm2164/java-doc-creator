package com.kakao.bengo.scala.javadoc.writer

import com.kakao.bengo.scala.functional.Monad
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.writer.exceptions._

import scala.language.{higherKinds, implicitConversions}

object CodeWriter {

  type CodeWriter[A] = CodeWriterState => CodeWriterValue[A]
  type CodeWriterValue[A] = (CodeWriterState, Either[Throwable, A])
  type NextCodeWriterMonad[A] = List[JavaSToken] => CodeWriter[A]

  def apply[A](f: List[JavaSToken] => (List[JavaSToken], Either[Throwable, A])): CodeWriter[A] = {
    case CodeWriterState(tokenList, sb, stack) =>
      val (nextList, value) = f(tokenList)
      (CodeWriterState(nextList, sb, stack), value)
  }

  implicit val codeWriterMonad: Monad[CodeWriter] = new Monad[CodeWriter] {
    override def flatMap[A, B](ma: CodeWriter[A])(f: A => CodeWriter[B]): CodeWriter[B] = prevState => {
      val (newState, value) = ma(prevState)
      value match {
        case Right(v) => f(v)(newState)
        case Left(e) => (newState, Left(e))
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

    def matchDebug(tag: String, msg: String = ""): CodeWriter[A] = prevState => {
      val (CodeWriterState(nextTokenList, newSb, newStack), v) = thisWriter(prevState)
      val sb = StringBuilder.newBuilder
      sb ++= "match"
      if (nextTokenList.nonEmpty) {
        val JavaSToken(tokenType, value) = nextTokenList.head
        sb ++= s""" |$tokenType|"$value"| with"""
      }

      sb ++= s" [${newStack.mkString("/")}]"

      if (msg.nonEmpty) {
        sb ++= s": $msg"
      }

      sb ++= s"[${newSb.debugStringBuilder(5)}]"
//
//      v match {
//        case Right(_) => println(sb.toString)
//        case _ =>
//      }
      (CodeWriterState(nextTokenList, newSb, newStack), v)
    }

    def pushTag(tag: String): CodeWriter[A] = {
      case CodeWriterState(prevTokenList, prevSb, prevStack) => {
        val (CodeWriterState(nextTokenList, newSb, _), v) = thisWriter.run(prevTokenList, prevSb, tag :: prevStack)
        (CodeWriterState(nextTokenList, newSb, prevStack), v)
      }
    }

    def run(state: List[JavaSToken], stringBuilder: IndentAwareStringBuilder, stack: List[String]): CodeWriterValue[A] = thisWriter(CodeWriterState(state, stringBuilder, stack))

    def tell(something: String): CodeWriter[A] = prevState => {
      val (CodeWriterState(nextState, newSb, newStack), v) = thisWriter(prevState)
      v match {
        case Right(_) => (CodeWriterState(nextState, newSb.append(something), newStack), v)
        case Left(e) => (CodeWriterState(nextState, newSb, newStack), v)
      }
    }

    def print(fmt: A => String): CodeWriter[Unit] = prevState => {
      val (CodeWriterState(nextState, newSb, newStack), v) = thisWriter(prevState)
      v match {
        case Right(str) => (CodeWriterState(nextState, newSb.append(fmt(str)), newStack), Right())
        case Left(e) => (CodeWriterState(nextState, newSb, newStack), Left(e))
      }
    }

    def tab(): CodeWriter[A] = prevState => {
      val (CodeWriterState(nextState, newSb, newStack), v) = thisWriter(prevState)
      (CodeWriterState(nextState, newSb.tab, newStack), v)
    }

    def untab(): CodeWriter[A] =
      prevState => {
        val (nextState, v) = thisWriter(prevState)
        (nextState.copy(stringBuilder = nextState.stringBuilder.untab), v)
      }

    def enter(): CodeWriter[A] = prevState => {
      val (nextState, v) = thisWriter(prevState)
      (nextState.copy(stringBuilder = nextState.stringBuilder.enter()), v)
    }

    def enterIf(cond: Boolean): CodeWriter[A] = prevState => {
      val (nextState, v) = thisWriter(prevState)
      (if (cond) nextState.copy(stringBuilder = nextState.stringBuilder.enter()) else nextState, v)
    }

    def forceIndent(indent: Int): CodeWriter[A] = prevState => {
      val (state, v) = thisWriter(prevState)
      (state.copy(stringBuilder = state.stringBuilder.set(indent)), v)
    }

    def recoverIndent(): CodeWriter[A] =
      prevState => {
        val (state, v) = thisWriter(prevState)
        (state.copy(stringBuilder = state.stringBuilder.unset), v)
      }

    def ||(x: CodeWriter[A]): CodeWriter[A] = orElse(x)

    def orElse(otherWriter: CodeWriter[A]): CodeWriter[A] = prevState => {
      val (nextState, ret) = thisWriter(prevState)
      ret match {
        case Right(_) => (nextState, ret)
        case Left(_: RecoverableException) => otherWriter(prevState)
        case Left(e: UnrecoverableException) => throw e
        case _ => throw new RuntimeException()
      }
    }

    def collect(tokens: List[JavaSToken]): String = {
      val (CodeWriterState(_, sb, _), _) = thisWriter.run(tokens, IndentAwareStringBuilder(0), Nil)
      sb.toString
    }

  }

  def pure[A](value: A): CodeWriter[A] = state => {
    (state, Right(value))
  }

  case class CodeWriterState(tokens: List[JavaSToken], stringBuilder: IndentAwareStringBuilder, stack: List[String])

  implicit def nextCodeWriterMonadConversion[A](nextCodeWriterMonad: NextCodeWriterMonad[A]): CodeWriter[A] = {

    case CodeWriterState(tokens, stringBuilder, stack) => nextCodeWriterMonad(tokens).run(tokens, stringBuilder, stack)
  }

  object NextCodeWriterMonad {
    def apply[A](f: List[JavaSToken] => CodeWriter[A]): NextCodeWriterMonad[A] = f
  }

}
