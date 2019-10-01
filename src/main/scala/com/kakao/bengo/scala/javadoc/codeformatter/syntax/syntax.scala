package com.kakao.bengo.scala.javadoc.codeformatter

import com.kakao.bengo.godoc.exceptions.TokenNotAcceptedException
import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.codeformatter.exceptions.{RecoverableException, TokenListEmptyException, TokenNotAllowedException, UnrecoverableException}
import com.kakao.bengo.scala.javadoc.codeformatter.monad._

import scala.language.implicitConversions

package object syntax {
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

      v match {
        case Right(_) => println(sb.toString)
        case _ =>
      }
      (CodeWriterState(nextTokenList, newSb, newStack), v)
    }

    private def commonHint(tokens: Seq[JavaTokenEnum], pred: JavaTokenEnum => Boolean): CodeWriter[A] = prevState => {
      prevState.tokens match {
        case Nil => (prevState, Left(new TokenListEmptyException))
        case JavaSToken(v, _) :: _ if pred(v) => thisWriter(prevState)
        case tokenList@JavaSToken(v, _) :: _ => (prevState,
          Left(new TokenNotAllowedException(s"token $v is not allowed, but should be one of ${tokens.mkString("/")}", tokenList)))
      }
    }

    def hint(tokens: JavaTokenEnum*): CodeWriter[A] = commonHint(tokens, x => tokens.contains(x))
    def notHint(tokens: JavaTokenEnum*): CodeWriter[A] = commonHint(tokens, x => !tokens.contains(x))

    def pushTag(tag: String): CodeWriter[A] = {
      case CodeWriterState(prevTokenList, prevSb, prevStack) =>
        val JavaSToken(enum, value) = prevTokenList.head
        val actualTag = s"$tag[$enum($value)]"
        val (CodeWriterState(nextTokenList, newSb, _), v) = thisWriter.run(prevTokenList, prevSb, actualTag :: prevStack)

        (CodeWriterState(nextTokenList, newSb, prevStack), v)
    }

    def tell(something: String): CodeWriter[A] = prevState => {
      val (CodeWriterState(nextState, newSb, newStack), v) = thisWriter(prevState)
      v match {
        case Right(_) => (CodeWriterState(nextState, newSb.append(something), newStack), v)
        case Left(e) => (CodeWriterState(nextState, newSb, newStack), v)
      }
    }

    def print(fmt: A => String = x => x.toString): CodeWriter[Unit] = prevState => {
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

    def run(state: List[JavaSToken], stringBuilder: IndentAwareStringBuilder, stack: List[String]): CodeWriterValue[A] = thisWriter(CodeWriterState(state, stringBuilder, stack))

  }


  implicit def nextCodeWriterMonadConversion[A](nextCodeWriterMonad: NextCodeWriterMonad[A]): CodeWriter[A] = {

    case CodeWriterState(tokens, stringBuilder, stack) => nextCodeWriterMonad(tokens).run(tokens, stringBuilder, stack)
  }

  object NextCodeWriterMonad {
    def apply[A](f: List[JavaSToken] => CodeWriter[A]): NextCodeWriterMonad[A] = f
  }
}
