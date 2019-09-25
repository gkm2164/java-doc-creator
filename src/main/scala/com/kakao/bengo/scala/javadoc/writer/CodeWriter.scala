package com.kakao.bengo.scala.javadoc.writer

import cats.data.State
import com.kakao.bengo.scala.functional.Monad
import com.kakao.bengo.scala.javadoc.JavaSToken

object CodeWriter {
  type CodeWriter[A] = State[(List[JavaSToken], IndentAwareStringBuilder), A]
  type NextCodeWriterMonad[A] = List[JavaSToken] => CodeWriter[A]

  implicit val codeWriterMonad: Monad[CodeWriter] = new Monad[CodeWriter] {
    override def flatMap[A, B](ma: CodeWriter[A])(f: A => CodeWriter[B]): CodeWriter[B] =
      State(state => {
        val (nextState, ret) = ma.run(state).value
        f(ret).run(nextState).value
      })

    override def unit[A](x: A): CodeWriter[A] = State(state => (state, x))
  }

  implicit class CodeWriterExt[A](codeWriterMonad: CodeWriter[A]) {
    def tell(something: String): CodeWriter[A] = {
      println(s"tell: [$something]")
      State(state => {
        val ((nextState, sb), v) = codeWriterMonad.run(state).value
        ((nextState, sb.append(something)), v)
      })
    }

    def tab(): CodeWriter[A] = {
      State(state => {
        val ((nextState, sb), v) = codeWriterMonad.run(state).value
        ((nextState, sb.tab), v)
      })
    }

    def untab(): CodeWriter[A] = {
      State(state => {
        val ((nextState, sb), v) = codeWriterMonad.run(state).value
        ((nextState, sb.untab), v)
      })
    }

    def enter(): CodeWriter[A] = {
      State(state => {
        val ((nextState, sb), v) = codeWriterMonad.run(state).value
        ((nextState, sb.enter()), v)
      })
    }

    def enterIf(cond: Boolean): CodeWriter[A] = {
      State(state => {
        val ((nextState, sb), v) = codeWriterMonad.run(state).value
        val nextSb = if (cond) sb.enter() else sb
        ((nextState, nextSb), v)
      })
    }

    def forceIndent(indent: Int): CodeWriter[A] = {
      State(state => {
        val ((nextState, sb), v) = codeWriterMonad.run(state).value
        ((nextState, sb.set(indent)), v)
      })
    }

    def recoverIndent(): CodeWriter[A] =
      State(state => {
        val ((nextState, sb), v) = codeWriterMonad.run(state).value
        ((nextState, sb.unset), v)
      })

    def orElse(x: CodeWriter[A]): CodeWriter[A] =
      State(state => {
        try {
          codeWriterMonad.run(state).value
        } catch {
          case _: NotIdentifiedYetException =>  x.run(state).value
        }
      })

    def ||(x: CodeWriter[A]): CodeWriter[A] = orElse(x)

    def collect(tokens: List[JavaSToken]): String =
      codeWriterMonad.run((tokens, new IndentAwareStringBuilder(0))).value._1._2.toString()

    def execute(tokens: List[JavaSToken]): IndentAwareStringBuilder => ((List[JavaSToken], IndentAwareStringBuilder), A) = sb => {
      codeWriterMonad.run((tokens, sb)).value
    }

    def executeA(tokens: List[JavaSToken]): IndentAwareStringBuilder => A =
      sb => codeWriterMonad.run((tokens, sb)).value._2
  }

  def pure[A](value: A): CodeWriter[A] = State(state => (state, value))

  def apply[A](f: List[JavaSToken] => (List[JavaSToken], A)): CodeWriter[A] = State(state => {
    val (tokens, writer) = state
    println(tokens)
    val (nextState, value) = f(tokens)
    ((nextState, writer), value)
  })

  implicit def nextCodeWriterMonadConversion[A](nextCodeWriterMonad: NextCodeWriterMonad[A]): CodeWriter[A] =
    State { case (tokens, sb) => nextCodeWriterMonad(tokens).run(tokens, sb).value }

  object NextCodeWriterMonad {
    def apply[A](f: List[JavaSToken] => CodeWriter[A]): NextCodeWriterMonad[A] = f
  }
}