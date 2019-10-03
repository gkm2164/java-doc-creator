package co.gyeongmin.lang.javadoc.codeformatter

import co.gyeongmin.lang.javadoc.JavaSToken
import co.gyeongmin.lang.javadoc.codeformatter.exceptions._
import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javalang.JavaTokenEnum

import scala.language.implicitConversions

package object syntax {

  implicit class CodeWriterExt[A](thisWriter: CodeWriter[A]) {
    def doSomething(x: => Unit): CodeWriter[A] = {
      x
      thisWriter
    }

    def matchDebug(tag: String, msg: String = ""): CodeWriter[A] = prevState => {
      val (CodeWriterState(nextTokenList, newSb, newStack, config), v) = thisWriter(prevState)

      config.debug match {
        case Some(debugOption) =>
          val sb = StringBuilder.newBuilder
          sb ++= "match"
          if (nextTokenList.nonEmpty) {
            val (JavaSToken(tokenType, value), idx) = nextTokenList.head
            sb ++= s""" $idx|$tokenType|"$value"| with"""
          }
          if (debugOption.stackTrace) {
            sb ++= s" [${newStack.mkString("/")}]"
          }

          if (msg.nonEmpty) {
            sb ++= s": $msg"
          }

          sb ++= s"[${newSb.debugStringBuilder(5)}]"
          v match {
            case Right(_) => println(sb.toString)
            case _ =>
          }
        case None =>
      }

      (CodeWriterState(nextTokenList, newSb, newStack, config), v)
    }

    def hint(tokens: JavaTokenEnum*): CodeWriter[A] = commonHint(tokens, x => tokens.contains(x))

    private def commonHint(tokens: Seq[JavaTokenEnum], pred: JavaTokenEnum => Boolean): CodeWriter[A] = prevState => {
      prevState.tokens match {
        case Nil => (prevState, Left(new TokenListEmptyError))
        case (JavaSToken(v, _), _) :: _ if pred(v) => thisWriter(prevState)
        case tokenList@(JavaSToken(v, _), _) :: _ => (prevState,
          Left(TokenNotAllowedError(s"token $v is not allowed, but should be one of ${tokens.mkString("/")}", tokenList)))
      }
    }

    def notHint(tokens: JavaTokenEnum*): CodeWriter[A] = commonHint(tokens, x => !tokens.contains(x))

    def pushTag(tag: String): CodeWriter[A] = {
      case CodeWriterState(prevTokenList, prevSb, prevStack, config) =>
        val (token, idx) = prevTokenList.head
        val actualTag = CodeWriterStackElem(idx, token, tag)
        val sb: StringBuilder = StringBuilder.newBuilder

        config.debug match {
          case Some(debugOption) =>
            sb ++= s"[${actualTag.toString}]"
            if (debugOption.stackTrace) {
              sb ++= s" <- [${prevStack.take(debugOption.maxStackSize).mkString(" / ")}]"
              println(f"${" " * prevStack.length}RUN    ${prevStack.length + 1}%3d ${sb.toString}")
            }

          case _ =>
        }

        val (CodeWriterState(nextTokenList, newSb, _, _), v) =
          thisWriter.run(prevTokenList, prevSb, actualTag :: prevStack, config)

        if (config.debug.isDefined) println(v match {
          case Right(_) => f"${" " * prevStack.length}ACCEPT ${prevStack.length + 1}%3d ${sb.toString}"
          case Left(_) =>  f"${" " * prevStack.length}REJECT ${prevStack.length + 1}%3d ${sb.toString}"
        })
        (CodeWriterState(nextTokenList, newSb, prevStack, config), v)
    }

    def tell(lit: String): CodeWriter[A] = prevState => {
      val (CodeWriterState(nextState, newSb, newStack, config), v) = thisWriter(prevState)
      v match {
        case Right(_) => (CodeWriterState(nextState, newSb.append(lit), newStack, config), v)
        case Left(e) => (CodeWriterState(nextState, newSb, newStack, config), Left(e))
      }
    }

    def print(fmt: A => String = x => x.toString): CodeWriter[Unit] = prevState => {
      val (nextState, v) = thisWriter(prevState)
      v match {
        case Right(str) => (nextState.copy(stringBuilder = nextState.stringBuilder.append(fmt(str))), Right())
        case Left(e) => (nextState, Left(e))
      }
    }

    def tab(): CodeWriter[A] = prevState => {
      val (nextState, v) = thisWriter(prevState)
      (nextState.copy(stringBuilder = nextState.stringBuilder.tab), v)
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
        case Left(_: RecoverableError) => otherWriter(prevState)
        case Left(e: UnrecoverableError) => throw e.asJavaException
        case _ => throw new RuntimeException()
      }
    }

    def collect(tokens: List[(JavaSToken, Int)], config: CodeWriterConfig): String = {
      val (CodeWriterState(_, sb, stack, _), v) = thisWriter.run(tokens, IndentAwareStringBuilder(0), Nil, config)
      v match {
        case Right(_) => sb.toString
        case Left(e) => s"Error occurred during parse code: $e, lastStack: ${stack.take(5)}"
      }
    }

    def run(state: List[(JavaSToken, Int)],
            stringBuilder: IndentAwareStringBuilder,
            stack: List[CodeWriterStackElem],
            config: CodeWriterConfig): CodeWriterValue[A] = thisWriter(CodeWriterState(state, stringBuilder, stack, config))

  }
}