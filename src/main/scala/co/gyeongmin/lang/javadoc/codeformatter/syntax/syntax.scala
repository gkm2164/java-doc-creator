package co.gyeongmin.lang.javadoc.codeformatter

import cats.syntax.functor._
import cats.syntax.flatMap._
import co.gyeongmin.lang.javadoc.JavaSToken
import co.gyeongmin.lang.javadoc.codeformatter.JavaCodeFormatter.{
  ErrorMessage,
  ParseError
}
import co.gyeongmin.lang.javadoc.codeformatter.exceptions._
import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javalang.JavaTokenEnum

import scala.language.implicitConversions

package object syntax {

  implicit class CodeWriterExt[A](thisWriter: CodeWriter[A]) {
    private def commonHint(
      tokens: Set[JavaTokenEnum],
      pred: JavaTokenEnum => Boolean
    ): CodeWriter[A] = prevState => {
      prevState.tokens match {
        case Nil                                   => (prevState, Left(new TokenListEmptyError))
        case (JavaSToken(v, _), _) :: _ if pred(v) => thisWriter(prevState)
        case tokenList @ (JavaSToken(v, _), _) :: _ =>
          (
            prevState,
            Left(
              TokenNotAllowedError(
                s"token $v is not allowed, but should be one of ${tokens.mkString("/")}",
                tokenList
              )
            )
          )
      }
    }

    def hint(tokens: Set[JavaTokenEnum]): CodeWriter[A] =
      commonHint(tokens, x => tokens.contains(x))

    def hint(token: JavaTokenEnum): CodeWriter[A] = hint(Set(token))

    def notHint(tokens: Set[JavaTokenEnum]): CodeWriter[A] =
      commonHint(tokens, x => !tokens.contains(x))

    def notHint(token: JavaTokenEnum): CodeWriter[A] = notHint(Set(token))

    def pushTag(tag: String): CodeWriter[A] = {
      case state @ CodeWriterState(Nil, _, _, _) =>
        (state, Left(TokenListEmptyError()))
      case CodeWriterState(prevTokenList, prevSb, prevStack, config) =>
        val (token, idx) = prevTokenList.head
        val actualTag = CodeWriterStackElem(idx, token, tag)
        val sb: StringBuilder = new StringBuilder()

        if (config.debug.stackTrace) {
          sb ++= s"[${actualTag.toString}]"
          sb ++= s" <- [${prevStack.take(config.debug.maxStackSize).mkString(" / ")}]"
          config.println(
            f"${" " * prevStack.length}RUN    ${prevStack.length + 1}%3d ${sb.toString}"
          )
        }

        val (CodeWriterState(nextTokenList, newSb, _, _), v) =
          thisWriter.run(prevTokenList, prevSb, actualTag :: prevStack, config)

        if (config.debug.stackTrace) {
          v match {
            case Right(_) =>
              config.println(
                f"${" " * prevStack.length}ACCEPT ${prevStack.length + 1}%3d ${sb.toString}"
              )
            case Left(_) =>
              if (!config.debug.printOnlyAccepted)
                config.println(
                  f"${" " * prevStack.length}REJECT ${prevStack.length + 1}%3d ${sb.toString}"
                )
          }
        }
        (CodeWriterState(nextTokenList, newSb, prevStack, config), v)
    }

    def foldable: CodeWriter[A] = for {
      _ <- CodeWriter.pure(()).tell("""<a class="foldable-block">""")
      res <- thisWriter
      _ <- CodeWriter.pure(()).tell("</a>")
    } yield res

    def tell(lit: String): CodeWriter[A] = prevState => {
      val (CodeWriterState(nextState, newSb, newStack, config), v) = thisWriter(
        prevState
      )
      v match {
        case Right(_) =>
          (CodeWriterState(nextState, newSb.append(lit), newStack, config), v)
        case Left(e) =>
          (CodeWriterState(nextState, newSb, newStack, config), Left(e))
      }
    }

    def print(fmt: A => String = x => x.toString): CodeWriter[Unit] =
      prevState => {
        val (nextState, v) = thisWriter(prevState)
        v match {
          case Right(str) =>
            (
              nextState.copy(stringBuilder =
                nextState.stringBuilder.append(fmt(str))
              ),
              Right(())
            )
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
      (
        if (cond)
          nextState.copy(stringBuilder = nextState.stringBuilder.enter())
        else nextState,
        v
      )
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

    // either의 recoverWith을 쓸 수 있을까?
    def orElse(otherWriter: CodeWriter[A]): CodeWriter[A] = prevState => {
      val (nextState, ret) = thisWriter(prevState)
      ret match {
        case Right(_)                  => (nextState, ret)
        case Left(_: RecoverableError) => otherWriter(prevState)
        case Left(e: UnrecoverableError) =>
          prevState.config.printConsole()
          (prevState, Left(e))
        case _ => throw new RuntimeException()
      }
    }

    def ~(x: CodeWriter[A]): CodeWriter[Unit] = for {
      _ <- thisWriter
      _ <- x
    } yield ()

    def collect(
      tokens: List[(JavaSToken, Int)],
      config: CodeWriterConfig
    ): Either[ParseError, String] = {
      val (CodeWriterState(_, sb, stack, cfg), v) =
        thisWriter.run(tokens, IndentAwareStringBuilder(0), Nil, config)
      v match {
        case Right(_) =>
          if (config.debug.stackTrace) config.printConsole()
          Right(sb.toString)
        case Left(e) =>
          val errMessage =
            s"Error occurred during parse code: $e, lastStack: ${stack.take(5)}"

          if (!config.debug.stackTrace) {
            val tmpState = CodeWriterState(
              tokens,
              IndentAwareStringBuilder(0),
              Nil,
              cfg.copy(debug = cfg.debug.copy(stackTrace = true))
            )
            val (s, _) = thisWriter(tmpState)
            s.config.printConsole()
          }

          Left(ErrorMessage(errMessage, config.printStacktraceString()))
      }
    }

    def run(
      state: List[(JavaSToken, Int)],
      stringBuilder: IndentAwareStringBuilder,
      stack: List[CodeWriterStackElem],
      config: CodeWriterConfig
    ): CodeWriterValue[A] = thisWriter(
      CodeWriterState(state, stringBuilder, stack, config)
    )

  }

}
