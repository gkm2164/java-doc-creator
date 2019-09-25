package com.kakao.bengo.scala.javadoc.writer

import com.kakao.bengo.godoc.exceptions.{TokenNoMoreToConsumeException, TokenNotAcceptedException}
import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.javalang.JavaTokenEnum._
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.writer.CodeWriter._

object JavaCodeFormatter {

  def printCode(tokens: Vector[JavaSToken]): Unit = {
    def fail: CodeWriter[Unit] = CodeWriter { tokens => throw new UnableToHandleException(tokens) }

    def none: CodeWriter[Unit] = CodeWriter.pure(())

    def empty: CodeWriter[Unit] = CodeWriter { tokens => if (tokens.isEmpty) (Nil, ()) else throw new NotEmptyException() }

    def consumeToken(enum: JavaTokenEnum): CodeWriter[JavaSToken] = CodeWriter {
      case Nil => throw new TokenNotAcceptedException(s"'tokens' is empty")
      case (st@JavaSToken(v, _)) +: t if v == enum => (t, st)
      case JavaSToken(v, _) +: _ => throw new TokenNotAcceptedException(s"not allow $v, but $enum")
    }

    def assertToken(enum: JavaTokenEnum): CodeWriter[Unit] = CodeWriter {
      case Nil => throw new TokenNotAcceptedException(s"'tokens' is empty")
      case JavaSToken(v, _) +: t if v == enum => (t, ())
      case JavaSToken(v, _) +: _ => throw new TokenNotAcceptedException(s"not allow $v, but $enum")
    }

    def statements: CodeWriter[Unit] = empty || (for {
      _ <- statement
      _ <- statements || none
    } yield ())

    def statement: CodeWriter[Unit] = for {
      _ <- blockStmt(true) || emptyStmt || expressionStmt || switchStmt ||
        doStmt || breakStmt || continueStmt || returnStmt ||
        forStmt || ifStmt || whileStmt ||
        synchronizedStmt || throwStmt || tryStmt || fail
    } yield ()

    def unaryStmt(token: JavaTokenEnum): CodeWriter[Unit] = for {
      _ <- consumeToken(token).tell(token.value)
      _ <- consumeToken(SEMICOLON).tell(";").enter()
    } yield ()

    def expressionStmt: CodeWriter[Unit] = ???

    def continueStmt: CodeWriter[Unit] = unaryStmt(CONTINUE)

    def breakStmt: CodeWriter[Unit] = unaryStmt(BREAK)

    def throwStmt: CodeWriter[Unit] = for {
      _ <- consumeToken(THROW).tell("throw ")
      _ <- expression
    } yield ()

    def emptyStmt: CodeWriter[Unit] = for {
      _ <- consumeToken(SEMICOLON).tell(";").enter()
    } yield ()

    def tryStmt: CodeWriter[Unit] = for {
      _ <- consumeToken(TRY).tell("try ")
      _ <- blockStmt(true)
      _ <- catchStmts
      _ <- finallyStmt
      _ <- statements
    } yield ()

    def catchStmts: CodeWriter[Unit] = for {
      _ <- consumeToken(CATCH).tell("catch")
      _ <- consumeToken(LEFT_PARENTHESIS).tell("(")
      _ <- declaration
      _ <- consumeToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt(false)
      _ <- catchStmts || none
    } yield ()

    def blockStmt(enterAtEndline: Boolean): CodeWriter[Unit] = for {
      _ <- consumeToken(LBRACE).tell("{").tab().enter()
      _ <- blockStmts || none
      _ <- consumeToken(RBRACE).untab().enter().tell("}").enterIf(enterAtEndline)
    } yield ()

    def blockStmts: CodeWriter[Unit] = for {
      _ <- declaration || statement
      _ <- blockStmts || none
    } yield ()

    def primitiveTypes: CodeWriter[String] = for {
      value <- consumeToken(PRIMITIVE_BYTE).tell("byte") ||
        consumeToken(PRIMITIVE_CHAR).tell("char") ||
        consumeToken(PRIMITIVE_SHORT).tell("short") ||
        consumeToken(PRIMITIVE_LONG).tell("long") ||
        consumeToken(PRIMITIVE_INT).tell("int") ||
        consumeToken(PRIMITIVE_FLOAT).tell("float") ||
        consumeToken(PRIMITIVE_DOUBLE).tell("double") ||
        consumeToken(PRIMITIVE_BOOLEAN).tell("boolean")
    } yield value

    def finallyStmt: CodeWriter[Unit] = for {
      _ <- consumeToken(FINALLY)
      _ <- blockStmt(true)
    } yield ()

    def returnStmt: CodeWriter[Unit] = for {
      _ <- consumeToken(RETURN)
      _ <- expressionStmt
    } yield ()

    def forStmt: CodeWriter[Unit] = ???

    def ifStmt: CodeWriter[Unit] = ???

    def whileStmt: CodeWriter[Unit] = ???

    def doStmt: CodeWriter[Unit] = ???

    def switchStmt: CodeWriter[Unit] = ???

    def expression: CodeWriter[Unit] = lambda || assignment

    def lambda: CodeWriter[Unit] = ???

    def assignment: CodeWriter[Unit] = ???

    def synchronizedStmt: CodeWriter[Unit] = for {
      _ <- consumeToken(SYNCHRONIZED).tell("synchronized")
      _ <- consumeToken(LEFT_PARENTHESIS).tell("(")
      _ <- expression
      _ <- consumeToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt(true)
    } yield ()

    def declaration: CodeWriter[Unit] = for {
      _ <- assertToken(FINAL).tell("final ") || none
      _ <- declDetail
    } yield ()

    def declDetail: CodeWriter[Unit] = for {
      _ <- primitiveTypes || (for {
        _ <- typeDef
        _ <- generic
      } yield ())
      _ <- arrayUse || none
      _ <- identifier
      _ <- variableInitialize || none
      _ <- (for {
        _ <- assertToken(COMMA)
        _ <- declDetail
      } yield ()) || none
      _ <- assertToken(SEMICOLON)
    } yield ()

    def typeDef: CodeWriter[Unit] = ???

    def variableInitialize: CodeWriter[Unit] = for {
      _ <- assertToken(SUBSTITUTE)
      _ <- expression || arrayInitializer
    } yield ()

    def arrayInitializer: CodeWriter[Unit] = for {
      _ <- assertToken(LBRACE).tell("{")
      _ <- tokenSeparatedCtx(variableInitialize, COMMA) || none
      _ <- assertToken(RBRACE).tell("}")
    } yield ()

    def generic: CodeWriter[Unit] = for {
      _ <- consumeToken(LT).tell("<")
      _ <- tokenSeparatedCtx(identifier, COMMA)
      _ <- consumeToken(GT).tell(">")
    } yield ()

    def arrayUse: CodeWriter[Unit] = for {
      _ <- consumeToken(LBRACKET)
      _ <- consumeToken(RBRACKET).tell("[]")
      _ <- arrayUse || none
    } yield ()

    def identifier: CodeWriter[Unit] = consumeToken(TOKEN).map(x => x.value)

    def tokenSeparatedCtx(chosenParser: CodeWriter[Unit], enum: JavaTokenEnum): CodeWriter[Unit] = for {
      _ <- chosenParser
      _ <- (for {
        _ <- consumeToken(enum)
        _ <- tokenSeparatedCtx(chosenParser, enum)
      } yield ()) || none
    } yield ()

    println(blockStmt(false).collect(tokens.toList))
  }

  case class IndentCounter(indent: Int) {
    def addIndent(): IndentCounter = IndentCounter(indent + 1)

    def decIndent(): IndentCounter = IndentCounter(indent - 1)

    def printIndent(sb: StringBuilder = StringBuilder.newBuilder): String = sb.append("    " * indent).toString()
  }

  case class PrintState(remains: Vector[JavaSToken], indent: IndentCounter) {
    def addIndent(): PrintState = copy(indent = indent.addIndent())

    def decIndent(): PrintState = copy(indent = indent.decIndent())

    def printIndent(): String = indent.printIndent()
  }

}

class UnableToHandleException(tokens: List[JavaSToken])
  extends Exception(s"there is no way to interprete this code ${tokens.map(x => x.tokenType.toString).mkString(", ")}.") {

}

class NotEmptyException() extends Exception
