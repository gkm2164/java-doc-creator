package com.kakao.bengo.scala.javadoc.writer

import cats.data.State
import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.javalang.JavaTokenEnum._
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.writer.CodeWriter._
import com.kakao.bengo.scala.javadoc.writer.JavaCodeFormatter.{additiveExpression, andExpression, conditionalAndExpression, conditionalOrExpression, equalityExpression}
import com.kakao.bengo.scala.javadoc.writer.exceptions._

object JavaCodeFormatter {
  def printCode(tokens: Vector[JavaSToken]): Unit = {
    println(s"parse ${tokens.map(_.value).mkString(" ")}")
    println(blockStmt(false).collect(tokens.toList))
  }

  def fail[T](reason: String): CodeWriter[T] = CodeWriter { tks => (tks, Left(new ParseFailException(reason))) }

  def none(from: String = ""): CodeWriter[Unit] = {
    println(s"fallback of $from")
    CodeWriter.pure(())
  }

  def consumeTokens(enums: List[JavaTokenEnum]): CodeWriter[JavaSToken] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (st@JavaSToken(v, _)) :: t if enums.contains(v) => (t, Right(st))
    case tokenList@JavaSToken(v, _) :: _ =>
      (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but should one of [${enums.map(_.toString).mkString(", ")}]", tokenList)))
  }

  implicit class EnumSyntax(thisToken: JavaTokenEnum) {
    def |(elem: JavaTokenEnum): List[JavaTokenEnum] = List(thisToken, elem)
  }

  implicit class EnumsSyntax(thisTokens: List[JavaTokenEnum]) {
    def |(elem: JavaTokenEnum): List[JavaTokenEnum] = thisTokens :+ elem
  }

  def assertToken(enum: JavaTokenEnum): CodeWriter[Unit] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case tokenList@JavaSToken(v, _) :: t =>
      if (v == enum) {
        println(s"succeed with: $v")
        (t, Right(()))
      } else {
        println(s"failed with: $v")
        (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but $enum", tokenList)))
      }
    case tokenList => println(tokenList)
      (tokenList, Left(new ParseFailException("unidentified token has arrived")))
  }

  def assertTokens(enums: List[JavaTokenEnum]): CodeWriter[Unit] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case tokenList@JavaSToken(v, _) :: t =>
      if (enums.contains(v)) {
        println(s"succeed with: $v")
        (t, Right(()))
      } else {
        println(s"failed with: $v")
        (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but one of [${enums.mkString(", ")}]", tokenList)))
      }
    case tokenList => println(tokenList)
      (tokenList, Left(new ParseFailException("unidentified token has arrived")))
  }

  def blockStmt(enterAtEndLine: Boolean): CodeWriter[Unit] = for {
    _ <- assertToken(LBRACE).tell("{").tab().enter()
    _ <- blockStmts.debug("try to parse as [blockStmts]") || none("blockStmt/blockStmts")
    _ <- assertToken(RBRACE).untab().enter().tell("}").enterIf(enterAtEndLine)
  } yield Right(())

  def statements: CodeWriter[Unit] = {
    def loop: CodeWriter[Unit] = {
      for {
        _ <- statement
        _ <- loop || none("statements")
      } yield Right()
    }

    loop || none("statements/loop")
  }

  def statement: CodeWriter[Unit] = for {
    _ <- blockStmt(true) || emptyStmt || expressionStmt || switchStmt ||
      doStmt || breakStmt || continueStmt || returnStmt || forStmt || ifStmt ||
      whileStmt || synchronizedStmt || throwStmt || tryStmt || fail("failed at [statement] block")
  } yield Right()

  def unaryStmt(token: JavaTokenEnum): CodeWriter[Unit] = {
    for {
      _ <- assertToken(token).tell(token.value)
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield Right()
  }

  def expressionStmt: CodeWriter[Unit] =
    for {
      _ <- assignment || preExpression || postExpression || methodInvocation || classInstanceCreation
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield Right()

  def assignment: CodeWriter[Unit] = for {
    _ <- identifier
    _ <- arrayRefs || none("assignment/arrayRefs")
    _ <- consumeTokens(SUBSTITUTE | PLUS_ACC | MINUS_ACC | MULTIPLY_ACC | DIVIDE_ACC).tell(" = ")
    _ <- expression
  } yield Right()

  def arrayRefs: CodeWriter[Unit] = for {
    _ <- assertToken(LBRACKET).tell("[")
    _ <- expression
    _ <- assertToken(RBRACKET).tell("]")
    _ <- arrayRefs || none("arrayRefs/arrayRefs")
  } yield Right()

  def preExpression: CodeWriter[Unit] = for {
    _ <- assertTokens(INC | DEC)
    _ <- identifier
  } yield Right()

  def postExpression: CodeWriter[Unit] = for {
    _ <- identifier
    _ <- assertTokens(INC | DEC)
  } yield Right()

  def methodInvocation: CodeWriter[Unit] = for {
    _ <- expression
    _ <- assertToken(LEFT_PARENTHESIS)
    _ <- tokenSeparatedCtx(expression, COMMA) || none("methodInvocation/tokenSeparatedCtx(expression, COMMA)")
    _ <- assertToken(RIGHT_PARENTHESIS)
  } yield Right()

  def classInstanceCreation: CodeWriter[Unit] = for {
    _ <- assertToken(NEW)
    _ <- identifier
    _ <- assertToken(LEFT_PARENTHESIS)
    _ <- tokenSeparatedCtx(expression, COMMA) || none("classInstanceCreation/tokenSeparatedCtx(expression, COMMA")
    _ <- assertToken(RIGHT_PARENTHESIS)
  } yield Right()

  def continueStmt: CodeWriter[Unit] = unaryStmt(CONTINUE)

  def breakStmt: CodeWriter[Unit] = unaryStmt(BREAK)

  def throwStmt: CodeWriter[Unit] = for {
    _ <- assertToken(THROW).tell("throw ")
    _ <- expression
  } yield Right()

  def emptyStmt: CodeWriter[Unit] = for {
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield Right()

  def tryStmt: CodeWriter[Unit] = for {
    _ <- assertToken(TRY).tell("try ")
    _ <- blockStmt(true)
    _ <- catchStmts
    _ <- finallyStmt
    _ <- statements
  } yield Right()

  def catchStmts: CodeWriter[Unit] = for {
    _ <- assertToken(CATCH).tell("catch")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- declaration
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false)
    _ <- catchStmts || none("catchStmts/catchStmts")
  } yield Right(())

  def blockStmts: CodeWriter[Unit] = for {
    _ <- declaration.debug("try to parse as [declaration]") || statement.debug("try to parse as [statement]")
    _ <- blockStmts || none("blockStmts/blockStmts")
  } yield Right(())

  def primitiveTypes: CodeWriter[Unit] = for {
    _ <- assertToken(PRIMITIVE_BYTE).tell("byte ") ||
      assertToken(PRIMITIVE_CHAR).tell("char ") ||
      assertToken(PRIMITIVE_SHORT).tell("short ") ||
      assertToken(PRIMITIVE_LONG).tell("long ") ||
      assertToken(PRIMITIVE_INT).tell("int ") ||
      assertToken(PRIMITIVE_FLOAT).tell("float ") ||
      assertToken(PRIMITIVE_DOUBLE).tell("double ") ||
      assertToken(PRIMITIVE_BOOLEAN).tell("boolean ")
  } yield Right()

  def finallyStmt: CodeWriter[Unit] = for {
    _ <- assertToken(FINALLY)
    _ <- blockStmt(true)
  } yield Right(())

  def returnStmt: CodeWriter[Unit] = for {
    _ <- assertToken(RETURN)
    _ <- expression
  } yield Right(())

  def forStmt: CodeWriter[Unit] = for {
    _ <- assertToken(FOR)
  } yield Right()

  def ifStmt: CodeWriter[Unit] = for {
    _ <- assertToken(IF)
  } yield Right()

  def whileStmt: CodeWriter[Unit] = for {
    _ <- assertToken(WHILE)
    _ <- assertToken(LEFT_PARENTHESIS).tell("while(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    _ <- blockStmt(true)
  } yield Right(())

  def doStmt: CodeWriter[Unit] = for {
    _ <- assertToken(DO).tell("do ")
    _ <- blockStmt(false).tell(" ")
    _ <- assertToken(WHILE)
    _ <- assertToken(LEFT_PARENTHESIS).tell("while(")
    _ <- expressionStmt
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield Right(())

  def switchStmt: CodeWriter[Unit] = for {
    _ <- assertToken(SWITCH).tell("switch ")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- assertToken(LBRACE).tab().enter()
    _ <- caseStmt || none("switchStmt/caseStmt")
    _ <- defaultStmt || none("switchStmt/defaultStmt")
    _ <- assertToken(RBRACE).untab().enter()
  } yield Right(())

  def caseStmt: CodeWriter[Unit] = for {
    _ <- caseStmtDetail
    _ <- caseStmt || none("caseStmt/caseStmt")
  } yield Right(())

  def defaultStmt: CodeWriter[Unit] = for {
    _ <- assertToken(DEFAULT)
    _ <- assertToken(COLON).tell("default:")
    _ <- statements
  } yield Right()

  def caseStmtDetail: CodeWriter[Unit] = for {
    _ <- assertToken(CASE).tell("case ")
    _ <- expression
    _ <- assertToken(COLON).tell(":")
    _ <- statements
  } yield Right()

  def expression: CodeWriter[Unit] = lambda || assignment || conditionalExpression

  def conditionalExpression: CodeWriter[Unit] = for {
    _ <- conditionalOrExpression
    _ <- assertToken(QUESTION_MARK)
    _ <- expression
    _ <- assertToken(COLON)
    _ <- conditionalExpression || lambda
  } yield Right()

  def conditionalOrExpression: CodeWriter[Unit] = for {
    _ <- conditionalAndExpression || (for {
      _ <- conditionalAndExpression
      - <- assertToken(OR)
      _ <- conditionalOrExpression
    } yield Right())
  } yield Right()

  def conditionalAndExpression: CodeWriter[Unit] = for {
    _ <- inclusiveOrExpression || (for {
      _ <- inclusiveOrExpression
      _ <- assertToken(AND)
      _ <- conditionalAndExpression
    } yield Right())
  } yield Right()

  def inclusiveOrExpression: CodeWriter[Unit] = for {
    _ <- exclusiveOrExpression || (for {
      _ <- exclusiveOrExpression
      _ <- assertToken(BIT_OR)
      _ <- inclusiveOrExpression
    } yield Right())
  } yield Right()

  def exclusiveOrExpression: CodeWriter[Unit] = for {
    _ <- andExpression || (for {
      _ <- andExpression
      _ <- assertToken(BIT_XOR)
      _ <- exclusiveOrExpression
    } yield Right())
  } yield Right()

  def andExpression: CodeWriter[Unit] = for {
    _ <- equalityExpression || (for {
      _ <- equalityExpression
      _ <- assertToken(BIT_AND)
      _ <- andExpression
    } yield Right())
  } yield Right()

  def equalityExpression: CodeWriter[Unit] = for {
    _ <- relationalExpression || (for {
      _ <- relationalExpression
      _ <- assertToken(EQUAL)
      _ <- equalityExpression
    } yield Right())
  } yield Right()

  def relationalExpression: CodeWriter[Unit] = for {
    _ <- shiftExpression || (for {
      _ <- shiftExpression
      _ <- assertTokens(LT | GT | LTE | GTE)
      _ <- relationalExpression
    } yield Right()) || (for {
      _ <- shiftExpression
      _ <- assertToken(INSTANCEOF)
      _ <- primitiveTypes || customDecl
    } yield Right())
  } yield Right()

  def shiftExpression: CodeWriter[Unit] = for {
    _ <- additiveExpression || (for {
      _ <- additiveExpression
      _ <- assertTokens(LEFT_SHIFT | RIGHT_SHIFT | U_RIGHT_SHIFT)
      _ <- shiftExpression
    } yield Right())
  } yield Right()

  def additiveExpression: CodeWriter[Unit] = for {
    _ <- multiplicativeExpression || (for {
      _ <- multiplicativeExpression
      _ <- assertTokens(PLUS | MINUS)
      _ <- additiveExpression
    } yield Right())
  } yield Right()

  def multiplicativeExpression: CodeWriter[Unit] = for {
    _ <- unaryExpression || (for {
      _ <- unaryExpression
      _ <- assertTokens(MULTIPLY | DIVIDE | MODULAR)
      _ <- multiplicativeExpression
    } yield Right())
  } yield Right()

  def unaryExpression: CodeWriter[Unit] = for {
    _ <- preExpression || unaryExpWith(PLUS | MINUS | NEGATE | EXCLAMATION_MARK) || postExpression || castExpression
  } yield Right()

  def castExpression: CodeWriter[Unit] = for {
    _ <- assertToken(LEFT_PARENTHESIS)
    _ <- primitiveTypes || customDecl
    _ <- assertToken(RIGHT_PARENTHESIS)
    _ <- unaryExpression || lambda
  } yield Right()

  def unaryExpWith(enums: List[JavaTokenEnum]): CodeWriter[Unit] = for {
    _ <- assertTokens(enums)
    _ <- unaryExpression
  } yield Right()

  def lambda: CodeWriter[Unit] = none("lambda")

  def synchronizedStmt: CodeWriter[Unit] = for {
    _ <- assertToken(SYNCHRONIZED).tell("synchronized")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(true)
  } yield Right()

  def declaration: CodeWriter[Unit] = {
    def declDetail: CodeWriter[Unit] = {
      def loop: CodeWriter[Unit] = for {
        _ <- assertToken(COMMA)
        _ <- declDetail
      } yield Right()

      for {
        _ <- primitiveTypes || customDecl
        _ <- arrayUse || none("declDetail/arrayUse")
        _ <- identifier
        _ <- variableInitialize || none("declDetail/variableInitialize")
        _ <- loop || none("declDetail/primitiveTypes")
        _ <- assertToken(SEMICOLON)
      } yield Right(())
    }

    for {
      _ <- assertToken(FINAL).tell("final").debug("try to assert [FINAL]") ||
        none("declaration/assertToken(FINAL)").debug("there's no final")
      _ <- declDetail
    } yield Right()
  }

  def customDecl: CodeWriter[Unit] = for {
    _ <- identifier.debug("inside [declDetail], [identifier]")
    _ <- generic || none("declDetail/generic")
  } yield Right()

  def variableInitialize: CodeWriter[Unit] = for {
    _ <- assertToken(SUBSTITUTE)
    _ <- expression.debug("came to [expression]") || arrayInitializer
  } yield Right(())

  def arrayInitializer: CodeWriter[Unit] = for {
    _ <- assertToken(LBRACE).tell("{")
    _ <- tokenSeparatedCtx(variableInitialize, COMMA) || none("arrayInitializer/commaSeparatedVariableInitialize")
    _ <- assertToken(RBRACE).tell("}")
  } yield Right(())

  def generic: CodeWriter[Unit] = for {
    _ <- assertToken(LT).tell("<")
    _ <- tokenSeparatedCtx(identifier, COMMA)
    _ <- assertToken(GT).tell(">")
  } yield Right(())

  def arrayUse: CodeWriter[Unit] = for {
    _ <- assertToken(LBRACKET)
    _ <- assertToken(RBRACKET).tell("[]")
    _ <- arrayUse || none("arrayUse/arrayUse")
  } yield Right(())

  def identifier: CodeWriter[Unit] = tokenSeparatedCtx(assertToken(TOKEN), DOT)

  def tokenSeparatedCtx(chosenParser: CodeWriter[Unit], enum: JavaTokenEnum): CodeWriter[Unit] = {
    def loop: CodeWriter[Unit] = for {
      _ <- assertToken(enum).tell(s"${enum.value} ")
      res <- tokenSeparatedCtx(chosenParser, enum)
    } yield res

    for {
      _ <- chosenParser
      _ <- loop || none("tokenSeparatedCtx/loop")
    } yield Right()
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
