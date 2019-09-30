package com.kakao.bengo.scala.javadoc.writer

import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.javalang.JavaTokenEnum._
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.writer.CodeWriter._
import com.kakao.bengo.scala.javadoc.writer.exceptions._

object JavaCodeFormatter {
  def printCode(codeName: String, tokens: Vector[JavaSToken]): Unit = {
    val reformatTokens = tokens.filterNot(x => List(COMMENT_BLOCK, COMMENT, COMMENT_MACRO_EXPLAIN, COMMENT_MACRO_CODE, COMMENT_MACRO_NAME).contains(x.tokenType))
    println(s"parse ${reformatTokens.map(_.value).mkString(" ")}")
    println(blockStmt(false).collect(reformatTokens.toList))
  }

  def fail[T](reason: String): CodeWriter[T] = CodeWriter { tks => (tks, Left(new ParseFailException(reason))) }

  def none: CodeWriter[Unit] = CodeWriter {
    tks => (tks, Right())
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

  def takeToken(enum: JavaTokenEnum): CodeWriter[String] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case JavaSToken(v, str) :: t if v == enum || (enum == TOKEN && v == SUPER) => (t, Right(str))
    case tokenList@JavaSToken(v, _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but $enum", tokenList)))
  }

  def takeTokens(enums: List[JavaTokenEnum]): CodeWriter[String] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case JavaSToken(v, str) :: t if enums.contains(v) => (t, Right(str))
    case tokenList@JavaSToken(v, _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but should be one of ${enums.mkString(", ")}", tokenList)))
  }

  def assertToken(enum: JavaTokenEnum): CodeWriter[Unit] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case tokenList@JavaSToken(v, _) :: t =>
      if (v == enum || (enum == TOKEN && v == SUPER)) {
        (t, Right(()))
      } else {
        (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but $enum", tokenList)))
      }
    case tokenList =>
      (tokenList, Left(new ParseFailException("unidentified token has arrived")))
  }

  def assertTokens(enums: List[JavaTokenEnum]): CodeWriter[Unit] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case tokenList@JavaSToken(v, _) :: t =>
      if (enums.contains(v)) {
        (t, Right(()))
      } else {
        (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but one of [${enums.mkString(", ")}]", tokenList)))
      }
    case tokenList =>
      (tokenList, Left(new ParseFailException("unidentified token has arrived")))
  }

  def blockStmt(enterAtEndLine: Boolean): CodeWriter[Unit] = for {
    _ <- assertToken(LBRACE).tell("{").enter().tab()
    _ <- blockStmts.matchDebug("blockStmts", "blockStmt") || none.matchDebug("none", "blockStmts", "not blockStmts")
    _ <- assertToken(RBRACE).untab().tell("}").enterIf(enterAtEndLine)
  } yield Right()

  def statements: CodeWriter[Unit] = {
    def loop: CodeWriter[Unit] = {
      for {
        _ <- statement
        _ <- loop || none.matchDebug("none", "statements/loop", "not loop")
      } yield Right()
    }

    loop || none.matchDebug("none", "statements", "not loop")
  }

  def statement: CodeWriter[Unit] = for {
    _ <- blockStmt(true).matchDebug("blockStmt", "statement") || emptyStmt ||
      expressionStmt.matchDebug("expressionStmt", "statement") || switchStmt ||
      doStmt || breakStmt || continueStmt || returnStmt || forStmt.matchDebug("forStmt", "statement") || ifStmt ||
      whileStmt || synchronizedStmt || throwStmt || tryStmt || declarationStmt
  } yield Right()

  def unaryStmt(token: JavaTokenEnum): CodeWriter[Unit] = {
    for {
      _ <- assertToken(token).tell(token.value)
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield Right()
  }

  def expressionStmt: CodeWriter[Unit] =
    for {
      _ <- assignment.matchDebug("assignment", "expressionStmt") || expression
      _ <- assertToken(SEMICOLON).tell(";").enter().printDebug()
    } yield Right()

  def assignment: CodeWriter[Unit] = for {
    _ <- identifier.matchDebug("identifier", "assignment")
    _ <- arrayRefs.matchDebug("arrayRefs", "assignment") || none.matchDebug("none", "assignment", "not arrayRefs")
    testTokens = SUBSTITUTE | PLUS_ACC | MINUS_ACC | MULTIPLY_ACC | DIVIDE_ACC
    _ <- takeTokens(testTokens).print(x => s" $x ").matchDebug(s"takeTokens($testTokens).print", "assignment")
    _ <- expression.matchDebug("expression", "assignment")
  } yield Right()

  def arrayRefs: CodeWriter[Unit] = for {
    _ <- assertToken(LBRACKET).tell("[")
    _ <- expression
    _ <- assertToken(RBRACKET).tell("]")
    _ <- arrayRefs || none.matchDebug("none", "arrayRefs", "not arrayRefs")
  } yield Right()

  def preExpression: CodeWriter[Unit] = for {
    _ <- takeTokens(INC | DEC).print(x => x)
    _ <- identifier
  } yield Right()

  def postExpression: CodeWriter[Unit] = for {
    _ <- identifier
    _ <- takeTokens(INC | DEC).print(x => x)
  } yield Right()

  def methodInvocation: CodeWriter[Unit] = for {
    _ <- reference
    _ <- assertToken(LEFT_PARENTHESIS)
    _ <- tokenSeparatedCtx(expression, COMMA, requireSpace = true) || none.matchDebug("none", "methodInvocation", "not tokenSeparatedCtx(expression, COMMA)")
    _ <- assertToken(RIGHT_PARENTHESIS)
  } yield Right()

  def classInstanceCreation: CodeWriter[Unit] = {
    def arrayInstance: CodeWriter[Unit] = for {
      _ <- assertToken(LBRACKET).tell("[")
      _ <- expression || none
      _ <- assertToken(RBRACKET).tell("]")
      _ <- arrayInitializer || none
    } yield Right()

    def classInstance: CodeWriter[Unit] = for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- tokenSeparatedCtx(expression, COMMA, requireSpace = true) || none.matchDebug("none", "classInstanceCreation", "not tokenSeparatedCtx(expression, COMMA)")
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    } yield Right()

    for {
      _ <- assertToken(NEW).tell("new ")
      _ <- primitiveTypes || identifier
      _ <- generic || none.matchDebug("none", "classInstanceCreation", "not generic")
      _ <- arrayInstance || classInstance
    } yield Right()
  }

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
    _ <- catchStmts || none.matchDebug("none", "catchStmts", "not catchStmts")
  } yield Right()

  def blockStmts: CodeWriter[Unit] = for {
    _ <- statement.matchDebug("statement", "blockStmts")
    _ <- blockStmts || none.matchDebug("none", "blockStmts", "not blockStmts")
  } yield Right()

  def primitiveTypes: CodeWriter[Unit] = for {
    _ <- assertToken(PRIMITIVE_BYTE).tell("byte") ||
      assertToken(PRIMITIVE_CHAR).tell("char") ||
      assertToken(PRIMITIVE_SHORT).tell("short") ||
      assertToken(PRIMITIVE_LONG).tell("long") ||
      assertToken(PRIMITIVE_INT).tell("int") ||
      assertToken(PRIMITIVE_FLOAT).tell("float") ||
      assertToken(PRIMITIVE_DOUBLE).tell("double") ||
      assertToken(PRIMITIVE_BOOLEAN).tell("boolean")
  } yield Right()

  def finallyStmt: CodeWriter[Unit] = for {
    _ <- assertToken(FINALLY)
    _ <- blockStmt(true)
  } yield Right(())

  def returnStmt: CodeWriter[Unit] = for {
    _ <- assertToken(RETURN).tell("return ")
    _ <- expression
  } yield Right(())

  def forStmt: CodeWriter[Unit] = {
    def forConditionStmt: CodeWriter[Unit] = {
      def triExpressions: CodeWriter[Unit] = for {
        _ <- declaration || expression || none.matchDebug("none", "forStmt/forConditionStmt/triExpression", "neither declaration nor expression")
        _ <- assertToken(SEMICOLON).tell("; ")
        _ <- conditionalExpression
        _ <- assertToken(SEMICOLON).tell("; ")
        _ <- expression || none
      }yield Right()

      def simpleTypes: CodeWriter[Unit] = for {
        _ <- customDecl.tell(" ")
        _ <- identifier
        _ <- assertToken(COLON).tell(": ")
        _ <- expression
      } yield Right()

      triExpressions || simpleTypes
    }

    for {
      _ <- assertToken(FOR).tell("for ").matchDebug("assertToken(FOR).tell", "forStmt")
      _ <- assertToken(LEFT_PARENTHESIS).tell("(").matchDebug("assertToken(LEFT_PARENTHESIS).tell", "forStmt")
      _ <- forConditionStmt.matchDebug("forConditionStmt", "forStmt")
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ").matchDebug("assertToken(RIGHT_PARENTHESIS).tell", "forStmt")
      _ <- (blockStmt(true) || statement).matchDebug("(blockStmt || statement)", "forStmt")
    } yield Right()
  }

  def ifStmt: CodeWriter[Unit] = for {
    _ <- assertToken(IF).tell("if ")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false).tell(" ") || statement
    _ <- elseIfStmts || none.matchDebug("none", "ifStmt", "not elseIfStmts")
    _ <- elseStmt || none.matchDebug("none", "ifStmt", "not elseStmt")
    _ <- none.enter()
  } yield Right()

  def elseIfStmts: CodeWriter[Unit] = for {
    _ <- assertToken(ELSE)
    _ <- assertToken(IF).tell("else if ")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false).tell(" ") || statement.enter()
    _ <- elseIfStmts || none.matchDebug("none", "elseIfStmts", "not elseIfStmts")
  } yield Right()

  def elseStmt: CodeWriter[Unit] = for {
    _ <- assertToken(ELSE).tell("else ")
    _ <- blockStmt(false) || statement
  } yield Right()

  def whileStmt: CodeWriter[Unit] = for {
    _ <- assertToken(WHILE)
    _ <- assertToken(LEFT_PARENTHESIS).tell("while(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    _ <- blockStmt(true) || statement
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
    _ <- assertToken(LBRACE).tell("{").enter().tab()
    _ <- caseStmts || none.matchDebug("none", "switchStmt", "not caseStmt")
    _ <- defaultStmt || none.matchDebug("none", "switchStmt", "not defaultStmt")
    _ <- assertToken(RBRACE).tell("}").untab().enter()
  } yield Right(())

  def caseStmts: CodeWriter[Unit] = for {
    _ <- caseStmtDetail
    _ <- caseStmts || none.matchDebug("none", "caseStmt", "not caseStmt")
  } yield Right(())

  def defaultStmt: CodeWriter[Unit] = for {
    _ <- assertToken(DEFAULT)
    _ <- assertToken(COLON).tell("default: ")
    _ <- statements
  } yield Right()

  def caseStmtDetail: CodeWriter[Unit] = for {
    _ <- assertToken(CASE).tell("case ")
    _ <- valueTypes || identifier
    _ <- assertToken(COLON).tell(": ")
    _ <- statements
  } yield Right()

  def expression: CodeWriter[Unit] =
    lambda.matchDebug("lambda", "expression") || assignment || conditionalExpression ||
      preExpression || postExpression || classInstanceCreation

  def conditionalExpression: CodeWriter[Unit] = {
    def detail: CodeWriter[Unit] = for {
      _ <- assertToken(QUESTION_MARK).tell(" ? ")
      _ <- expression
      _ <- assertToken(COLON).tell(" : ")
      _ <- conditionalExpression || lambda
    } yield Right()

    for {
      _ <- conditionalOrExpression.matchDebug("conditionalOrExpression", "conditionalExpression")
      _ <- detail || none.matchDebug("none", "conditionalExpression", "not detail")
    } yield Right()
  }

  def infixOperator(operator: JavaTokenEnum, next: CodeWriter[Unit]): CodeWriter[Unit] = {
    def detail: CodeWriter[Unit] = for {
      _ <- takeToken(operator).print(x => s" $x ")
      _ <- infixOperator(operator, next)
    } yield Right()

    for {
      _ <- next
      _ <- detail || none.matchDebug("none", s"infixOperator($operator)", "not detail")
    } yield Right()
  }


  def infixOperators(operators: List[JavaTokenEnum], next: CodeWriter[Unit]): CodeWriter[Unit] = {
    def detail: CodeWriter[Unit] = for {
      _ <- takeTokens(operators).print(x => s" $x ")
      _ <- infixOperators(operators, next)
    } yield Right()

    for {
      _ <- next
      _ <- detail || none.matchDebug("none", s"infixOperator($operators)", "not detail")
    } yield Right()
  }

  def conditionalOrExpression: CodeWriter[Unit] = infixOperator(OR, conditionalAndExpression)

  def conditionalAndExpression: CodeWriter[Unit] = infixOperator(AND, inclusiveOrExpression)

  def inclusiveOrExpression: CodeWriter[Unit] = infixOperator(BIT_OR, exclusiveOrExpression)

  def exclusiveOrExpression: CodeWriter[Unit] = infixOperator(BIT_XOR, andExpression)

  def andExpression: CodeWriter[Unit] = infixOperator(BIT_AND, equalityExpression)

  def equalityExpression: CodeWriter[Unit] = infixOperators(EQUAL | NOT_EQUAL, relationalExpression)

  def relationalExpression: CodeWriter[Unit] = for {
    _ <- infixOperators(LT | GT | LTE | GTE, shiftExpression) || (for {
      _ <- shiftExpression
      _ <- assertToken(INSTANCEOF).tell(" instanceof ")
      _ <- primitiveTypes || customDecl
    } yield Right())
  } yield Right()

  def shiftExpression: CodeWriter[Unit] = infixOperators(LEFT_SHIFT | RIGHT_SHIFT | U_RIGHT_SHIFT, additiveExpression)

  def additiveExpression: CodeWriter[Unit] = infixOperators(PLUS | MINUS, multiplicativeExpression)

  def multiplicativeExpression: CodeWriter[Unit] = infixOperators(MULTIPLY | DIVIDE | MODULAR, unaryExpression)

  def unaryExpression: CodeWriter[Unit] = for {
    _ <- preExpression || unaryExpWith(PLUS | MINUS | NEGATE | EXCLAMATION_MARK) ||
      parenthesisExpression || postExpression || castExpression || classInstanceCreation ||
      reference || valueTypes
  } yield Right()

  def parenthesisExpression: CodeWriter[Unit] = for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
  } yield Right()

  def valueTypes: CodeWriter[Unit] = takeTokens(STRING | CHAR | NUMBER).print(x => x)

  def castExpression: CodeWriter[Unit] = for {
    _ <- assertToken(LEFT_PARENTHESIS)
    _ <- primitiveTypes || customDecl
    _ <- assertToken(RIGHT_PARENTHESIS)
    _ <- unaryExpression || lambda
  } yield Right()

  def unaryExpWith(enums: List[JavaTokenEnum]): CodeWriter[Unit] = for {
    _ <- takeTokens(enums).print(x => x)
    _ <- unaryExpression
  } yield Right()

  def reference: CodeWriter[Unit] = {
    def funcArg: CodeWriter[Unit] = for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- tokenSeparatedCtx(expression, COMMA, requireSpace = true) || none.matchDebug("none", "reference/funcRef", "not tokenSeparatedCtx(_, COMMA)")
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    } yield Right()

    def loop: CodeWriter[Unit] = for {
      _ <- assertToken(DOT).tell(".")
      _ <- reference.matchDebug("reference", "reference")
    } yield Right()

    for {
      _ <- identifier.matchDebug("identifier", "reference")
      _ <- arrayRefs.matchDebug("arrayRefs", "reference") || none.matchDebug("none", "reference", "not arrayRefs")
      _ <- funcArg.matchDebug("funcRef", "reference") || none.matchDebug("none", "reference", "not funcRef")
      _ <- loop || none.matchDebug("none", "reference", "not reference")
    } yield Right()
  }

  def lambda: CodeWriter[Unit] = {
    def lambdaDecl: CodeWriter[Unit] = for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- tokenSeparatedCtx(declaration || takeToken(TOKEN).print(x => s"$x"), COMMA, requireSpace = true) || none
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    } yield Right()

    for {
      _ <- lambdaDecl || takeToken(TOKEN).print(x => s"$x")
      _ <- assertToken(LAMBDA_BODY_START).tell(" -> ")
      _ <- expression || blockStmt(false)
    } yield Right()
  }

  def synchronizedStmt: CodeWriter[Unit] = for {
    _ <- assertToken(SYNCHRONIZED).tell("synchronized")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(true)
  } yield Right()

  def declarationStmt: CodeWriter[Unit] = for {
    _ <- declaration
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield Right()

  def declaration: CodeWriter[Unit] = {
    def declDetail: CodeWriter[Unit] = {
      def loop: CodeWriter[Unit] = for {
        _ <- assertToken(COMMA).tell(",").enter()
        _ <- declDetail
      } yield Right()

      for {
        _ <- (primitiveTypes || customDecl).matchDebug("primitiveTypes || customDecl", "declaration/declDetail")
        _ <- (arrayUse || none.matchDebug("none", "declaration/declDetail", "not arrayUse")).tell(" ")
        _ <- identifier.matchDebug("identifier", "declaration/declDetail")
        _ <- variableInitialize.matchDebug("variableInitialize", "declaration/declDetail") || none.matchDebug("none", "declaration/declDetail", "not variableInitialize")
        _ <- loop || none.matchDebug("none", "declaration/declDetail", "not loop")
      } yield Right(())
    }

    for {
      _ <- assertToken(FINAL).tell("final ").matchDebug("assertToken(FINAL).tell", "declaration") ||
        none.matchDebug("none", "declaration", "there's no final")
      _ <- declDetail
    } yield Right()
  }

  def customDecl: CodeWriter[Unit] = for {
    _ <- identifier.matchDebug("identifier", "customDecl")
    _ <- generic || none.matchDebug("none", "customDecl", "not generic")
  } yield Right()

  def variableInitialize: CodeWriter[Unit] = for {
    _ <- assertToken(SUBSTITUTE).tell(" = ")
    _ <- expression.matchDebug("expression", "variableInitialize") || arrayInitializer
  } yield Right(())

  def arrayInitializer: CodeWriter[Unit] = for {
    _ <- assertToken(LBRACE).tell("{")
    _ <- tokenSeparatedCtx(arrayInitializer || expression, COMMA, requireSpace = true) ||
      none.matchDebug("none", "arrayInitializer", "tokenSeparatedCtx(variableInitialize, COMMA)")
    _ <- assertToken(RBRACE).tell("}")
  } yield Right(())

  def generic: CodeWriter[Unit] = for {
    _ <- assertToken(LT).tell("<")
    _ <- tokenSeparatedCtx(identifier, COMMA, requireSpace = true) ||
      none.matchDebug("none", "generic", "not tokenSeparatedCtx(COMMA)")
    _ <- assertToken(GT).tell(">")
  } yield Right(())

  def arrayUse: CodeWriter[Unit] = for {
    _ <- assertToken(LBRACKET)
    _ <- assertToken(RBRACKET).tell("[]")
    _ <- arrayUse || none.matchDebug("none", "arrayUse", "not arrayUse")
  } yield Right(())

  def identifier: CodeWriter[Unit] = tokenSeparatedCtx(takeToken(TOKEN).print(tk => s"$tk"), DOT)

  def tokenSeparatedCtx(chosenParser: CodeWriter[Unit], enum: JavaTokenEnum, requireSpace: Boolean = false): CodeWriter[Unit] = {
    def loop: CodeWriter[Unit] = for {
      _ <- assertToken(enum).tell(s"${enum.value}" + (if (requireSpace) " " else ""))
      res <- tokenSeparatedCtx(chosenParser, enum, requireSpace = requireSpace)
    } yield res

    for {
      _ <- chosenParser
      _ <- loop || none.matchDebug("none", s"tokenSeparatedCtx($enum)", "not loop")
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
