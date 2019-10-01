package com.kakao.bengo.scala.javadoc.codeformatter

import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.javalang.JavaTokenEnum._
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.codeformatter.exceptions._
import com.kakao.bengo.scala.javadoc.codeformatter.monad._

object JavaParser {
  import com.kakao.bengo.scala.javadoc.codeformatter.syntax._
  def blockStmt(enterAtEndLine: Boolean): CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACE).tell("{").enter().tab()
    _ <- blockStmts || none
    _ <- assertToken(RBRACE).untab().tell("}").enterIf(enterAtEndLine)
  } yield Right(), "blockStmt")

  def statements: CodeWriter[Unit] = tag({
    def loop: CodeWriter[Unit] = {
      for {
        _ <- statement
        _ <- loop || none
      } yield Right()
    }

    loop || none
  }, "statements")

  def statement: CodeWriter[Unit] = tag(for {
    _ <- blockStmt(true) || emptyStmt || expressionStmt || switchStmt ||
      doStmt || breakStmt || continueStmt || returnStmt || forStmt || ifStmt ||
      whileStmt || synchronizedStmt || throwStmt || tryStmt || declarationStmt
  } yield Right(), "statement")

  def unaryStmt(token: JavaTokenEnum): CodeWriter[Unit] = tag(for {
    _ <- assertToken(token).tell(token.value)
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield Right(), s"unaryStmt($token)")

  def expressionStmt: CodeWriter[Unit] = tag(for {
    _ <- assignment || expression
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield Right(), "expressionStmt")

  def assignment: CodeWriter[Unit] = tag(for {
    _ <- identifier
    _ <- arrayRefs || none
    _ <- takeTokens(SUBSTITUTE | PLUS_ACC | MINUS_ACC | MULTIPLY_ACC | DIVIDE_ACC).print(x => s" $x ")
    _ <- expression
  } yield Right(), "assignment")

  def arrayRefs: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACKET).tell("[")
    _ <- expression
    _ <- assertToken(RBRACKET).tell("]")
    _ <- arrayRefs || none
  } yield Right(), "arrayRefs")

  def preExpression: CodeWriter[Unit] = tag(for {
    _ <- takeTokens(INC | DEC).print(x => x)
    _ <- identifier
  } yield Right(), "preExpression")

  def postExpression: CodeWriter[Unit] = tag(for {
    _ <- identifier
    _ <- takeTokens(INC | DEC).print(x => x)
  } yield Right(), "postExpression")

  def methodInvocation: CodeWriter[Unit] = tag(for {
    _ <- reference
    _ <- assertToken(LEFT_PARENTHESIS)
    _ <- tokenSeparatedCtx(expression, COMMA, requireSpace = true) || none
    _ <- assertToken(RIGHT_PARENTHESIS)
  } yield Right(), "methodInvocation")

  def parameters: CodeWriter[Unit] = for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- tokenSeparatedCtx(expression, COMMA, requireSpace = true) || none
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
  } yield Right()

  def classInstanceCreation: CodeWriter[Unit] = tag({
    def arrayInstance: CodeWriter[Unit] = tag(for {
      _ <- assertToken(LBRACKET).tell("[")
      _ <- expression || none
      _ <- assertToken(RBRACKET).tell("]")
      _ <- arrayInitializer || none
    } yield Right(), "arrayInstance")

    for {
      _ <- assertToken(NEW).tell("new ")
      _ <- primitiveTypes || identifier
      _ <- generic || none
      _ <- arrayInstance || parameters
    } yield Right()
  }, "classInstanceCreation")

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
    _ <- blockStmt(false)
    _ <- catchStmts
    _ <- finallyStmt || none
    _ <- none.enter()
  } yield Right()

  def catchStmts: CodeWriter[Unit] = for {
    _ <- assertToken(CATCH).tell(" catch")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- declaration
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false)
    _ <- catchStmts || none
  } yield Right()

  def blockStmts: CodeWriter[Unit] = for {
    _ <- statement
    _ <- blockStmts || none
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
    _ <- assertToken(FINALLY).tell(" finally ")
    _ <- blockStmt(false)
  } yield Right()

  def returnStmt: CodeWriter[Unit] = for {
    _ <- assertToken(RETURN).tell("return ")
    _ <- expression
  } yield Right()

  def forStmt: CodeWriter[Unit] = {
    def forConditionStmt: CodeWriter[Unit] = {
      def triExpressions: CodeWriter[Unit] = for {
        _ <- declaration || expression || none
        _ <- assertToken(SEMICOLON).tell("; ")
        _ <- conditionalExpression
        _ <- assertToken(SEMICOLON).tell("; ")
        _ <- expression || none
      } yield Right()

      def simpleTypes: CodeWriter[Unit] = for {
        _ <- customDecl.tell(" ")
        _ <- identifier
        _ <- assertToken(COLON).tell(": ")
        _ <- expression
      } yield Right()

      triExpressions || simpleTypes
    }

    for {
      _ <- assertToken(FOR).tell("for ")
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- forConditionStmt
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt(true) || statement
    } yield Right()
  }

  def ifStmt: CodeWriter[Unit] = for {
    _ <- assertToken(IF).tell("if ")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false).tell(" ") || statement
    _ <- elseIfStmts || none
    _ <- elseStmt || none
    _ <- none.enter()
  } yield Right()

  def elseIfStmts: CodeWriter[Unit] = for {
    _ <- assertToken(ELSE)
    _ <- assertToken(IF).tell("else if ")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false).tell(" ") || statement.enter()
    _ <- elseIfStmts || none
  } yield Right()

  def elseStmt: CodeWriter[Unit] = for {
    _ <- assertToken(ELSE).tell("else ")
    _ <- blockStmt(false) || statement
  } yield Right()

  def whileStmt: CodeWriter[Unit] = for {
    _ <- assertToken(WHILE)
    _ <- assertToken(LEFT_PARENTHESIS).tell("while (")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(true) || statement
  } yield Right()

  //noinspection MutatorLikeMethodIsParameterless
  def doStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(DO).tell("do ")
    _ <- blockStmt(false).tell(" ")
    _ <- assertToken(WHILE)
    _ <- assertToken(LEFT_PARENTHESIS).tell("while(")
    _ <- expressionStmt
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield Right(), "doStmt")

  def switchStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(SWITCH).tell("switch ")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- assertToken(LBRACE).tell("{").enter().tab()
    _ <- caseStmts || none
    _ <- defaultStmt || none
    _ <- assertToken(RBRACE).tell("}").untab().enter()
  } yield Right(), "switchStmt")

  def caseStmts: CodeWriter[Unit] = tag(for {
    _ <- caseStmtDetail
    _ <- caseStmts || none
  } yield Right(), "caseStmts")

  def defaultStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(DEFAULT)
    _ <- assertToken(COLON).tell("default: ")
    _ <- statements
  } yield Right(), "defaultStmt")

  def caseStmtDetail: CodeWriter[Unit] = tag(for {
    _ <- assertToken(CASE).tell("case ")
    _ <- valueTypes || identifier
    _ <- assertToken(COLON).tell(": ")
    _ <- statements
  } yield Right(), "caseStmtDetail")

  def expression: CodeWriter[Unit] = tag(lambda || castExpression || assignment || conditionalExpression, "expression")

  def conditionalExpression: CodeWriter[Unit] = {
    def detail: CodeWriter[Unit] = for {
      _ <- assertToken(QUESTION_MARK).tell(" ? ")
      _ <- expression
      _ <- assertToken(COLON).tell(" : ")
      _ <- conditionalExpression || lambda
    } yield Right()

    for {
      _ <- conditionalOrExpression
      _ <- detail || none
    } yield Right()
  }

  def infixOperator(operator: JavaTokenEnum, next: CodeWriter[Unit]): CodeWriter[Unit] = {
    def detail: CodeWriter[Unit] = for {
      _ <- takeToken(operator).print(x => s" $x ")
      _ <- infixOperator(operator, next)
    } yield Right()

    for {
      _ <- next
      _ <- detail || none
    } yield Right()
  }


  def infixOperators(operators: List[JavaTokenEnum], next: CodeWriter[Unit]): CodeWriter[Unit] = {
    def detail: CodeWriter[Unit] = for {
      _ <- takeTokens(operators).print(x => s" $x ")
      _ <- infixOperators(operators, next)
    } yield Right()

    for {
      _ <- next
      _ <- detail || none
    } yield Right()
  }

  def conditionalOrExpression: CodeWriter[Unit] = infixOperator(OR, conditionalAndExpression)

  def conditionalAndExpression: CodeWriter[Unit] = infixOperator(AND, inclusiveOrExpression)

  def inclusiveOrExpression: CodeWriter[Unit] = infixOperator(BIT_OR, exclusiveOrExpression)

  def exclusiveOrExpression: CodeWriter[Unit] = infixOperator(BIT_XOR, andExpression)

  def andExpression: CodeWriter[Unit] = infixOperator(BIT_AND, equalityExpression)

  def equalityExpression: CodeWriter[Unit] = infixOperators(EQUAL | NOT_EQUAL, relationalExpression)

  def relationalExpression: CodeWriter[Unit] = tag(for {
    _ <- shiftExpression
    _ <- shiftTail || instanceOfTail || none
  } yield Right(), "relationalExpression")

  def shiftTail: CodeWriter[Unit] = for {
    _ <- takeTokens(LT | GT | LTE | GTE).print(x => s" $x ")
    _ <- relationalExpression
  } yield Right()

  def instanceOfTail: CodeWriter[Unit] = for {
    _ <- assertToken(INSTANCEOF).tell(" instanceof ")
    _ <- typeUse
  } yield Right()

  def shiftExpression: CodeWriter[Unit] = infixOperators(LEFT_SHIFT | RIGHT_SHIFT | U_RIGHT_SHIFT, additiveExpression)

  def additiveExpression: CodeWriter[Unit] = infixOperators(PLUS | MINUS, multiplicativeExpression)

  def multiplicativeExpression: CodeWriter[Unit] = infixOperators(MULTIPLY | DIVIDE | MODULAR, unaryExpression)

  def unaryExpression: CodeWriter[Unit] = tag(for {
    _ <- preExpression || postExpression || unaryExpWith(PLUS | MINUS | NEGATE | EXCLAMATION_MARK) ||
      classInstanceCreation || tokenSeparatedCtx(valueReferable, DOT)
  } yield Right(), "unaryExpression")

  def valueReferable: CodeWriter[Unit] = tag(for {
    _ <- parenthesisExpression || reference || valueTypes
    _ <- arrayRefs || none
  } yield Right(), "valueReferable")

  def parenthesisExpression: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
  } yield Right(), "parenthesisExpression")

  def valueTypes: CodeWriter[Unit] = takeTokens(STRING | CHAR | NUMBER).print(x => x)

  def castExpression: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- typeUse
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    _ <- unaryExpression || lambda
  } yield Right(), "castExpression")

  def unaryExpWith(enums: List[JavaTokenEnum]): CodeWriter[Unit] = tag(for {
    _ <- takeTokens(enums).print(x => x)
    _ <- unaryExpression
  } yield Right(), s"unaryExpWith($enums)")

  def reference: CodeWriter[Unit] = {
    for {
      _ <- identifier
      _ <- arrayRefs || none
      _ <- parameters || none
    } yield Right()
  }

  def lambda: CodeWriter[Unit] = tag({
    def lambdaDecl: CodeWriter[Unit] = tag(for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- tokenSeparatedCtx(declaration || takeToken(TOKEN).print(x => s"$x"), COMMA, requireSpace = true) || none
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    } yield Right(), "lambdaDecl")

    def originLambda: CodeWriter[Unit] = tag(for {
      _ <- lambdaDecl || takeToken(TOKEN).print(x => s"$x")
      _ <- assertToken(LAMBDA_BODY_START).tell(" -> ")
      _ <- expression || blockStmt(false)
    } yield Right(), "originLambda")

    def shortenLambda: CodeWriter[Unit] = tag(for {
      _ <- identifier
      _ <- assertToken(NAMESPACE).tell("::")
      _ <- identifier
    } yield Right(), "shortenLambda")

    originLambda || shortenLambda
  }, "lambda")

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
        _ <- typeUse.tell(" ")
        _ <- identifier
        _ <- variableInitialize || none
        _ <- loop || none
      } yield Right()
    }

    for {
      _ <- assertToken(FINAL).tell("final ") || none
      _ <- declDetail
    } yield Right()
  }

  def typeUse: CodeWriter[Unit] = tag(for {
    _ <- primitiveTypes || customDecl
    _ <- arrayUse || none
  } yield Right(), "typeUse")

  def customDecl: CodeWriter[Unit] = tag(for {
    _ <- identifier
    _ <- generic || none
  } yield Right(), "customDecl")

  def variableInitialize: CodeWriter[Unit] = tag(for {
    _ <- assertToken(SUBSTITUTE).tell(" = ")
    _ <- expression || arrayInitializer
  } yield Right(), "variableInitialize")

  def arrayInitializer: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACE).tell("{")
    _ <- tokenSeparatedCtx(arrayInitializer || expression, COMMA, requireSpace = true) || none
    _ <- assertToken(RBRACE).tell("}")
  } yield Right(), "arrayInitializer")

  def generic: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LT).tell("<")
    _ <- tokenSeparatedCtx(typeUse || takeToken(QUESTION_MARK).print(x => x), COMMA, requireSpace = true) || none
    _ <- assertToken(GT).tell(">") ||
      unrollingRightShift.tell(">")
  } yield Right(), "generic")

  def arrayUse: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACKET)
    _ <- assertToken(RBRACKET).tell("[]")
    _ <- arrayUse || none
  } yield Right(), "arrayUse")

  def identifier: CodeWriter[Unit] = tokenSeparatedCtx(takeTokens(TOKEN | CLASS | SUPER).print(tk => s"$tk"), DOT)

  def tokenSeparatedCtx(chosenParser: CodeWriter[Unit], enum: JavaTokenEnum, requireSpace: Boolean = false): CodeWriter[Unit] = {
    def loop: CodeWriter[Unit] = for {
      _ <- assertToken(enum).tell(s"${enum.value}" + (if (requireSpace) " " else ""))
      res <- tokenSeparatedCtx(chosenParser, enum, requireSpace = requireSpace)
    } yield res

    for {
      _ <- chosenParser
      _ <- loop || none
    } yield Right()
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
    case JavaSToken(v, str) :: t if v == enum => (t, Right(str))
    case tokenList@JavaSToken(v, _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but $enum", tokenList)))
  }

  def takeTokens(enums: List[JavaTokenEnum]): CodeWriter[String] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case JavaSToken(v, str) :: t if enums.contains(v) => (t, Right(str))
    case tokenList@JavaSToken(v, _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but should be one of ${enums.mkString(", ")}", tokenList)))
  }

  def assertToken(enum: JavaTokenEnum): CodeWriter[Unit] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case JavaSToken(v, _) :: t if v == enum => (t, Right())
    case tokenList@JavaSToken(v, _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but $enum", tokenList)))
  }

  def assertTokens(enums: List[JavaTokenEnum]): CodeWriter[Unit] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case JavaSToken(v, _) :: t if enums.contains(v) => (t, Right())
    case tokenList@JavaSToken(v, _) :: _ => (tokenList,
      Left(new TokenNotAllowedException(s"not allow $v, but one of [${enums.mkString(", ")}]", tokenList)))
  }

  def unrollingRightShift: CodeWriter[Unit] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case JavaSToken(shiftType@(RIGHT_SHIFT | U_RIGHT_SHIFT), _) :: t => shiftType match {
      case RIGHT_SHIFT => (JavaSToken(GT, ">") :: t, Right())
      case U_RIGHT_SHIFT => (JavaSToken(RIGHT_SHIFT, ">>") :: t, Right())
      case _ => (Nil, Left(new ParseFailException("unexpected")))
    }
    case tokenList => (tokenList, Left(new TokenNotAllowedException("token is not '>>'", tokenList)))
  }

  def tag(writer: CodeWriter[Unit], t: String): CodeWriter[Unit] = writer.pushTag(t)

}