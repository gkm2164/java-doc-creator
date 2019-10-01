package com.kakao.bengo.scala.javadoc.codeformatter

import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.javalang.JavaTokenEnum.{PRIMITIVE_CHAR, PRIMITIVE_LONG, _}
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.codeformatter.exceptions._
import com.kakao.bengo.scala.javadoc.codeformatter.monad._

object JavaParser {

  import com.kakao.bengo.scala.javadoc.codeformatter.syntax._

  val PrimitiveTypeTokens: Seq[JavaTokenEnum] = Seq(PRIMITIVE_BYTE, PRIMITIVE_CHAR, PRIMITIVE_SHORT, PRIMITIVE_LONG,
    PRIMITIVE_INT, PRIMITIVE_FLOAT, PRIMITIVE_DOUBLE, PRIMITIVE_BOOLEAN)
  private val ControlStatementBeginTokens: Seq[JavaTokenEnum] =
    Seq(LBRACE, SEMICOLON, SWITCH, DO, BREAK, CONTINUE, RETURN, FOR, IF, WHILE, SYNCHRONIZED, THROW, TRY)

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
    _ <- blockStmt(true).hint(LBRACE) ||
      emptyStmt.hint(SEMICOLON) ||
      switchStmt.hint(SWITCH) ||
      doStmt.hint(DO) ||
      breakStmt.hint(BREAK) ||
      continueStmt.hint(CONTINUE) ||
      returnStmt.hint(RETURN) ||
      forStmt.hint(FOR) ||
      ifStmt.hint(IF) ||
      whileStmt.hint(WHILE) ||
      synchronizedStmt.hint(SYNCHRONIZED) ||
      throwStmt.hint(THROW) ||
      tryStmt.hint(TRY) ||
      (expressionStmt || declarationStmt).notHint(ControlStatementBeginTokens: _*)
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
    _ <- takeTokens(INC | DEC).print()
    _ <- identifier
  } yield Right(), "preExpression")

  def postExpression: CodeWriter[Unit] = tag(for {
    _ <- identifier
    _ <- takeTokens(INC | DEC).print()
  } yield Right(), "postExpression")

  def methodInvocation: CodeWriter[Unit] = tag(for {
    _ <- reference
    _ <- assertToken(LEFT_PARENTHESIS)
    _ <- tokenSeparatedCtx(expression, COMMA, requireSpace = true) || none
    _ <- assertToken(RIGHT_PARENTHESIS)
  } yield Right(), "methodInvocation")

  def parameters: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- tokenSeparatedCtx(expression, COMMA, requireSpace = true) || none
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
  } yield Right(), "parameters")

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

  def continueStmt: CodeWriter[Unit] = tag(unaryStmt(CONTINUE), "continueStmt")

  def breakStmt: CodeWriter[Unit] = tag(unaryStmt(BREAK), "breakStmt")

  def throwStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(THROW).tell("throw ")
    _ <- expression
  } yield Right(), "throwStmt")

  def emptyStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield Right(), "emptyStmt")

  def tryStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(TRY).tell("try ")
    _ <- blockStmt(false)
    _ <- catchStmts
    _ <- finallyStmt || none
    _ <- none.enter()
  } yield Right(), "tryStmt")

  def catchStmts: CodeWriter[Unit] = tag(for {
    _ <- assertToken(CATCH).tell(" catch")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- catchDeclaration
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false)
    _ <- catchStmts || none
  } yield Right(), "catchStmt")

  def catchDeclaration: CodeWriter[Unit] = tag(for {
    _ <- infixOperator(BIT_OR, typeUse).tell(" ")
    _ <- identifier
  } yield Right(), "catchDeclaration")

  def blockStmts: CodeWriter[Unit] = tag(for {
    _ <- statement
    _ <- blockStmts || none
  } yield Right(), "blockStmts")

  def primitiveTypes: CodeWriter[Unit] = tag(takeTokens(PrimitiveTypeTokens.toList).print(), "primitiveTypes")

  def finallyStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(FINALLY).tell(" finally ")
    _ <- blockStmt(false)
  } yield Right(), "finallyStmt")

  def returnStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(RETURN).tell("return ")
    _ <- expression
  } yield Right(), "returnStmt")

  def forStmt: CodeWriter[Unit] = tag({
    def forConditionStmt: CodeWriter[Unit] = tag({
      def triExpressions: CodeWriter[Unit] = tag(for {
        _ <- declaration || expression || none
        _ <- assertToken(SEMICOLON).tell("; ")
        _ <- conditionalExpression
        _ <- assertToken(SEMICOLON).tell("; ")
        _ <- expression || none
      } yield Right(), "triExpressions")

      def simpleTypes: CodeWriter[Unit] = tag(for {
        _ <- customDecl.tell(" ")
        _ <- identifier
        _ <- assertToken(COLON).tell(": ")
        _ <- expression
      } yield Right(), "simpleTypes")

      triExpressions || simpleTypes
    }, "forConditionStmt")

    for {
      _ <- assertToken(FOR).tell("for ")
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- forConditionStmt
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt(true) || statement
    } yield Right()
  }, "forStmt")

  def ifStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(IF).tell("if ")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false).tell(" ") || statement
    _ <- elseIfStmts || none
    _ <- elseStmt || none
    _ <- none.enter()
  } yield Right(), "ifStmt")

  def elseIfStmts: CodeWriter[Unit] = tag(for {
    _ <- assertToken(ELSE)
    _ <- assertToken(IF).tell("else if ")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false).tell(" ") || statement.enter()
    _ <- elseIfStmts || none
  } yield Right(), "elseIfStmts")

  def elseStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(ELSE).tell("else ")
    _ <- blockStmt(false) || statement
  } yield Right(), "elseStmt")

  def whileStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(WHILE)
    _ <- assertToken(LEFT_PARENTHESIS).tell("while (")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(true) || statement
  } yield Right(), "whileStmt")

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

  def conditionalExpression: CodeWriter[Unit] = tag({
    def conditionalExpressionDetail: CodeWriter[Unit] = tag(for {
      _ <- assertToken(QUESTION_MARK).tell(" ? ")
      _ <- expression
      _ <- assertToken(COLON).tell(" : ")
      _ <- expression || lambda
    } yield Right(), "conditionalExpressionDetail")

    for {
      _ <- conditionalOrExpression
      _ <- conditionalExpressionDetail || none
    } yield Right()
  }, "conditionalExpression")

  def infixOperator(operator: JavaTokenEnum, next: CodeWriter[Unit]): CodeWriter[Unit] = tag({
    def infixOperatorDetail: CodeWriter[Unit] = tag(for {
      _ <- takeToken(operator).print(x => s" $x ")
      _ <- infixOperator(operator, next)
    } yield Right(), "infixOperatorDetail")

    for {
      _ <- next
      _ <- infixOperatorDetail || none
    } yield Right()
  }, s"infixOperator($operator)")


  def infixOperators(operators: List[JavaTokenEnum], next: CodeWriter[Unit]): CodeWriter[Unit] = tag({
    def infixOperatorsDetail: CodeWriter[Unit] = tag(for {
      _ <- takeTokens(operators).print(x => s" $x ")
      _ <- infixOperators(operators, next)
    } yield Right(), "infixOperatorsDetail")

    for {
      _ <- next
      _ <- infixOperatorsDetail || none
    } yield Right()
  }, s"infixOperators(${operators.mkString(" | ")})")

  def conditionalOrExpression: CodeWriter[Unit] = tag(infixOperator(OR, conditionalAndExpression), "conditionalOrExpression")

  def conditionalAndExpression: CodeWriter[Unit] = tag(infixOperator(AND, inclusiveOrExpression), "conditionalAndExpression")

  def inclusiveOrExpression: CodeWriter[Unit] = tag(infixOperator(BIT_OR, exclusiveOrExpression), "inclusiveOrExpression")

  def exclusiveOrExpression: CodeWriter[Unit] = tag(infixOperator(BIT_XOR, andExpression), "exclusiveOrExpression")

  def andExpression: CodeWriter[Unit] = tag(infixOperator(BIT_AND, equalityExpression), "andExpression")

  def equalityExpression: CodeWriter[Unit] = tag(infixOperators(EQUAL | NOT_EQUAL, relationalExpression), "equalityExpression")

  def relationalExpression: CodeWriter[Unit] = tag(for {
    _ <- shiftExpression
    _ <- shiftTail || instanceOfTail || none
  } yield Right(), "relationalExpression")

  def shiftTail: CodeWriter[Unit] = tag(for {
    _ <- takeTokens(LT | GT | LTE | GTE).print(x => s" $x ")
    _ <- relationalExpression
  } yield Right(), "shiftTail")

  def instanceOfTail: CodeWriter[Unit] = tag(for {
    _ <- assertToken(INSTANCEOF).tell(" instanceof ")
    _ <- typeUse
  } yield Right(), "instanceOfTail")

  def shiftExpression: CodeWriter[Unit] =
    tag(infixOperators(LEFT_SHIFT | RIGHT_SHIFT | U_RIGHT_SHIFT, additiveExpression), "shiftExpression")

  def additiveExpression: CodeWriter[Unit] =
    tag(infixOperators(PLUS | MINUS, multiplicativeExpression), "additiveExpression")

  def multiplicativeExpression: CodeWriter[Unit] =
    tag(infixOperators(MULTIPLY | DIVIDE | MODULAR, unaryExpression), "multiplicativeExpression")

  def unaryExpression: CodeWriter[Unit] = tag(for {
    _ <- preExpression.hint(INC, DEC) || postExpression || unaryExpWith(PLUS | MINUS | NEGATE | EXCLAMATION_MARK) ||
      classInstanceCreation.hint(NEW) || tokenSeparatedCtx(valueReferable, DOT)
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

  def valueTypes: CodeWriter[Unit] = tag(takeTokens(STRING | CHAR | NUMBER).print(), "valueTypes").hint(STRING, CHAR, NUMBER)

  def castExpression: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- typeUse
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    _ <- unaryExpression || lambda
  } yield (), "castExpression").hint(LEFT_PARENTHESIS)

  def unaryExpWith(enums: List[JavaTokenEnum]): CodeWriter[Unit] = tag(for {
    _ <- takeTokens(enums).print()
    _ <- unaryExpression
  } yield (), s"unaryExpWith($enums)").hint(enums: _*)

  def reference: CodeWriter[Unit] = tag(for {
    _ <- identifier
    _ <- arrayRefs || none
    _ <- parameters || none
  } yield Right(), "reference")

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
  }, "lambda").hint(LEFT_PARENTHESIS, TOKEN, CLASS, SUPER)

  def synchronizedStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(SYNCHRONIZED).tell("synchronized")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(true)
  } yield Right(), "synchronizedStmt")

  def declarationStmt: CodeWriter[Unit] = tag(for {
    _ <- declaration
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield Right(), "declarationStmt")

  def declaration: CodeWriter[Unit] = tag({
    def declDetail: CodeWriter[Unit] = tag({
      def loop: CodeWriter[Unit] = tag(for {
        _ <- assertToken(COMMA).tell(",").enter()
        _ <- declDetail
      } yield Right(), "loop")

      for {
        _ <- typeUse.tell(" ")
        _ <- identifier
        _ <- variableInitialize.hint(SUBSTITUTE) || none
        _ <- loop || none
      } yield Right()
    }, "declDetail")

    for {
      _ <- assertToken(FINAL).tell("final ") || none
      _ <- declDetail
    } yield Right()
  }, "declaration")

  def typeUse: CodeWriter[Unit] = tag(for {
    _ <- primitiveTypes.hint(PrimitiveTypeTokens: _*) || customDecl
    _ <- arrayUse || none
  } yield Right(), "typeUse")

  def customDecl: CodeWriter[Unit] = tag(for {
    _ <- identifier
    _ <- generic.hint(LT) || none
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
    _ <- tokenSeparatedCtx(typeUse || takeToken(QUESTION_MARK).print(), COMMA, requireSpace = true) || none
    _ <- (assertToken(GT) || unrollingRightShift.hint(RIGHT_SHIFT, U_RIGHT_SHIFT)).tell(">")
  } yield Right(), "generic")

  def arrayUse: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACKET)
    _ <- assertToken(RBRACKET).tell("[]")
    _ <- arrayUse || none
  } yield Right(), "arrayUse")

  def identifier: CodeWriter[Unit] = tokenSeparatedCtx(takeTokens(TOKEN | CLASS | SUPER).print(), DOT)

  def tokenSeparatedCtx(chosenParser: CodeWriter[Unit], enum: JavaTokenEnum, requireSpace: Boolean = false): CodeWriter[Unit] = tag({
    def loop: CodeWriter[Unit] = tag(for {
      _ <- assertToken(enum).tell(s"${enum.value}" + (if (requireSpace) " " else ""))
      res <- tokenSeparatedCtx(chosenParser, enum, requireSpace = requireSpace)
    } yield res, "loop")

    for {
      _ <- chosenParser
      _ <- loop || none
    } yield Right()
  }, s"tokenSeparatedCtx($enum)")

  def fail[T](reason: String): CodeWriter[T] = CodeWriter { tks => (tks, Left(new ParseFailException(reason))) }

  def none: CodeWriter[Unit] = CodeWriter {
    tks => (tks, Right())
  }

  def consumeTokens(enums: List[JavaTokenEnum]): CodeWriter[JavaSToken] = CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (st@JavaSToken(v, _), _) :: t if enums.contains(v) => (t, Right(st))
    case tokenList@(JavaSToken(v, _), _) :: _ =>
      (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but should one of [${enums.map(_.toString).mkString(", ")}]", tokenList)))
  }

  implicit class EnumSyntax(thisToken: JavaTokenEnum) {
    def |(elem: JavaTokenEnum): List[JavaTokenEnum] = List(thisToken, elem)
  }

  implicit class EnumsSyntax(thisTokens: List[JavaTokenEnum]) {
    def |(elem: JavaTokenEnum): List[JavaTokenEnum] = thisTokens :+ elem
  }

  def takeToken(enum: JavaTokenEnum): CodeWriter[String] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (JavaSToken(v, str), _) :: t if v == enum => (t, Right(str))
    case tokenList@(JavaSToken(v, _), _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but $enum", tokenList)))
  }, s"takeToken($enum)")

  def takeTokens(enums: List[JavaTokenEnum]): CodeWriter[String] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (JavaSToken(v, str), _) :: t if enums.contains(v) => (t, Right(str))
    case tokenList@(JavaSToken(v, _), _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but should be one of ${enums.mkString(", ")}", tokenList)))
  }, s"takeTokens(${enums.mkString(" | ")})").hint(enums: _*)

  def assertToken(enum: JavaTokenEnum): CodeWriter[Unit] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (JavaSToken(v, _), _) :: t if v == enum => (t, Right())
    case tokenList@(JavaSToken(v, _), _) :: _ => (tokenList, Left(new TokenNotAllowedException(s"not allow $v, but $enum", tokenList)))
  }, s"assertToken($enum)")

  def assertTokens(enums: List[JavaTokenEnum]): CodeWriter[Unit] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (JavaSToken(v, _), _) :: t if enums.contains(v) => (t, Right())
    case tokenList@(JavaSToken(v, _), _) :: _ => (tokenList,
      Left(new TokenNotAllowedException(s"not allow $v, but one of [${enums.mkString(", ")}]", tokenList)))
  }, s"assertTokens(${enums.mkString(" | ")})")

  def tag[A](writer: CodeWriter[A], t: String): CodeWriter[A] = writer.pushTag(t)

  def unrollingRightShift: CodeWriter[Unit] = tag(CodeWriter {
    case Nil => (Nil, Left(new TokenListEmptyException()))
    case (JavaSToken(shiftType@(RIGHT_SHIFT | U_RIGHT_SHIFT), _), idx) :: t => shiftType match {
      case RIGHT_SHIFT => ((JavaSToken(GT, ">"), idx) :: t, Right())
      case U_RIGHT_SHIFT => ((JavaSToken(RIGHT_SHIFT, ">>"), idx) :: t, Right())
      case _ => (Nil, Left(new ParseFailException("unexpected")))
    }
    case tokenList => (tokenList, Left(new TokenNotAllowedException("token is not '>>'", tokenList)))
  }, "unrollingRightShift")
}