package co.gyeongmin.lang.javadoc.codeformatter

import co.gyeongmin.lang.javadoc.JavaSToken
import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javalang.JavaTokenEnum
import co.gyeongmin.lang.javalang.JavaTokenEnum._

import scala.language.postfixOps

object JavaParser {

  import co.gyeongmin.lang.javadoc.codeformatter.syntax._
  import Helper._

  // LL(1) parser
  private val PrimitiveTypeTokens: List[JavaTokenEnum] =
    List(PRIMITIVE_BYTE, PRIMITIVE_CHAR, PRIMITIVE_SHORT, PRIMITIVE_LONG,
      PRIMITIVE_INT, PRIMITIVE_FLOAT, PRIMITIVE_DOUBLE, PRIMITIVE_BOOLEAN)
  private val ControlStatementBeginTokens: List[JavaTokenEnum] =
    List(LBRACE, SEMICOLON, SWITCH, DO, BREAK, CONTINUE, RETURN, FOR, IF,
      WHILE, SYNCHRONIZED, THROW, TRY)
  private val JavaValueTypeTokens: List[JavaTokenEnum] =
    List(STRING, NUMBER, CHAR, TRUE, FALSE, NULL)
  private val IdentifiableTokens: List[JavaTokenEnum] =
    List(TOKEN, SUPER, CLASS)
  private val UnaryStartable: List[JavaTokenEnum] =
    List(PLUS, MINUS, NEGATE, EXCLAMATION_MARK)
  private val ExpressionStartable =
    INC :: DEC :: NEW :: LEFT_PARENTHESIS :: UnaryStartable ::: IdentifiableTokens ::: JavaValueTypeTokens ::: PrimitiveTypeTokens
  val ModifierStartToken: List[JavaTokenEnum] =
    List(PUBLIC, PRIVATE, PROTECTED, STRICTFP, FINAL, ABSTRACT, STATIC, ANNOTATION)


  def blockStmt(enterAtEndLine: Boolean): CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACE).tell(block("{")).enter().tab()
    _ <- blockStmts || none
    _ <- assertToken(RBRACE).untab().tell(block("}")).enterIf(enterAtEndLine)
  } yield (), "blockStmt").foldable

  def statements: CodeWriter[Unit] = tag({
    def loop: CodeWriter[Unit] = {
      for {
        _ <- statement
        _ <- loop || none
      } yield ()
    }

    loop || none
  }, "statements")

  def statement: CodeWriter[Unit] = tag(for {
    _ <- controlStatement.hint(ControlStatementBeginTokens: _*) ||
      nonControlStatement.notHint(ControlStatementBeginTokens: _*)
  } yield (), "statement")

  def controlStatement: CodeWriter[Unit] = tag(
    blockStmt(true).hint(LBRACE) ||
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
      tryStmt.hint(TRY), "controlStatement")

  def nonControlStatement: CodeWriter[Unit] = tag(expressionStmt || declarationStmt, "nonControlStatement")

  def unaryStmt(token: JavaTokenEnum): CodeWriter[Unit] = tag(for {
    _ <- assertToken(token).tell(keyword(token.value))
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield (), s"unaryStmt($token)")

  def expressionStmt: CodeWriter[Unit] = tag(for {
    _ <- assignment || expression.hint(ExpressionStartable: _*)
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield (), "expressionStmt")

  def assignment: CodeWriter[Unit] = tag(for {
    _ <- identifier
    _ <- arrayRefs || none
    _ <- takeTokens(SUBSTITUTE | PLUS_ACC | MINUS_ACC | MULTIPLY_ACC | DIVIDE_ACC).print(x => s" $x ")
    _ <- expression
  } yield (), "assignment").hint(TOKEN, SUPER)

  def arrayRefs: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACKET).tell("[")
    _ <- expression
    _ <- assertToken(RBRACKET).tell("]")
    _ <- arrayRefs || none
  } yield (), "arrayRefs")

  def preExpression: CodeWriter[Unit] = tag(for {
    _ <- takeTokens(INC | DEC).print()
    _ <- identifier
  } yield (), "preExpression")

  def postExpression: CodeWriter[Unit] = tag(for {
    _ <- identifier
    _ <- takeTokens(INC | DEC).print()
  } yield (), "postExpression")

  def parameters: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- tokenSeparatedCtx(expression, COMMA, requireSpace = true) || none
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    _ <- classBodyDefinition || none
  } yield (), "parameters")

  def classBodyDefinition: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACE).tell(" {").enter().tab()
    _ <- definitionElements || none
    _ <- assertToken(RBRACE).untab().tell("}")
  } yield (), "classBodyDefinition")

  def definitionElements: CodeWriter[Unit] = tag(for {
    _ <- (classDefinition || memberDefinition).enter()
    _ <- definitionElements || none
  } yield (), "definitionElements")

  def classDefinition: CodeWriter[Unit] = tag(for {
    _ <- modifiers || none
    _ <- takeTokens(ENUM | CLASS).print(x => keyword(s"$x "))
    _ <- identifier
    _ <- typeParameters || none
    _ <- superClass || none
    _ <- superInterface || none
    _ <- classBodyDefinition
  } yield (), "classDefinition")

  def modifiers: CodeWriter[Unit] = tag(for {
    _ <- takeTokens(PUBLIC | PRIVATE | PROTECTED | STRICTFP | FINAL | ABSTRACT | STATIC).print(x => keyword(s"$x ")) || annotation.enter()
    _ <- modifiers || none
  } yield (), "modifiers").hint(ModifierStartToken: _*)

  def annotation: CodeWriter[Unit] = tag(for {
    _ <- assertToken(ANNOTATION).tell(color("@", "yellow"))
    _ <- takeToken(TOKEN).print(color(_, "yellow"))
    _ <- annotationTail || none
  } yield (), "annotation")

  def annotationTail: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- annotationSingleValue || tokenSeparatedCtx(annotationValuePairs, COMMA, requireSpace = true) || none
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
  } yield (), "annotationTail")

  def annotationSingleValue: CodeWriter[Unit] =
    tag(conditionalExpression || arrayInitializer || annotation, "annotationSingleValue")

  def annotationValuePairs: CodeWriter[Unit] = tag(for {
    _ <- identifier
    _ <- assertToken(SUBSTITUTE).tell(" = ")
    _ <- annotationSingleValue
  } yield (), "annotationValuePairs")

  def typeParameters: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LT).tell("<")
    _ <- tokenSeparatedCtx(typeVariable, COMMA, requireSpace = true)
    _ <- assertToken(GT).tell(">") || unrollingRightShift.tell(">")
  } yield (), "typeParameters")

  def typeVariable: CodeWriter[Unit] = tag(for {
    _ <- annotation || none
    _ <- identifier
    _ <- typeBound || none
  } yield (), "typeVariable")

  def typeBound: CodeWriter[Unit] = tag(for {
    _ <- assertToken(EXTENDS).tell("extends ")
    _ <- identifier
    _ <- additionalBound || none
  } yield (), "typeBound")

  def additionalBound: CodeWriter[Unit] = tag(for {
    _ <- assertToken(BIT_AND).tell(" & ")
    _ <- identifier
  } yield (), "additionalBound")

  def superClass: CodeWriter[Unit] = tag(for {
    _ <- assertToken(EXTENDS).tell("extends ")
    _ <- tokenSeparatedCtx(typeUse, COMMA, requireSpace = true)
  } yield (), "superClass")

  def superInterface: CodeWriter[Unit] = tag(for {
    _ <- assertToken(IMPLEMENTS).tell("implements ")
    _ <- tokenSeparatedCtx(typeUse, COMMA, requireSpace = true)
  } yield (), "superInterface")

  def memberDefinition: CodeWriter[Unit] = tag(for {
    _ <- modifiers || none
    _ <- typeUse.tell(" ")
    _ <- identifier
    _ <- fieldDefinition || methodDefinition
  } yield (), "memberDefinition")

  def fieldDefinition: CodeWriter[Unit] = tag(for {
    _ <- variableInitialize || none
    _ <- takeToken(SEMICOLON).tell(";")
  } yield (), "fieldDefinition")

  def methodDefinition: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- tokenSeparatedCtx(methodArgDef, COMMA, requireSpace = true) || none
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- methodThrows || none
    _ <- blockStmt(false) || assertToken(SEMICOLON).tell(";").enter()
  } yield (), "methodDefinition")

  def methodThrows: CodeWriter[Unit] = tag(for {
    _ <- assertToken(THROWS).tell(keyword("throws "))
    _ <- tokenSeparatedCtx(typeUse, COMMA, requireSpace = true)
  } yield(), "methodThrows")

  def methodArgDef: CodeWriter[Unit] = tag(for {
    _ <- annotation.hint(ANNOTATION) || none
    _ <- typeUse
    _ <- identifier
  } yield (), "methodArgDef")

  def classInstanceCreation: CodeWriter[Unit] = tag({
    def classInstance: CodeWriter[Unit] = tag(for {
      _ <- parameters
      _ <- classBodyDefinition || none
    } yield (), "classInstance")

    def arrayInstance: CodeWriter[Unit] = tag(for {
      _ <- assertToken(LBRACKET).tell("[")
      _ <- expression || none
      _ <- assertToken(RBRACKET).tell("]")
      _ <- arrayInitializer || none
    } yield (), "arrayInstance")

    def newInstanceReferable: CodeWriter[Unit] = tag(for {
      _ <- assertToken(DOT).tell(".")
      _ <- tokenSeparatedCtx(reference, DOT)
    } yield (), "newInstanceReferable")

    for {
      _ <- assertToken(NEW).tell(keyword("new "))
      _ <- primitiveTypes.hint(PrimitiveTypeTokens: _*) || tokenSeparatedCtx(takeToken(TOKEN).print(typeNameCss), DOT)
      _ <- generic.hint(LT) || none
      _ <- arrayInstance.hint(LBRACKET, LBRACE) || classInstance.hint(LEFT_PARENTHESIS)
      _ <- newInstanceReferable || none
    } yield ()
  }, "classInstanceCreation")

  def continueStmt: CodeWriter[Unit] = tag(unaryStmt(CONTINUE), "continueStmt")

  def breakStmt: CodeWriter[Unit] = tag(unaryStmt(BREAK), "breakStmt")

  def throwStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(THROW).tell(keyword("throw "))
    _ <- expression
  } yield (), "throwStmt")

  def emptyStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield (), "emptyStmt")

  def tryStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(TRY).tell(keyword("try "))
    _ <- tryDeclaration || none
    _ <- blockStmt(false)
    _ <- catchStmts || none
    _ <- finallyStmt || none
    _ <- none.enter()
  } yield (), "tryStmt")

  def tryDeclaration: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- declaration
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
  } yield(), "tryDeclaration")

  def catchStmts: CodeWriter[Unit] = tag(for {
    _ <- assertToken(CATCH).tell(keyword(" catch "))
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- catchDeclaration
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false)
    _ <- catchStmts || none
  } yield (), "catchStmt")

  def catchDeclaration: CodeWriter[Unit] = tag(for {
    _ <- infixOperator(BIT_OR, typeUse).tell(" ")
    _ <- identifier
  } yield (), "catchDeclaration")

  def blockStmts: CodeWriter[Unit] = tag(for {
    _ <- statement
    _ <- blockStmts || none
  } yield (), "blockStmts")

  def primitiveTypes: CodeWriter[Unit] = tag(takeTokens(PrimitiveTypeTokens).print(keyword), "primitiveTypes")

  def finallyStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(FINALLY).tell(keyword(" finally "))
    _ <- blockStmt(false)
  } yield (), "finallyStmt")

  def returnStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(RETURN).tell(keyword("return "))
    _ <- expression || none
  } yield (), "returnStmt")

  def forStmt: CodeWriter[Unit] = tag({
    def forConditionStmt: CodeWriter[Unit] = tag({
      def triExpressions: CodeWriter[Unit] = tag(for {
        _ <- declaration || expression || none
        _ <- assertToken(SEMICOLON).tell("; ")
        _ <- conditionalExpression
        _ <- assertToken(SEMICOLON).tell("; ")
        _ <- expression || none
      } yield (), "triExpressions")

      def simpleTypes: CodeWriter[Unit] = tag(for {
        _ <- typeUse.tell(" ") || none
        _ <- identifier
        _ <- assertToken(COLON).tell(": ")
        _ <- expression
      } yield (), "simpleTypes")

      triExpressions || simpleTypes
    }, "forConditionStmt")

    for {
      _ <- assertToken(FOR).tell(keyword("for "))
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- forConditionStmt
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt(true) || statement
    } yield ()
  }, "forStmt")

  def ifStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(IF).tell(keyword("if "))
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false).tell(" ") || statement
    _ <- elseIfStmts || none
    _ <- elseStmt || none
    _ <- none.enter()
  } yield (), "ifStmt")

  def elseIfStmts: CodeWriter[Unit] = tag(for {
    _ <- assertToken(ELSE)
    _ <- assertToken(IF).tell(keyword("else if "))
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(false).tell(" ") || statement.enter()
    _ <- elseIfStmts || none
  } yield (), "elseIfStmts")

  def elseStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(ELSE).tell(keyword("else "))
    _ <- blockStmt(false) || statement
  } yield (), "elseStmt")

  def whileStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(WHILE).tell(keyword("while "))
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(true) || statement
  } yield (), "whileStmt")

  //noinspection MutatorLikeMethodIsParameterless
  def doStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(DO).tell(keyword("do "))
    _ <- blockStmt(false).tell(" ")
    _ <- assertToken(WHILE).tell(keyword("while "))
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expressionStmt
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield (), "doStmt")

  def switchStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(SWITCH).tell(keyword("switch "))
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- caseBlock
  } yield (), "switchStmt")

  def caseBlock: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACE).tell(block("{")).enter().tab()
    _ <- caseStmts || none
    _ <- defaultStmt || none
    _ <- assertToken(RBRACE).tell(block("}")).untab().enter()
  } yield (), "caseBlock").foldable

  def caseStmts: CodeWriter[Unit] = tag(for {
    _ <- caseStmtDetail
    _ <- caseStmts || none
  } yield (), "caseStmts")

  def defaultStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(DEFAULT).tell(keyword("default"))
    _ <- assertToken(COLON).tell(":").enter().tab()
    _ <- statements.untab()
  } yield (), "defaultStmt")

  def caseStmtDetail: CodeWriter[Unit] = tag(for {
    _ <- assertToken(CASE).tell(keyword("case "))
    _ <- valueTypes || identifier
    _ <- assertToken(COLON).tell(":").enter().tab()
    _ <- statements.untab()
  } yield (), "caseStmtDetail")

  def expression: CodeWriter[Unit] = tag(lambda || castExpression || assignment || conditionalExpression, "expression")

  def conditionalExpression: CodeWriter[Unit] = tag({
    def conditionalExpressionDetail: CodeWriter[Unit] = tag(for {
      _ <- assertToken(QUESTION_MARK).tell(" ? ")
      _ <- expression
      _ <- assertToken(COLON).tell(" : ")
      _ <- expression || lambda
    } yield (), "conditionalExpressionDetail")

    for {
      _ <- conditionalOrExpression
      _ <- conditionalExpressionDetail || none
    } yield ()
  }, "conditionalExpression").hint(ExpressionStartable: _*)

  def infixOperator(operator: JavaTokenEnum, next: CodeWriter[Unit]): CodeWriter[Unit] = tag({
    def infixOperatorDetail: CodeWriter[Unit] = tag(for {
      _ <- takeToken(operator).print(x => s" $x ")
      _ <- infixOperator(operator, next)
    } yield (), "infixOperatorDetail")

    for {
      _ <- next
      _ <- infixOperatorDetail || none
    } yield ()
  }, s"infixOperator($operator)")


  def infixOperators(operators: List[JavaTokenEnum], next: CodeWriter[Unit]): CodeWriter[Unit] = tag({
    def infixOperatorsDetail: CodeWriter[Unit] = tag(for {
      _ <- takeTokens(operators).print(x => s" $x ")
      _ <- infixOperators(operators, next)
    } yield (), "infixOperatorsDetail")

    for {
      _ <- next
      _ <- infixOperatorsDetail || none
    } yield ()
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
  } yield (), "relationalExpression")

  def shiftTail: CodeWriter[Unit] = tag(for {
    _ <- takeTokens(LT | GT | LTE | GTE).print(x => s" $x ")
    _ <- relationalExpression
  } yield (), "shiftTail")

  def instanceOfTail: CodeWriter[Unit] = tag(for {
    _ <- assertToken(INSTANCEOF).tell(keyword(" instanceof "))
    _ <- typeUse
  } yield (), "instanceOfTail")

  def shiftExpression: CodeWriter[Unit] =
    tag(infixOperators(LEFT_SHIFT | RIGHT_SHIFT | U_RIGHT_SHIFT, additiveExpression), "shiftExpression")

  def additiveExpression: CodeWriter[Unit] =
    tag(infixOperators(PLUS | MINUS, multiplicativeExpression), "additiveExpression")

  def multiplicativeExpression: CodeWriter[Unit] =
    tag(infixOperators(MULTIPLY | DIVIDE | MODULAR, unaryExpression), "multiplicativeExpression")

  def unaryExpression: CodeWriter[Unit] = tag(for {
    _ <- preExpression.hint(INC, DEC) || postExpression.hint(IdentifiableTokens: _*) || unaryExpWith(UnaryStartable) ||
      classInstanceCreation.hint(NEW) || tokenSeparatedCtx(referableValue, DOT).hint(LEFT_PARENTHESIS :: PrimitiveTypeTokens ::: IdentifiableTokens ::: JavaValueTypeTokens: _*)
  } yield (), "unaryExpression").hint(ExpressionStartable: _*)

  def referableValue: CodeWriter[Unit] = tag(for {
    _ <- parenthesisExpression || reference || valueTypes
    _ <- arrayRefs || none
  } yield (), "valueReferable")

  def parenthesisExpression: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
  } yield (), "parenthesisExpression")

  def valueTypes: CodeWriter[Unit] = tag(takeTokens(JavaValueTypeTokens, {
    case JavaSToken(enum, v) => enum match {
      case TRUE | FALSE | NULL => keyword(v)
      case NUMBER => color(v, "#3986AF")
      case STRING | CHAR => color(v, "#477944")
      case _ => v
    }
  }).print(), "valueTypes").hint(JavaValueTypeTokens: _*)

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

  def primitiveTypesArrType: CodeWriter[Unit] = tag(for {
    _ <- primitiveTypes
    _ <- arrayUse || none
  } yield(), "primitiveTypesArrType")

  def reference: CodeWriter[Unit] = tag(for {
    _ <- generic || none
    _ <- identifier || primitiveTypesArrType // 이 문법이 여기에 와도 되려나?
    _ <- arrayRefs || none
    _ <- parameters || none
  } yield (), "reference")

  def lambda: CodeWriter[Unit] = tag({
    def lambdaDecl: CodeWriter[Unit] = tag(for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- tokenSeparatedCtx(declaration || takeToken(TOKEN).print(x => s"$x"), COMMA, requireSpace = true) || none
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    } yield (), "lambdaDecl")

    def originLambda: CodeWriter[Unit] = tag(for {
      _ <- lambdaDecl || takeToken(TOKEN).print(x => s"$x")
      _ <- assertToken(LAMBDA_BODY_START).tell(" -> ")
      _ <- expression || blockStmt(false)
    } yield (), "originLambda")

    def shortenLambda: CodeWriter[Unit] = tag(for {
      _ <- tokenSeparatedCtx(typeUse || identifier, DOT)
      _ <- assertToken(NAMESPACE).tell("::")
      _ <- identifier || assertToken(NEW).tell(keyword("new"))
    } yield (), "shortenLambda")

    originLambda || shortenLambda
  }, "lambda").hint(LEFT_PARENTHESIS, TOKEN, CLASS, SUPER)

  def synchronizedStmt: CodeWriter[Unit] = tag(for {
    _ <- assertToken(SYNCHRONIZED).tell("synchronized")
    _ <- assertToken(LEFT_PARENTHESIS).tell("(")
    _ <- expression
    _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
    _ <- blockStmt(true)
  } yield (), "synchronizedStmt")

  def declarationStmt: CodeWriter[Unit] = tag(for {
    _ <- declaration
    _ <- assertToken(SEMICOLON).tell(";").enter()
  } yield (), "declarationStmt")

  def declaration: CodeWriter[Unit] = tag({
    def declDetail: CodeWriter[Unit] = tag({
      def loop: CodeWriter[Unit] = tag(for {
        _ <- assertToken(COMMA).tell(",").enter()
        _ <- declDetail
      } yield (), "loop")

      for {
        _ <- typeUse.tell(" ")
        _ <- identifier
        _ <- variableInitialize.hint(SUBSTITUTE) || none
        _ <- loop || none
      } yield ()
    }, "declDetail")

    for {
      _ <- annotation.tell(" ") || none
      _ <- assertToken(FINAL).tell(keyword("final ")) || none
      _ <- declDetail
    } yield ()
  }, "declaration")

  def typeUse: CodeWriter[Unit] = tag(for {
    _ <- primitiveTypes.hint(PrimitiveTypeTokens: _*) || customDecl
    _ <- arrayUse || none
  } yield (), "typeUse")

  def customDecl: CodeWriter[Unit] = tag(for {
    _ <- tokenSeparatedCtx(takeToken(TOKEN).print(typeNameCss), DOT)
    _ <- generic || none
  } yield (), "customDecl")

  def variableInitialize: CodeWriter[Unit] = tag(for {
    _ <- assertToken(SUBSTITUTE).tell(" = ")
    _ <- expression || arrayInitializer
  } yield (), "variableInitialize")

  def arrayInitializer: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACE).tell("{")
    _ <- tokenSeparatedCtx(arrayInitializer || expression, COMMA, requireSpace = true) || none
    _ <- assertToken(RBRACE).tell("}")
  } yield (), "arrayInitializer").foldable

  def generic: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LT).tell("<")
    _ <- tokenSeparatedCtx(genericTypeUse, COMMA, requireSpace = true) || none
    _ <- (assertToken(GT) || unrollingRightShift).tell(">")
  } yield (), "generic")

  def genericTypeUse: CodeWriter[Unit] = tag(for {
    _ <- typeUse || takeToken(QUESTION_MARK).print()
    _ <- genericTypeBound || none
  } yield (), "genericExtends")

  def genericTypeBound: CodeWriter[Unit] = tag(for {
    _ <- takeTokens(EXTENDS | SUPER).print(x => keyword(s" $x "))
    _ <- typeUse
  } yield (), "genericTypeBound")

  def arrayUse: CodeWriter[Unit] = tag(for {
    _ <- assertToken(LBRACKET)
    _ <- assertToken(RBRACKET).tell("[]")
    _ <- arrayUse || none
  } yield (), "arrayUse")

  def identifier: CodeWriter[Unit] = tag(tokenSeparatedCtx(takeTokens(IdentifiableTokens, {
    case JavaSToken(TOKEN, "this") => keyword("this")
    case JavaSToken(SUPER | CLASS, v) => keyword(v)
    case JavaSToken(_, v) => v
  }).print(), DOT), "identifier")

  def tokenSeparatedCtx(chosenParser: CodeWriter[Unit], enum: JavaTokenEnum, requireSpace: Boolean = false): CodeWriter[Unit] = tag({
    def loop: CodeWriter[Unit] = tag(for {
      _ <- assertToken(enum).tell(s"${enum.value}" + (if (requireSpace) " " else ""))
      res <- tokenSeparatedCtx(chosenParser, enum, requireSpace = requireSpace)
    } yield res, "loop")

    for {
      _ <- chosenParser
      _ <- loop || none
    } yield ()
  }, s"tokenSeparatedCtx($enum)")
}