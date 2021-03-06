package co.gyeongmin.lang.javadoc.codeformatter

import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javalang.JavaTokenEnum
import co.gyeongmin.lang.javalang.JavaTokenEnum._

import scala.language.postfixOps

object JavaParser {

  import Helper._
  import co.gyeongmin.lang.javadoc.codeformatter.syntax._

  val ModifierTokens: Set[JavaTokenEnum] =
    Set(
      VOLATILE,
      TRANSIENT,
      PUBLIC,
      PRIVATE,
      PROTECTED,
      STRICTFP,
      FINAL,
      ABSTRACT,
      STATIC,
      DEFAULT,
      SYNCHRONIZED
    )

  val ModifierStartToken: Set[JavaTokenEnum] =
    ModifierTokens + ANNOTATION
  // LL(1) parser
  private val PrimitiveTypeTokens: Set[JavaTokenEnum] =
    Set(
      PRIMITIVE_BYTE,
      PRIMITIVE_CHAR,
      PRIMITIVE_SHORT,
      PRIMITIVE_LONG,
      PRIMITIVE_INT,
      PRIMITIVE_FLOAT,
      PRIMITIVE_DOUBLE,
      PRIMITIVE_BOOLEAN
    )
  private val ControlStatementBeginTokens: Set[JavaTokenEnum] =
    Set(
      LBRACE,
      SEMICOLON,
      SWITCH,
      DO,
      BREAK,
      CONTINUE,
      RETURN,
      FOR,
      IF,
      WHILE,
      SYNCHRONIZED,
      THROW,
      TRY
    )
  private val JavaValueTypeTokens: Set[JavaTokenEnum] =
    Set(STRING, NUMBER, CHAR, TRUE, FALSE, NULL)
  private val JavaReferableTokens: Set[JavaTokenEnum] = TOKEN | THIS | SUPER
  private val IdentifiableTokens: Set[JavaTokenEnum] = Set(TOKEN)
  private val UnaryStartable: Set[JavaTokenEnum] =
    Set(PLUS, MINUS, NEGATE, EXCLAMATION_MARK)
  private val ExpressionStartable =
    IdentifiableTokens ++ JavaValueTypeTokens ++ JavaReferableTokens ++
      PrimitiveTypeTokens ++ UnaryStartable + (INC, DEC, NEW, LEFT_PARENTHESIS)

  import cats.syntax._
  import flatMap._
  import functor._

  private val SubstitutionTokens: Set[JavaTokenEnum] =
    SUBSTITUTE | PLUS_ACC | MINUS_ACC | MULTIPLY_ACC | DIVIDE_ACC | BIT_OR_ACC | BIT_AND_ACC | BIT_XOR_ACC

  def javaCode: CodeWriter[Unit] = tag(
    for {
      _ <- packageDefinition.hint(PACKAGE | ANNOTATION)
      _ <- enter ~ imports.hint(IMPORT) || none
      _ <- empty || (for {
        _ <- enter ~ symbolLoop(for {
          _ <- modifiers || none
          _ <- classDefinition.hint(CLASS) ||
            enumDefinition.hint(ENUM) ||
            interfaceDefinition.hint(INTERFACE) ||
            annotationInterfaceDefinition.hint(ANNOTATION_INTERFACE)
        } yield ())
      } yield ())
    } yield (),
    "javaCode"
  )

  def classDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- takeToken(CLASS).print(x => keyword(s"$x "))
      _ <- identifier || fail("identifier should be come after class")
      _ <- typeParameters.hint(LT) || none
      _ <- superClass.hint(EXTENDS) || none
      _ <- superInterface.hint(IMPLEMENTS) || none
      _ <- classBodyDefinition
      _ <- enter
    } yield (),
    "classDefDetail"
  )

  def interfaceDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(INTERFACE).tell(keyword("interface "))
      _ <- identifier || fail("expected identifier")
      _ <- typeParameters || none
      _ <- superInterfaceExtends || none
      _ <- interfaceBody
      _ <- enter
    } yield (),
    "interfaceDefinition"
  )

  def annotationInterfaceDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(ANNOTATION_INTERFACE)
        .tell(color("@", "yellow"))
        .tell(keyword("interface "))
      _ <- identifier
      _ <- annotationBody
    } yield (),
    "annotationInterfaceDefinition"
  )

  def annotationBody: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LBRACE).tell(" {").enter().tab()
      _ <- symbolLoop(annotationBodyDetail) || none
      _ <- assertToken(RBRACE).untab().tell("}").enter()
    } yield (),
    "annotationBody"
  )

  def annotationBodyDetail: CodeWriter[Unit] = tag(
    for {
      _ <- modifiers || none
      _ <- annotationTypeElementDeclaration ||
        constantDeclaration ||
        classDefinition ||
        interfaceDefinition ||
        annotationInterfaceDefinition
    } yield (),
    "annotationBodyDetail"
  )

  def annotationTypeElementDeclaration: CodeWriter[Unit] = tag(
    for {
      _ <- typeUse
      _ <- identifier
      _ <- symbolLoop(emptyArrayBoxes) || none
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
      _ <- defaultValue || none
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield (),
    "annotationTypeElementDeclaration"
  )

  def defaultValue: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(DEFAULT).tell(keyword("default "))
      _ <- tokenSeparatedLoop(referableValue, DOT) || arrayInitializer
    } yield (),
    "defaultValue"
  )

  def superInterfaceExtends: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(EXTENDS).tell(keyword("implements "))
      _ <- tokenSeparatedLoop(typeUse, COMMA, space)
    } yield (),
    "superInterface"
  )

  def interfaceBody: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LBRACE).tell("{").enter().tab()
      _ <- symbolLoop(interfaceBodyDeclaration.enter()).notHint(RBRACE) || none
      _ <- assertToken(RBRACE).untab().tell("}").enter()
    } yield (),
    "interfaceBody"
  )

  def interfaceBodyDeclaration: CodeWriter[Unit] = tag(
    for {
      _ <- modifiers || none
      _ <- interfaceMethodDeclaration ||
        constantDeclaration ||
        classDefinition || interfaceDefinition
    } yield (),
    "interfaceDeclaration"
  )

  def constantDeclaration: CodeWriter[Unit] =
    tag(declDetail ~ assertToken(SEMICOLON).tell(";"), "constantDeclaration")

  def interfaceMethodDeclaration: CodeWriter[Unit] = tag(
    for {
      _ <- generic || none
      _ <- methodHeader
      _ <- enter
    } yield (),
    "interfaceMethodDeclaration"
  )

  def methodHeader: CodeWriter[Unit] = tag(
    for {
      _ <- typeUse.tell(" ")
      _ <- identifier
      _ <- methodDefinition
    } yield (),
    "methodHeader"
  )

  def classBodyDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LBRACE).tell(" {").enter().tab()
      _ <- symbolLoop(definitionElements.enter()).notHint(RBRACE) || none
      _ <- assertToken(RBRACE).untab().tell("}")
    } yield (),
    "classBodyDefinition"
  )

  def staticBlockStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(STATIC).tell(keyword("static "))
      _ <- blockStmt.enter()
    } yield (),
    "staticBodyStmt"
  )

  def enumDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- takeToken(ENUM).print(x => keyword(s"$x "))
      _ <- identifier || fail("expected identifier")
      _ <- superInterface.hint(IMPLEMENTS) || none
      _ <- enumBodyDefinition || fail("expected enum body definition")
      _ <- enter
    } yield (),
    "enumDefinition"
  )

  def enumBodyDefinition: CodeWriter[Unit] = tag(
    {
      def enumBodyDetail: CodeWriter[Unit] = tag(
        for {
          _ <- enumBody
          _ <- (for {
            _ <- assertToken(SEMICOLON).tell(";").enter()
            _ <- symbolLoop(definitionElements.enter()).notHint(RBRACE) || none
          } yield ()) || none
        } yield (),
        "enumBodyDetail"
      )

      for {
        _ <- assertToken(LBRACE).tell(" {").enter().tab()
        _ <- enumBodyDetail || none
        _ <- assertToken(RBRACE).untab().tell("}")
      } yield ()
    },
    "enumBodyDefinition"
  )

  def enumBody: CodeWriter[Unit] = tag(
    tokenSeparatedLoop(
      for {
        _ <- symbolLoop(annotation) || none
        _ <- identifier
        _ <- enumParameters || none
        _ <- classBodyDefinition || none
      } yield (),
      COMMA,
      enter
    ),
    "enumBody"
  )

  def enumParameters: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- tokenSeparatedLoop(
        tokenSeparatedLoop(referableValue, DOT),
        COMMA,
        space
      )
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    } yield (),
    "enumParameters"
  )

  def definitionElements: CodeWriter[Unit] = tag(
    staticBlockStmt || (for {
      _ <- modifiers || none
      _ <-
        classDefinition || enumDefinition || constructorDef || classMemberDefinition || interfaceDefinition
    } yield ()),
    "definitionElements"
  )

  def classMemberDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- typeParameters.tell(" ") || none
      _ <- memberDefinition
    } yield (),
    "classMemberDefinition"
  )

  def constructorDef: CodeWriter[Unit] = tag(
    for {
      _ <- identifier
      _ <- methodDefinition
    } yield (),
    "constructorDef"
  )

  def packageDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- symbolLoop(annotation) || none
      _ <- assertToken(PACKAGE).tell(keyword("package "))
      _ <- tokenSeparatedLoop(identifier, DOT)
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield (),
    "packageDefinition"
  )

  def imports: CodeWriter[Unit] = tag(
    symbolLoop(for {
      _ <- assertToken(IMPORT).tell(keyword("import "))
      _ <- assertToken(STATIC).tell(keyword("static ")) || none
      _ <- tokenSeparatedLoop(identifier, DOT)
      _ <- (assertToken(DOT) ~ assertToken(MULTIPLY).tell("*")) || none
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield ()),
    "symbolLoop"
  )

  def blockStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LBRACE).tell(block("{")).enter().tab()
      _ <- blockStmts.notHint(RBRACE) || none
      _ <- assertToken(RBRACE).untab().tell(block("}"))
    } yield (),
    "blockStmt"
  ).foldable

  def statements: CodeWriter[Unit] = tag(
    (for {
      _ <- statement
      _ <- statements || none
    } yield ()) || none,
    "statements"
  )

  def statement: CodeWriter[Unit] = tag(
    controlStatement.hint(ControlStatementBeginTokens) ||
      nonControlStatement.notHint(ControlStatementBeginTokens),
    "statement"
  )

  def controlStatement: CodeWriter[Unit] = tag(
    blockStmt.hint(LBRACE) ||
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
      tryStmt.hint(TRY),
    "controlStatement"
  )

  def nonControlStatement: CodeWriter[Unit] =
    tag(expressionStmt || declarationStmt, "nonControlStatement")

  def unaryStmt(token: JavaTokenEnum): CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(token).tell(keyword(token.value))
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield (),
    s"unaryStmt($token)"
  )

  def expressionStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assignment || expression.hint(ExpressionStartable)
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield (),
    "expressionStmt"
  )

  def assignment: CodeWriter[Unit] = tag(
    for {
      _ <- tokenSeparatedLoop(reference.notHint(SubstitutionTokens), DOT)
      _ <- takeToken(SubstitutionTokens).print(x => s" $x ")
      _ <- expression
    } yield (),
    "assignment"
  ).hint(TOKEN | SUPER | THIS)

  def arrayRefs: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LBRACKET).tell("[")
      _ <- expression
      _ <- assertToken(RBRACKET).tell("]")
      _ <- arrayRefs || none
    } yield (),
    "arrayRefs"
  )

  def preExpression: CodeWriter[Unit] = tag(
    for {
      _ <- takeToken(INC | DEC).print()
      _ <- tokenSeparatedLoop(reference, DOT)
    } yield (),
    "preExpression"
  )

  def postExpression: CodeWriter[Unit] = tag(
    for {
      _ <- tokenSeparatedLoop(reference, DOT)
      _ <- takeToken(INC | DEC).print()
    } yield (),
    "postExpression"
  )

  def parameters: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- tokenSeparatedLoop(expression, COMMA, space) || none
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
      _ <- classBodyDefinition || none
    } yield (),
    "parameters"
  )

  def modifiers: CodeWriter[Unit] = tag(
    for {
      _ <- annotation.enter() ||
        takeToken(
          PUBLIC | PRIVATE | PROTECTED | STRICTFP | FINAL | ABSTRACT | STATIC | TRANSIENT | VOLATILE | DEFAULT | SYNCHRONIZED
        ).print(x => keyword(s"$x "))
      _ <- modifiers || none
    } yield (),
    "modifiers"
  ).hint(ModifierStartToken)

  def annotation: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(ANNOTATION).tell(color("@", "yellow"))
      _ <- tokenSeparatedLoop(takeToken(TOKEN).print(color(_, "yellow")), DOT)
      _ <- annotationTail || none
    } yield (),
    "annotation"
  )

  def annotationTail: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- tokenSeparatedLoop(
        annotationValuePairs,
        COMMA,
        space
      ) || annotationSingleValue || none
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    } yield (),
    "annotationTail"
  )

  def annotationSingleValue: CodeWriter[Unit] =
    tag(
      conditionalExpression || arrayInitializer || annotation,
      "annotationSingleValue"
    )

  def annotationValuePairs: CodeWriter[Unit] = tag(
    for {
      _ <- identifier
      _ <- assertToken(SUBSTITUTE).tell(" = ")
      _ <- annotationSingleValue
    } yield (),
    "annotationValuePairs"
  )

  def typeParameters: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LT).tell("<")
      _ <- tokenSeparatedLoop(typeVariable, COMMA, space)
      _ <- assertToken(GT).tell(">") || unrollingRightShift.tell(">")
    } yield (),
    "typeParameters"
  )

  def typeVariable: CodeWriter[Unit] = tag(
    for {
      _ <- annotation || none
      _ <- identifier
      _ <- typeBound || none
    } yield (),
    "typeVariable"
  )

  def typeBound: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(EXTENDS).tell("extends ")
      _ <- typeUse
      _ <- additionalBound || none
    } yield (),
    "typeBound"
  )

  def additionalBound: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(BIT_AND).tell(" & ")
      _ <- identifier
    } yield (),
    "additionalBound"
  )

  def superClass: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(EXTENDS).tell(keyword(" extends "))
      _ <- tokenSeparatedLoop(typeUse, COMMA, space) || fail(
        "type should be come after extends keyword"
      )
    } yield (),
    "superClass"
  )

  def superInterface: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(IMPLEMENTS).tell(keyword(" implements "))
      _ <- tokenSeparatedLoop(typeUse, COMMA, space) || fail(
        "type should be come after implements keyword"
      )
    } yield (),
    "superInterface"
  )

  def memberDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- typeUse.tell(" ")
      _ <- identifier
      _ <- fieldDefinition || methodDefinition
    } yield (),
    "memberDefinition"
  )

  def fieldDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- variableInitialize || none
      _ <- takeToken(SEMICOLON).tell(";")
    } yield (),
    "fieldDefinition"
  )

  def methodDefinition: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- tokenSeparatedLoop(methodArgDef, COMMA, space) || none
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- methodThrows || none
      _ <- blockStmt || assertToken(SEMICOLON).tell(";").enter()
    } yield (),
    "methodDefinition"
  )

  def methodThrows: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(THROWS).tell(keyword("throws "))
      _ <- tokenSeparatedLoop(typeUse, COMMA, space)
    } yield (),
    "methodThrows"
  )

  def methodArgDef: CodeWriter[Unit] = tag(
    for {
      _ <- annotation.hint(ANNOTATION) || none
      _ <- assertToken(FINAL).tell(keyword("final ")).hint(FINAL) || none
      _ <- typeUse
      _ <- space
      _ <- identifier
    } yield (),
    "methodArgDef"
  )

  def classInstanceCreation: CodeWriter[Unit] = tag(
    {
      def classInstance: CodeWriter[Unit] = tag(
        for {
          _ <- parameters
          _ <- classBodyDefinition || none
        } yield (),
        "classInstance"
      )

      def arrayInstance: CodeWriter[Unit] = tag(
        for {
          _ <- assertToken(LBRACKET).tell("[")
          _ <- expression || none
          _ <- assertToken(RBRACKET).tell("]")
          _ <- symbolLoop(emptyArrayBoxes) || none
          _ <- arrayInitializer || none
        } yield (),
        "arrayInstance"
      )

      def newInstanceReferable: CodeWriter[Unit] = tag(
        for {
          _ <- assertToken(DOT).tell(".")
          _ <- tokenSeparatedLoop(reference, DOT)
        } yield (),
        "newInstanceReferable"
      )

      for {
        _ <- assertToken(NEW).tell(keyword("new "))
        _ <- primitiveTypes.hint(PrimitiveTypeTokens) || tokenSeparatedLoop(
          takeToken(TOKEN).print(typeNameCss),
          DOT
        )
        _ <- generic.hint(LT) || none
        _ <- arrayInstance.hint(Set(LBRACKET, LBRACE)) || classInstance.hint(
          LEFT_PARENTHESIS
        )
        _ <- newInstanceReferable || none
      } yield ()
    },
    "classInstanceCreation"
  )

  def continueStmt: CodeWriter[Unit] = tag(unaryStmt(CONTINUE), "continueStmt")

  def breakStmt: CodeWriter[Unit] = tag(unaryStmt(BREAK), "breakStmt")

  def throwStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(THROW).tell(keyword("throw "))
      _ <- expression
    } yield (),
    "throwStmt"
  )

  def emptyStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield (),
    "emptyStmt"
  )

  def tryStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(TRY).tell(keyword("try "))
      _ <- tryDeclaration || none
      _ <- blockStmt || fail("block statement expected")
      _ <- catchStmts || none
      _ <- finallyStmt || none
      _ <- enter
    } yield (),
    "tryStmt"
  )

  def tryDeclaration: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- declaration
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    } yield (),
    "tryDeclaration"
  )

  def catchStmts: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(CATCH).tell(keyword(" catch "))
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- catchDeclaration
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt
      _ <- catchStmts || none
    } yield (),
    "catchStmt"
  )

  def catchDeclaration: CodeWriter[Unit] = tag(
    for {
      _ <- infixOperator(BIT_OR, typeUse).tell(" ")
      _ <- identifier
    } yield (),
    "catchDeclaration"
  )

  def blockStmts: CodeWriter[Unit] = tag(
    for {
      _ <- statement
      _ <- blockStmts.notHint(RBRACE) || none
    } yield (),
    "blockStmts"
  )

  def primitiveTypes: CodeWriter[Unit] =
    tag(takeToken(PrimitiveTypeTokens).print(keyword), "primitiveTypes")

  def finallyStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(FINALLY).tell(keyword(" finally "))
      _ <- blockStmt
    } yield (),
    "finallyStmt"
  )

  def returnStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(RETURN).tell(keyword("return "))
      _ <- expression || none
    } yield (),
    "returnStmt"
  )

  def forStmt: CodeWriter[Unit] = tag(
    {
      def forConditionStmt: CodeWriter[Unit] = tag(
        {
          def triExpressions: CodeWriter[Unit] = tag(
            for {
              _ <- declaration || expression || none
              _ <- assertToken(SEMICOLON).tell("; ")
              _ <- conditionalExpression
              _ <- assertToken(SEMICOLON).tell("; ")
              _ <- tokenSeparatedLoop(expression, COMMA, space) || none
            } yield (),
            "triExpressions"
          )

          def simpleTypes: CodeWriter[Unit] = tag(
            for {
              _ <- typeUse.tell(" ") || none
              _ <- identifier
              _ <- assertToken(COLON).tell(": ")
              _ <- expression
            } yield (),
            "simpleTypes"
          )

          triExpressions || simpleTypes
        },
        "forConditionStmt"
      )

      for {
        _ <- assertToken(FOR).tell(keyword("for "))
        _ <- assertToken(LEFT_PARENTHESIS).tell("(")
        _ <- forConditionStmt
        _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
        _ <- blockStmt.enter() || statement
      } yield ()
    },
    "forStmt"
  )

  def ifStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(IF).tell(keyword("if "))
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- expression
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt.tell(" ") || statement
      _ <- elseIfStmts || none
      _ <- elseStmt || none
    } yield (),
    "ifStmt"
  ).enter()

  def elseIfStmts: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(ELSE)
      _ <- assertToken(IF).tell(keyword("else if "))
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- expression
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt.tell(" ") || statement.enter()
      _ <- elseIfStmts || none
    } yield (),
    "elseIfStmts"
  )

  def elseStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(ELSE).tell(keyword("else "))
      _ <- blockStmt || statement
    } yield (),
    "elseStmt"
  )

  def whileStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(WHILE).tell(keyword("while "))
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- expression
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt || statement
      _ <- enter
    } yield (),
    "whileStmt"
  )

  //noinspection MutatorLikeMethodIsParameterless
  def doStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(DO).tell(keyword("do "))
      _ <- blockStmt.tell(" ")
      _ <- assertToken(WHILE).tell(keyword("while "))
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- assignment || expression.hint(ExpressionStartable)
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield (),
    "doStmt"
  )

  def switchStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(SWITCH).tell(keyword("switch "))
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- expression
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- caseBlock
    } yield (),
    "switchStmt"
  )

  def caseBlock: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LBRACE).tell(block("{")).enter().tab()
      _ <- caseStmts || none
      _ <- defaultStmt || none
      _ <- assertToken(RBRACE).tell(block("}")).untab().enter()
    } yield (),
    "caseBlock"
  ).foldable

  def caseStmts: CodeWriter[Unit] = tag(
    for {
      _ <- caseStmtDetail
      _ <- caseStmts || none
    } yield (),
    "caseStmts"
  )

  def defaultStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(DEFAULT).tell(keyword("default"))
      _ <- assertToken(COLON).tell(":").enter().tab()
      _ <- statements.untab()
    } yield (),
    "defaultStmt"
  )

  def caseStmtDetail: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(CASE).tell(keyword("case "))
      _ <- valueTypes || tokenSeparatedLoop(identifier, DOT)
      _ <- assertToken(COLON).tell(":").enter().tab()
      _ <- statements.untab()
    } yield (),
    "caseStmtDetail"
  )

  def expression: CodeWriter[Unit] =
    tag(lambda || assignment || conditionalExpression, "expression")

  def conditionalExpression: CodeWriter[Unit] = tag(
    {
      def conditionalExpressionDetail: CodeWriter[Unit] = tag(
        for {
          _ <- assertToken(QUESTION_MARK).tell(" ? ")
          _ <- expression
          _ <- assertToken(COLON).tell(" : ")
          _ <- expression || lambda
        } yield (),
        "conditionalExpressionDetail"
      )

      for {
        _ <- conditionalOrExpression
        _ <- conditionalExpressionDetail || none
      } yield ()
    },
    "conditionalExpression"
  ).hint(ExpressionStartable)

  def infixOperator(
    operator: JavaTokenEnum,
    next: CodeWriter[Unit]
  ): CodeWriter[Unit] = tag(
    {
      def infixOperatorDetail: CodeWriter[Unit] = tag(
        for {
          _ <- takeToken(operator).print(x => s" $x ")
          _ <- infixOperator(operator, next)
        } yield (),
        "infixOperatorDetail"
      )

      for {
        _ <- next
        _ <- infixOperatorDetail || none
      } yield ()
    },
    s"infixOperator($operator)"
  )

  def infixOperators(
    operators: Set[JavaTokenEnum],
    next: CodeWriter[Unit]
  ): CodeWriter[Unit] = tag(
    {
      def infixOperatorsDetail: CodeWriter[Unit] = tag(
        for {
          _ <- takeToken(operators).print(x => s" $x ")
          _ <- infixOperators(operators, next)
        } yield (),
        "infixOperatorsDetail"
      )

      for {
        _ <- next
        _ <- infixOperatorsDetail || none
      } yield ()
    },
    s"infixOperators(${operators.mkString(" | ")})"
  )

  def conditionalOrExpression: CodeWriter[Unit] =
    tag(infixOperator(OR, conditionalAndExpression), "conditionalOrExpression")

  def conditionalAndExpression: CodeWriter[Unit] =
    tag(infixOperator(AND, inclusiveOrExpression), "conditionalAndExpression")

  def inclusiveOrExpression: CodeWriter[Unit] =
    tag(infixOperator(BIT_OR, exclusiveOrExpression), "inclusiveOrExpression")

  def exclusiveOrExpression: CodeWriter[Unit] =
    tag(infixOperator(BIT_XOR, andExpression), "exclusiveOrExpression")

  def andExpression: CodeWriter[Unit] =
    tag(infixOperator(BIT_AND, equalityExpression), "andExpression")

  def equalityExpression: CodeWriter[Unit] = tag(
    infixOperators(EQUAL | NOT_EQUAL, relationalExpression),
    "equalityExpression"
  )

  def relationalExpression: CodeWriter[Unit] = tag(
    for {
      _ <- shiftExpression
      _ <- shiftTail || instanceOfTail || none
    } yield (),
    "relationalExpression"
  )

  def shiftTail: CodeWriter[Unit] = tag(
    for {
      _ <- takeToken(LT | GT | LTE | GTE).print(x => s" $x ")
      _ <- relationalExpression
    } yield (),
    "shiftTail"
  )

  def instanceOfTail: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(INSTANCEOF).tell(keyword(" instanceof "))
      _ <- typeUse
    } yield (),
    "instanceOfTail"
  )

  def shiftExpression: CodeWriter[Unit] =
    tag(
      infixOperators(
        LEFT_SHIFT | RIGHT_SHIFT | U_RIGHT_SHIFT,
        additiveExpression
      ),
      "shiftExpression"
    )

  def additiveExpression: CodeWriter[Unit] =
    tag(
      infixOperators(PLUS | MINUS, multiplicativeExpression),
      "additiveExpression"
    )

  def multiplicativeExpression: CodeWriter[Unit] =
    tag(
      infixOperators(MULTIPLY | DIVIDE | MODULAR, unaryExpression),
      "multiplicativeExpression"
    )

  def unaryExpression: CodeWriter[Unit] = tag(
    for {
      _ <- castExpression ||
        preExpression.hint(INC | DEC) ||
        postExpression.hint(JavaReferableTokens) || unaryExpWith(
          UnaryStartable
        ) ||
        classInstanceCreation.hint(NEW) ||
        tokenSeparatedLoop(referableValue, DOT).hint(
          PrimitiveTypeTokens ++ IdentifiableTokens ++ JavaValueTypeTokens ++ JavaReferableTokens + LEFT_PARENTHESIS
        )
    } yield (),
    "unaryExpression"
  ).hint(ExpressionStartable)

  def referableValue: CodeWriter[Unit] = tag(
    for {
      _ <- parenthesisExpression || typeAsValue || reference || valueTypes
      _ <- arrayRefs || none
    } yield (),
    "referableValue"
  )

  def typeAsValue: CodeWriter[Unit] = tag(
    for {
      _ <- tokenSeparatedLoop(primitiveTypesArrType || typeUse, DOT)
      _ <- assertToken(DOT).tell(".")
      _ <- assertToken(CLASS).tell(keyword("class"))
    } yield (),
    "typeAsValue"
  )

  def primitiveTypesArrType: CodeWriter[Unit] = tag(
    for {
      _ <- primitiveTypes
      _ <- arrayUse || none
    } yield (),
    "primitiveTypesArrType"
  )

  def parenthesisExpression: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- expression
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
    } yield (),
    "parenthesisExpression"
  )

  def valueTypes: CodeWriter[Unit] = tag(
    takeToken(TRUE | FALSE | NULL).print(keyword) ||
      takeToken(STRING | CHAR).print(color(_, "#477944")) || (for {
        _ <- assertToken(MINUS).tell("-") || none
        _ <- takeToken(NUMBER).print(color(_, "#3986AF"))
      } yield ()),
    "valueTypes"
  )

  def castExpression: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- typeUse
      _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
      _ <- lambda || unaryExpression
    } yield (),
    "castExpression"
  ).hint(LEFT_PARENTHESIS)

  def unaryExpWith(enums: Set[JavaTokenEnum]): CodeWriter[Unit] = tag(
    for {
      _ <- takeToken(enums).print()
      _ <- unaryExpression
    } yield (),
    s"unaryExpWith($enums)"
  ).hint(enums)

  def customReference: CodeWriter[Unit] = tag(
    (for {
      _ <- identifier
      _ <- arrayRefs || none
    } yield ()) || takeToken(THIS | SUPER).print(keyword),
    "customReference"
  )

  def reference: CodeWriter[Unit] = tag(
    for {
      _ <- generic || none
      _ <- customReference
      _ <- parameters || none
    } yield (),
    "reference"
  )

  def lambda: CodeWriter[Unit] = tag(
    {
      def lambdaDecl: CodeWriter[Unit] = tag(
        for {
          _ <- assertToken(LEFT_PARENTHESIS).tell("(")
          _ <- tokenSeparatedLoop(
            declaration || takeToken(TOKEN).print(x => s"$x"),
            COMMA,
            space
          ) || none
          _ <- assertToken(RIGHT_PARENTHESIS).tell(")")
        } yield (),
        "lambdaDecl"
      )

      def originLambda: CodeWriter[Unit] = tag(
        for {
          _ <- lambdaDecl || takeToken(TOKEN).print(x => s"$x")
          _ <- assertToken(LAMBDA_BODY_START).tell(" -> ")
          _ <- expression || blockStmt
        } yield (),
        "originLambda"
      )

      def shortenLambda: CodeWriter[Unit] = tag(
        for {
          _ <- tokenSeparatedLoop(referableValue, DOT, none, "typeUse") ||
            tokenSeparatedLoop(typeUse, DOT, none, "referableValue")
          _ <- assertToken(NAMESPACE).tell("::")
          _ <- assertToken(NEW).tell(keyword("new")) || identifier
        } yield (),
        "shortenLambda"
      )

      shortenLambda || originLambda
    },
    "lambda"
  ).hint(LEFT_PARENTHESIS | TOKEN | THIS | SUPER)

  def synchronizedStmt: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(SYNCHRONIZED).tell("synchronized")
      _ <- assertToken(LEFT_PARENTHESIS).tell("(")
      _ <- expression
      _ <- assertToken(RIGHT_PARENTHESIS).tell(") ")
      _ <- blockStmt.enter()
    } yield (),
    "synchronizedStmt"
  )

  def declarationStmt: CodeWriter[Unit] = tag(
    for {
      _ <- declaration
      _ <- assertToken(SEMICOLON).tell(";").enter()
    } yield (),
    "declarationStmt"
  )

  def declaration: CodeWriter[Unit] = tag(
    for {
      _ <- annotation.tell(" ") || none
      _ <- assertToken(FINAL).tell(keyword("final ")) || none
      _ <- declDetail
    } yield (),
    "declaration"
  )

  def declDetail: CodeWriter[Unit] = tag(
    {
      def declDetailLoop: CodeWriter[Unit] = tag(
        for {
          _ <- identifier
          _ <- variableInitialize.hint(SUBSTITUTE) || none
          _ <- (assertToken(COMMA).tell(",").enter() ~ declDetailLoop) || none
        } yield (),
        "declDetailLoop"
      )

      for {
        _ <- typeUse.tell(" ")
        _ <- declDetailLoop
      } yield ()
    },
    "declDetail"
  )

  def typeUse: CodeWriter[Unit] = tag(
    for {
      _ <- primitiveTypes.hint(PrimitiveTypeTokens) || customDecl
      _ <- arrayUse || none
      _ <- assertToken(ETC_ARRAY).tell("...") || none
    } yield (),
    "typeUse"
  )

  def customDecl: CodeWriter[Unit] = tag(
    for {
      _ <- tokenSeparatedLoop(takeToken(TOKEN).print(), DOT)
      _ <- generic || none
    } yield (),
    "customDecl"
  )

  def variableInitialize: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(SUBSTITUTE).tell(" = ")
      _ <- expression || arrayInitializer
    } yield (),
    "variableInitialize"
  )

  def arrayInitializer: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LBRACE).tell("{")
      _ <- tokenSeparatedLoop(
        arrayInitializer || expression,
        COMMA,
        space
      ) || none
      _ <- assertToken(RBRACE).tell("}")
    } yield (),
    "arrayInitializer"
  ).foldable

  def generic: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LT).tell("<")
      _ <- tokenSeparatedLoop(genericTypeUse, COMMA, space) || none
      _ <- (assertToken(GT) || unrollingRightShift).tell(">")
    } yield (),
    "generic"
  )

  def genericTypeUse: CodeWriter[Unit] = tag(
    for {
      _ <- typeUse || takeToken(QUESTION_MARK).print()
      _ <- genericTypeBound || none
    } yield (),
    "genericExtends"
  )

  def genericTypeBound: CodeWriter[Unit] = tag(
    for {
      _ <- takeToken(EXTENDS | SUPER).print(x => keyword(s" $x "))
      _ <- typeUse
    } yield (),
    "genericTypeBound"
  )

  def emptyArrayBoxes: CodeWriter[Unit] = tag(
    for {
      _ <- assertToken(LBRACKET).tell("[")
      _ <- assertToken(RBRACKET).tell("]")
    } yield (),
    "emptyArrayBoxes"
  )

  def arrayUse: CodeWriter[Unit] = tag(symbolLoop(emptyArrayBoxes), "arrayUse")

  def identifier: CodeWriter[Unit] = tag(takeToken(TOKEN).print(), "identifier")

  def tokenSeparatedLoop(
    chosenParser: CodeWriter[Unit],
    separator: JavaTokenEnum,
    afterToken: CodeWriter[Unit] = none,
    name: String = ""
  ): CodeWriter[Unit] = tag(
    for {
      _ <- chosenParser
      _ <- (takeToken(separator).print() ~ afterToken ~ tokenSeparatedLoop(
        chosenParser,
        separator,
        afterToken
      )) || none
    } yield (),
    s"tokenSeparatedLoop(${if (name.nonEmpty) s"$name, " else ""}$separator)"
  )
}
