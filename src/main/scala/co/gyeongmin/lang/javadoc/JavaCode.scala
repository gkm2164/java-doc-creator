package co.gyeongmin.lang.javadoc

import scala.language.{higherKinds, implicitConversions}
import cats.data._
import co.gyeongmin.lang.javalang.JavaTokenEnum
import co.gyeongmin.lang.javalang.JavaTokenEnum._
import co.gyeongmin.lang.javalang.exceptions.TokenNotAcceptedException
import levsha.Document.Node
import levsha.text.symbolDsl._


final case class JavaCode(packageName: String,
                          imports: Vector[String],
                          annotationsBuf: Vector[JavaAnnotationCall],
                          defs: Vector[JavaDefinition]) {
  def appendAnnotationBuf(annotation: JavaAnnotationCall): JavaCode = copy(annotationsBuf = annotationsBuf :+ annotation)

  def setPackageName(str: String): JavaCode = copy(packageName = str)

  def appendImport(imp: String): JavaCode = this.copy(imports = imports :+ imp)

  def appendDefinition(javaDefinition: JavaDefinition): JavaCode = this.copy(defs = defs :+ javaDefinition)

  def copyWithoutAnnotations: JavaCode = JavaCode(packageName, imports, Vector.empty, defs)

  import docgen._

  def show[T]: Node[T] = 'div(defs.sortBy(_.name).map(_.show(Indent(0))))
}

object JavaCode {
  type CodeState[A] = State[List[JavaSToken], A]
  type NextCodeState[A] = List[JavaSToken] => CodeState[A]

  def emptyCode: JavaCode = JavaCode("", Vector.empty, Vector.empty, Vector.empty)

  implicit def nextCodeStateToCodeStateConversion[A](nextCodeState: NextCodeState[A]): CodeState[A] =
    CodeState { tokens => nextCodeState(tokens).run(tokens).value }

  def parseAnnotationName(base: String): CodeState[String] = NextCodeState {
    case Nil => for {
      ret <- State.pure(base)
    } yield ret
    case JavaSToken(_, _) :: JavaSToken(DOT, _) :: _ => for {
      annName <- takeString
      _ <- assertToken(DOT)
      annotationName <- parseAnnotationName(base + s"$annName.")
    } yield annotationName
    case _ => for {
      annName <- takeString
    } yield base + annName
  }


  def parseAnnotation: State[List[JavaSToken], JavaAnnotationCall] = for {
    name <- parseAnnotationName("")
    defs <- parseParenthesis(LEFT_PARENTHESIS, RIGHT_PARENTHESIS)
  } yield JavaAnnotationCall(name, defs)

  def parseEnum(modifier: JavaModifier): CodeState[JavaEnumClass] = {
    def parseEnum(name: String, implements: Vector[JavaTypeUse]): CodeState[JavaEnumClass] = {
      def loop(acc: Vector[String]): CodeState[JavaEnumClass] = NextCodeState {
        case Nil => throw new TokenNotAcceptedException("state is empty")
        case JavaSToken(ANNOTATION, _) :: _ => for {
          _ <- assertToken(ANNOTATION)
          _ <- parseAnnotation
          cls <- loop(acc)
        } yield cls
        case JavaSToken(TOKEN, _) :: _ => for {
          tokenName <- takeString
          _ <- parseParenthesis(LEFT_PARENTHESIS, RIGHT_PARENTHESIS)
          _ <- parseClassBody
          enumClass <- parseEnumBody(tokenName, acc)
        } yield enumClass
        case h :: _ => throw new TokenNotAcceptedException(s"unknown token: $h")
      }

      def parseClassBody: CodeState[Unit] = NextCodeState {
        case JavaSToken(LBRACE, _) :: _ => for {
          _ <- assertToken(LBRACE)
          _ <- parseClassInside("", Vector.empty)
        } yield ()
        case _ => State.pure()
      }

      def parseEnumBody(tokenName: String, acc: Vector[String]): CodeState[JavaEnumClass] = NextCodeState {
        case Nil => throw new TokenNotAcceptedException("nil list")
        case JavaSToken(COMMA, _) :: JavaSToken(SEMICOLON, _) :: _ => for {
          _ <- assertToken(COMMA)
          _ <- assertToken(SEMICOLON)
          defs <- parseClassInside(modifier.fullPath, Vector.empty)
        } yield JavaEnumClass(name, modifier, acc, defs, implements)
        case JavaSToken(COMMA, _) :: _ => for {
          _ <- assertToken(COMMA)
          res <- loop(acc :+ tokenName)
        } yield res
        case JavaSToken(SEMICOLON, _) :: _ => for {
          _ <- assertToken(SEMICOLON)
          defs <- parseClassInside(modifier.fullPath, Vector.empty)
        } yield JavaEnumClass(name, modifier, acc :+ tokenName, defs, implements)
        case JavaSToken(RBRACE, _) :: _ => for {
          _ <- assertToken(RBRACE)
        } yield JavaEnumClass(name, modifier, acc :+ tokenName, Vector.empty, implements)
        case h :: _ => throw new TokenNotAcceptedException(s"not allowed token: $h")
      }

      loop(Vector.empty)
    }

    for {
      name <- takeString
      implements <- parseTypesFrom(IMPLEMENTS)
      _ <- assertToken(LBRACE)
      cls <- parseEnum(name, implements)
    } yield cls
  }

  // generic definition
  // <T extends U, V super K, W>

  def parseDefs(modifier: JavaModifier): CodeState[JavaDefinition] = NextCodeState[JavaDefinition] {
    case Nil => throw new TokenNotAcceptedException("token is empty")
    case JavaSToken(ANNOTATION, _) :: _ => for {
      _ <- assertToken(ANNOTATION)
      annotation <- parseAnnotation
      defs <- parseDefs(modifier.appendAnnotation(annotation))
    } yield defs
    case JavaSToken(testToken@(PRIVATE | PROTECTED | PUBLIC | VOLATILE | TRANSIENT | SYNCHRONIZED), _) :: _ => for {
      access <- assertTokens(testToken)
      defs <- parseDefs(modifier.setAccess(access))
    } yield defs
    case JavaSToken(LBRACE, _) :: _ => for {
      _ <- assertToken(LBRACE)
      _ <- parseParenthesisSkipHead(LBRACE, RBRACE)
      defs <- parseDefs(JavaModifier.empty(modifier.fullPath))
    } yield defs
    case JavaSToken(STATIC, _) :: JavaSToken(LBRACE, _) :: _ => for {
      _ <- assertToken(STATIC)
      _ <- assertToken(LBRACE)
      _ <- parseParenthesisSkipHead(LBRACE, RBRACE)
    } yield JavaDefinitionUnit
    case JavaSToken(STATIC, _) :: _ => for {
      _ <- assertToken(STATIC)
      defs <- parseDefs(modifier.setStatic)
    } yield defs
    case JavaSToken(LT, _) :: _ => for {
      genericDef <- parseGenerics
      defs <- parseDefs(modifier.setGeneric(genericDef))
    } yield defs
    case JavaSToken(FINAL, _) :: _ => for {
      _ <- assertToken(FINAL)
      defs <- parseDefs(modifier.setFinal)
    } yield defs
    case JavaSToken(ABSTRACT, _) :: _ => for {
      _ <- assertToken(ABSTRACT)
      defs <- parseDefs(modifier.setAbstract)
    } yield defs
    case JavaSToken(COMMENT_MACRO_CODE | COMMENT_MACRO_EXPLAIN | COMMENT_MACRO_NAME, v) :: _ => for {
      _ <- assertTokens(COMMENT_MACRO_CODE, COMMENT_MACRO_EXPLAIN, COMMENT_MACRO_NAME)
      defs <- parseDefs(modifier.appendMacro(v))
    } yield defs
    case JavaSToken(CLASS, _) :: _ => for {
      _ <- assertToken(CLASS)
      cls <- parseClass(modifier)
    } yield cls
    case JavaSToken(ENUM, _) :: _ => for {
      _ <- assertToken(ENUM)
      enum <- parseEnum(modifier)
    } yield enum
    case JavaSToken(INTERFACE, _) :: _ => for {
      _ <- assertToken(INTERFACE)
      interface <- parseInterface(modifier)
    } yield interface
    case JavaSToken(ANNOTATION_INTERFACE, _) :: _ => for {
      _ <- assertToken(ANNOTATION_INTERFACE)
      annotationInterface <- parseAnnotationInterface(modifier)
    } yield annotationInterface
    case JavaSToken(DEFAULT, _) :: _ => for {
      _ <- assertToken(DEFAULT)
      defs <- parseDefs(modifier)
    } yield defs
    case _ => for {
      members <- parseMembers(modifier)
    } yield members
  }

  def takeDotSeparated: CodeState[List[String]] = State(tokens => {
    def loop(acc: List[String]): CodeState[List[String]] = State {
      case Nil => (Nil, acc)
      case h :: JavaSToken(DOT, _) :: t =>
        loop(acc :+ h.value).run(t).value
      case h :: t =>
        State.pure(acc :+ h.value).run(t).value
    }

    loop(Nil).run(tokens).value
  })

  def parseArrayType(acc: String): CodeState[String] = State {
    case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: t =>
      parseArrayType(acc + "[]").run(t).value
    case JavaSToken(ETC_ARRAY, _) :: t =>
      parseArrayType(acc + "...").run(t).value
    case t =>
      State.pure(acc).run(t).value
  }

  def parseTypeDesignatorRemains(acc: Vector[JavaTypeDesignate]): CodeState[Vector[JavaTypeDesignate]] = CodeState {
    case JavaSToken(COMMA, _) :: t => (for {
      typeDesignate <- parseTypeDesignator
      remains <- parseTypeDesignatorRemains(acc :+ typeDesignate)
    } yield remains).run(t).value
    case t => State.pure(acc).run(t).value
  }

  def parseGenerics: CodeState[Vector[JavaTypeDesignate]] = NextCodeState[Vector[JavaTypeDesignate]] {
    case JavaSToken(LT, _) :: _ => for {
      _ <- assertToken(LT)
      typeDesignate <- parseTypeDesignator
      list <- parseTypeDesignatorRemains(Vector(typeDesignate))
      _ <- CodeState {
        case Nil => throw new TokenNotAcceptedException("list is Nil")
        case JavaSToken(GT, _) :: t => (t, ())
        case JavaSToken(RIGHT_SHIFT, _) :: t => (JavaSToken(GT, ">") :: t, ())
        case JavaSToken(U_RIGHT_SHIFT, _) :: t => (JavaSToken(RIGHT_SHIFT, ">>") :: t, ())
        case JavaSToken(v, _) :: _ => throw new TokenNotAcceptedException(s"expected >, >>, >>>, but $v")
      }
    } yield list
    case _ => State.pure(Vector.empty)
  }

  def parseTypeRelation: CodeState[Option[(String, JavaTypeDesignate)]] = NextCodeState[Option[(String, JavaTypeDesignate)]] {
    case JavaSToken(EXTENDS | SUPER, _) :: _ => for {
      relation <- takeString
      typeDesig <- parseTypeDesignator
    } yield Some((relation, typeDesig))
    case _ => State.pure(None)
  }

  def parseArrBrackets(acc: String): CodeState[String] = NextCodeState {
    case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: _ => for {
      _ <- assertToken(LBRACKET)
      _ <- assertToken(RBRACKET)
      _ <- parseArrBrackets(acc)
    } yield "[]" + acc
    case _ => State.pure(acc)
  }

  def parseTypeDesignator: CodeState[JavaTypeDesignate] = for {
    typename <- takeDotSeparated.map(_.mkString("."))
    extend <- parseTypeRelation
    generics <- parseGenerics
    arrNotations <- parseArrBrackets("")
  } yield JavaTypeDesignate(typename, extend, generics, arrNotations)

  def parseType: CodeState[JavaTypeUse] = for {
    typeDesignator <- parseTypeDesignator
    arrayNotations <- parseArrayType("")
  } yield JavaTypeUse(typeDesignator, arrayNotations)

  def takeToken[A](x: A): CodeState[A] = State(tokens => (tokens.tail, x)) // 토큰 한개를 흘려보냄

  def parseArgs: CodeState[Vector[JavaArgument]] = CodeState(tokens => {
    def parseArgName(typename: JavaTypeUse, arg: JavaArgument): CodeState[JavaArgument] =
      State(tokens => (tokens.tail, arg.copy(argumentType = typename, name = tokens.head.value)))

    def parseArg(javaArg: JavaArgument): CodeState[JavaArgument] = NextCodeState {
      case JavaSToken(ANNOTATION, _) :: _ => for {
        _ <- assertToken(ANNOTATION)
        annotation <- parseAnnotation
        arg <- parseArg(javaArg.appendAnnotation(annotation))
      } yield arg
      case JavaSToken(FINAL, _) :: _ => for {
        _ <- assertToken(FINAL)
        arg <- parseArg(javaArg.setFinal)
      } yield arg
      case _ => for {
        typename <- parseType
        argName <- parseArgName(typename, javaArg)
      } yield argName
    }

    def loop(acc: Vector[JavaArgument]): CodeState[Vector[JavaArgument]] = CodeState {
      case JavaSToken(RIGHT_PARENTHESIS, _) :: t =>
        State.pure(acc).run(t).value
      case JavaSToken(COMMA, _) :: t =>
        loop(acc).run(t).value
      case t => (for {
        arg <- parseArg(JavaArgument.empty)
        args <- loop(acc :+ arg)
      } yield args).run(t).value
    }

    loop(Vector.empty).run(tokens).value
  })

  def takeTokenOrElse(token: JavaTokenEnum, default: String): CodeState[String] = CodeState {
    case JavaSToken(tk, v) :: t if token == tk =>
      State.pure(v).run(t).value
    case t =>
      State.pure(default).run(t).value
  }


  def parseMembers(modifier: JavaModifier): CodeState[JavaMembers] = {
    def parseCodes(name: String): CodeState[Vector[JavaSToken]] = CodeState {
      case Nil => throw new TokenNotAcceptedException("nil list!")
      case JavaSToken(SEMICOLON, _) :: t => State.pure(Vector()).run(t).value
      case (tk@JavaSToken(LBRACE, _)) :: t => (for {
        codes <- parseParenthesisSkipHead(LBRACE, RBRACE)
      } yield codes).run(t).value
      case JavaSToken(DEFAULT, _) :: t => (for {
        _ <- parseUntil(SEMICOLON)
      } yield Vector()).run(t).value
      case JavaSToken(THROWS, _) :: t => (for {
        _ <- separateByType
        codes <- parseCodes(name)
      } yield codes).run(t).value
      case h :: _ => throw new TokenNotAcceptedException(s"member: ${h.toString}")
    }

    def parseMemberAfterName(modifier: JavaModifier, typename: JavaTypeUse, name: String): CodeState[JavaMembers] =
      NextCodeState[JavaMembers] {
        case Nil => throw new TokenNotAcceptedException("nil list!")
        case JavaSToken(LEFT_PARENTHESIS, _) :: _ => for {
          _ <- assertToken(LEFT_PARENTHESIS)
          args <- parseArgs
          codes <- parseCodes(name)
        } yield JavaMethod(modifier, name, typename, args, codes)
        case JavaSToken(SUBSTITUTE, _) :: _ => for {
          _ <- assertToken(SUBSTITUTE)
          _ <- parseUntilDepth(SEMICOLON, LBRACE, RBRACE)
        } yield JavaMember(modifier, name, typename)
        case JavaSToken(SEMICOLON, _) :: _ => for {
          _ <- assertToken(SEMICOLON)
        } yield JavaMember(modifier, name, typename)
        case h :: t => throw new TokenNotAcceptedException(s"$h, $t")
      }

    for {
      typename <- parseType
      name <- takeTokenOrElse(TOKEN, typename.name)
      after <- parseMemberAfterName(modifier, typename, name)
    } yield after
  }

  // LL(1)의 한계...ㅠㅠ, commaSeparatedType := type "," commaSeparatedType
  //                                       | type
  def separateByType: CodeState[Vector[JavaTypeUse]] = CodeState(tokens => {
    def loop(acc: Vector[JavaTypeUse]): CodeState[Vector[JavaTypeUse]] = for {
      typename <- parseType
      types <- NextCodeState {
        case JavaSToken(COMMA, _) :: _ => for {
          _ <- assertToken(COMMA)
          res <- loop(acc :+ typename)
        } yield res
        case _ => State.pure(acc :+ typename)
      }
    } yield types

    loop(Vector.empty).run(tokens).value
  })

  def parseClassInside(path: String, acc: Vector[JavaDefinition]): CodeState[Vector[JavaDefinition]] = NextCodeState {
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: _ => for {
      _ <- assertToken(RBRACE)
    } yield acc
    case _ => for {
      definition <- parseDefs(JavaModifier.empty(path))
      classInside <- parseClassInside(path, acc :+ definition)
    } yield classInside
  }

  def parseClass(modifier: JavaModifier): CodeState[JavaClass] = for {
    name <- takeString
    generics <- parseGenerics
    extendInh <- parseTypesFrom(EXTENDS)
    implementH <- parseTypesFrom(IMPLEMENTS)
    _ <- assertToken(LBRACE)
    defs <- parseClassInside(s"${modifier.fullPath}.$name", Vector.empty)
  } yield JavaClass(name, modifier.copy(generic = generics), defs, extendInh, implementH)

  def parseTypesFrom(targetToken: JavaTokenEnum): CodeState[Vector[JavaTypeUse]] = CodeState {
    case JavaSToken(token, _) :: t if token == targetToken =>
      separateByType.run(t).value
    case tails => (tails, Vector.empty)
  }

  def parseInterfaceDefs(path: String, acc: Vector[JavaDefinition]): CodeState[Vector[JavaDefinition]] = CodeState {
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: t => (t, acc)
    case t => (for {
      defs <- parseDefs(JavaModifier.empty(path))
      interfaceDef <- parseInterfaceDefs(path, acc :+ defs)
    } yield interfaceDef).run(t).value
  }

  def parseAnnotationInterface(modifier: JavaModifier): CodeState[JavaAnnotationInterface] = for {
    name <- takeString
    extendInh <- parseTypesFrom(EXTENDS)
    _ <- assertToken(LBRACE)
    defs <- parseInterfaceDefs(s"${modifier.fullPath}.$name", Vector.empty)
  } yield JavaAnnotationInterface(name, modifier, defs, extendInh)

  def parseInterface(modifier: JavaModifier): CodeState[JavaInterface] = for {
    name <- takeString
    generic <- parseGenerics
    extendInh <- parseTypesFrom(EXTENDS)
    _ <- assertToken(LBRACE)
    defs <- parseInterfaceDefs(s"${modifier.fullPath}.$name", Vector.empty)
  } yield JavaInterface(name, modifier.copy(generic = generic), defs, extendInh)

  def assertTokens(tokens: JavaTokenEnum*): CodeState[JavaTokenEnum] = CodeState {
    case JavaSToken(tk, _) :: t if tokens.contains(tk) => State.pure(tk).run(t).value
    case t => throw new TokenNotAcceptedException(s"${t.head.tokenType} => ${t.head.value}, ${t.tail.take(5)}")
  }

  def parseParenthesis(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): CodeState[String] = {
    def loop(cnt: Int, acc: String): CodeState[String] = NextCodeState {
      case Nil => for {
        res <- State.pure(acc)
      } yield res
      case JavaSToken(lp, _) :: _ if lp == leftPar => for {
        _ <- assertToken(lp)
        l <- loop(cnt + 1, acc + lp.value)
      } yield l
      case JavaSToken(rp, _) :: _ if cnt == 0 && rp == rightPar => for {
        _ <- assertToken(rp)
        res <- State.pure(acc + rp.value)
      } yield res
      case JavaSToken(rp, _) :: _ if rp == rightPar => for {
        _ <- assertToken(rp)
        res <- loop(cnt - 1, acc + rp.value)
      } yield res
      case _ => for {
        v <- takeString
        res <- loop(cnt, acc + v)
      } yield res
    }

    NextCodeState[String] {
      case JavaSToken(lp, _) :: _ if lp == leftPar => for {
        _ <- assertToken(lp)
        res <- loop(0, lp.value)
      } yield res
      case _ =>
        State.pure("")
    }
  }

  def parseParenthesisList(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): CodeState[Vector[String]] = {
    def loop(cnt: Int, works: String, acc: Vector[String]): CodeState[Vector[String]] = NextCodeState[Vector[String]] {
      case Nil => State.pure(acc :+ works)
      case JavaSToken(lp, _) :: _ if lp == leftPar => for {
        _ <- assertToken(lp)
        res <- loop(cnt + 1, works + lp.value, acc)
      } yield res
      case JavaSToken(rp, _) :: _ if cnt == 0 && rp == rightPar => for {
        _ <- assertToken(rp)
      } yield acc
      case JavaSToken(rp, _) :: _ if rp == rightPar => for {
        _ <- assertToken(rp)
        res <- loop(cnt - 1, "", acc :+ works + rp.value)
      } yield res
      case JavaSToken(_, _) :: _ => for {
        v <- takeString
        res <- loop(cnt, "", acc :+ v)
      } yield res
    }

    NextCodeState[Vector[String]] {
      case JavaSToken(lp, _) :: _ if lp == leftPar => for {
        _ <- assertToken(lp)
        res <- loop(0, "", Vector.empty)
      } yield res
      case _ => State.pure(Vector.empty)
    }
  }

  def takeString: CodeState[String] = CodeState(tokens => (tokens.tail, if (tokens.head.value.isEmpty) tokens.head.tokenType.value else tokens.head.value))

  def assertToken(token: JavaTokenEnum): CodeState[Unit] = CodeState {
    case JavaSToken(tk, _) :: t if tk == token => State.pure(()).run(t).value
    case t => throw new TokenNotAcceptedException(s"expected $token but ${t.head.tokenType} => ${t.head.value}, ${t.tail.take(5)}")
  }

  def parseParenthesisSkipHead(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): CodeState[Vector[JavaSToken]] = {
    def loop(cnt: Int, acc: Vector[JavaSToken]): CodeState[Vector[JavaSToken]] = NextCodeState[Vector[JavaSToken]] {
      case Nil => State.pure(acc)
      case (t@JavaSToken(lp, _)) :: _ if lp == leftPar => for {
        _ <- assertToken(lp)
        res <- loop(cnt + 1, acc :+ t)
      } yield res
      case (t@JavaSToken(rp, _)) :: _ if cnt == 0 && rp == rightPar => for {
        _ <- assertToken(rp)
      } yield acc :+ t
      case (t@JavaSToken(rp, _)) :: _ if rp == rightPar => for {
        _ <- assertToken(rp)
        res <- loop(cnt - 1, acc :+ t)
      } yield res
      case (t@JavaSToken(_, _)) :: _ => for {
        _ <- takeString
        res <- loop(cnt, acc :+ t)
      } yield res
    }

    loop(0, Vector(JavaSToken(leftPar, leftPar.value)))
  }

  def parseUntil(until: JavaTokenEnum): CodeState[String] = {
    def loop(acc: String): CodeState[String] = NextCodeState {
      case Nil => State.pure(acc)
      case JavaSToken(target, _) :: _ if target == until => for {
        _ <- assertToken(target)
      } yield acc
      case _ => for {
        h <- takeString
        res <- loop(acc + h)
      } yield res
    }

    loop("")
  }

  def parseUntilDepth(until: JavaTokenEnum, depthStart: JavaTokenEnum, depthEnd: JavaTokenEnum, depth: Int = 0): CodeState[String] = {
    def loop(acc: String): CodeState[String] = NextCodeState {
      case Nil => State.pure(acc)
      case JavaSToken(target, _) :: _ if depth == 0 && target == until => for {
        _ <- assertToken(target)
      } yield acc
      case JavaSToken(x, _) :: _ if x == depthStart => for {
        _ <- assertToken(x)
        res <- parseUntilDepth(until, depthStart, depthEnd, depth + 1)
      } yield res
      case JavaSToken(x, _) :: _ if x == depthEnd => for {
        _ <- assertToken(x)
        res <- parseUntilDepth(until, depthStart, depthEnd, depth - 1)
      } yield res
      case _ => for {
        h <- takeString
        res <- loop(acc + h)
      } yield res
    }

    loop("")
  }

  def parseCode(bowl: JavaCode): CodeState[JavaCode] = NextCodeState {
    case Nil => CodeState.pure(bowl)
    case JavaSToken(ANNOTATION, _) :: _ => for {
      _ <- assertToken(ANNOTATION)
      annotation <- parseAnnotation
      code <- parseCode(bowl.appendAnnotationBuf(annotation))
    } yield code
    case JavaSToken(PACKAGE, _) :: _ => for {
      _ <- assertToken(PACKAGE)
      pkgName <- parseUntil(SEMICOLON)
      code <- parseCode(bowl.setPackageName(pkgName).copyWithoutAnnotations)
    } yield code
    case JavaSToken(IMPORT, _) :: _ => for {
      _ <- assertToken(IMPORT)
      importName <- parseUntil(SEMICOLON)
      code <- parseCode(bowl.appendImport(importName).copyWithoutAnnotations)
    } yield code
    case _ => for {
      definition <- parseDefs(JavaModifier.empty(bowl.packageName, bowl.annotationsBuf))
      code <- parseCode(bowl.appendDefinition(definition).copyWithoutAnnotations)
    } yield code
  }

  def apply(tokens: List[JavaSToken]): JavaCode =
    parseCode(emptyCode).runA(tokens.filterNot(x => List(COMMENT, COMMENT_BLOCK).contains(x.tokenType))).value

  object NextCodeState {
    def apply[A](f: List[JavaSToken] => CodeState[A]): NextCodeState[A] = f
  }

  object CodeState {
    def pure[A](a: A): CodeState[A] = CodeState { xs => (xs, a) }

    def apply[A](f: List[JavaSToken] => (List[JavaSToken], A)): CodeState[A] = State(f)
  }

}
