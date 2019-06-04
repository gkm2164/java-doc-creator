package com.kakao.bengo.scala.javadoc

import com.kakao.bengo.godoc.exceptions.TokenNotAcceptedException
import com.kakao.bengo.javalang.JavaTokenEnum
import com.kakao.bengo.scala.functional.TokenListState
import levsha.Document.Node
import levsha.text.symbolDsl._

case class JavaCode(packageName: String,
                    imports: List[String],
                    annotationsBuf: List[JavaAnnotationCall],
                    defs: List[JavaDefinition]) {
  def appendAnnotationBuf(annotation: JavaAnnotationCall): JavaCode = copy(annotationsBuf = annotationsBuf :+ annotation)

  def setPackageName(str: String): JavaCode = copy(packageName = str)

  def appendImport(imp: String): JavaCode = this.copy(imports = imports :+ imp)

  def appendDefinition(javaDefinition: JavaDefinition): JavaCode = this.copy(defs = defs :+ javaDefinition)

  def copyWithoutAnnotations: JavaCode = JavaCode(packageName, imports, Nil, defs)

  import writer._

  def show[T]: Node[T] = 'div (defs.sortBy(_.name).map(_.show(Indent(0))))
}

object JavaCode {

  import com.kakao.bengo.javalang.JavaTokenEnum._
  import com.kakao.bengo.scala.functional.MonadSyntax._
  import com.kakao.bengo.scala.functional.TokenListStateBehavior._

  def emptyCode: JavaCode = JavaCode("", Nil, Nil, Nil)

  def parseAnnotationName(base: String): TokenListState[String] = TokenListState {
    case Nil => (base, Nil)
    case JavaSToken(DOT, _) :: JavaSToken(_, annName) :: t =>
      parseAnnotationName(base + s".$annName").run(t)
    case JavaSToken(_, n) :: t =>
      TokenListState.unit(base + n).run(t)
  }

  def parseAnnotation: TokenListState[JavaAnnotationCall] = for {
    name <- parseAnnotationName("")
    defs <- parseParenthesis(LEFT_PARENTHESIS, RIGHT_PARENTHESIS)
  } yield JavaAnnotationCall(name, defs)

  def parseEnum(modifier: JavaModifier): TokenListState[JavaEnumClass] = {
    def parseEnumInside(name: String, implements: List[JavaTypeUse]): TokenListState[JavaEnumClass] = TokenListState(state => {
      def loop(acc: List[String]): TokenListState[JavaEnumClass] = TokenListState {
        case JavaSToken(ANNOTATION, _) :: tail => (for {
          _ <- parseAnnotation
          cls <- loop(acc)
        } yield cls).run(tail)
        case JavaSToken(TOKEN, tokenName) :: tail =>
          (for {
          _ <- parseParenthesis(LEFT_PARENTHESIS, RIGHT_PARENTHESIS)
          enumClass <- TokenListState {
            case Nil => throw new TokenNotAcceptedException("nil list")
            case JavaSToken(COMMA, _) :: t =>
              loop(acc :+ tokenName).run(t)
            case JavaSToken(SEMICOLON, _) :: t => (for {
              defs <- parseClassInside(modifier.fullPath, Nil)
            } yield JavaEnumClass(name, modifier, acc :+ tokenName, defs, implements)).run(t)
            case JavaSToken(RBRACE, _) :: t =>
              TokenListState.unit(JavaEnumClass(name, modifier, acc :+ tokenName, Nil, implements)).run(t)
            case h :: _ => throw new TokenNotAcceptedException(s"not allowed token: $h")
          }
        } yield enumClass).run(tail)

        case t => throw new TokenNotAcceptedException(s"unknown token: ${t.head}")
      }

      loop(Nil).run(state)
    })

    for {
      name <- takeString
      implements <- parseTypesFrom(IMPLEMENTS)
      _ <- assertToken(LBRACE)
      cls <- parseEnumInside(name, implements)
    } yield cls
  }

  def parseDefs(modifier: JavaModifier): TokenListState[JavaDefinition] = TokenListState {
    case Nil => throw new TokenNotAcceptedException("token is empty")
    case JavaSToken(ANNOTATION, _) :: t =>
      println("seems to meet annotation")
      (for {
      annotation <- parseAnnotation
      defs <- parseDefs(modifier.appendAnnotation(annotation))
    } yield defs).run(t)
    case JavaSToken(access@(PRIVATE | PROTECTED | PUBLIC), _) :: t =>
      parseDefs(modifier.setAccess(access)).run(t)
    case JavaSToken(LBRACE, _) :: t => (for {
      _ <- parseParenthesisSkipHead(LBRACE, RBRACE)
      defs <- parseDefs(JavaModifier.empty(modifier.fullPath))
    } yield defs).run(t)
    case JavaSToken(STATIC, _) :: t =>
      parseDefs(modifier.setStatic).run(t)
    case JavaSToken(LT, _) :: t => (for {
      genericDef <- parseParenthesisSkipHead(LT, GT)
      defs <- parseDefs(modifier.setGeneric(genericDef))
    } yield defs).run(t)
    case JavaSToken(FINAL, _) :: t =>
      parseDefs(modifier.setFinal).run(t)
    case JavaSToken(ABSTRACT, _) :: t =>
      parseDefs(modifier.setAbstract).run(t)
    case JavaSToken(COMMENT_MACRO_CODE | COMMENT_MACRO_EXPLAIN | COMMENT_MACRO_NAME, v) :: t =>
      parseDefs(modifier.appendMacro(v)).run(t)
    case JavaSToken(CLASS, _) :: t =>
      parseClass(modifier).run(t)
    case JavaSToken(ENUM, _) :: t =>
      parseEnum(modifier).run(t)
    case JavaSToken(INTERFACE, _) :: t =>
      parseInterface(modifier).run(t)
    case JavaSToken(ANNOTATION_INTERFACE, _) :: t =>
      println(modifier)
      parseAnnotationInterface(modifier).run(t)
    case JavaSToken(DEFAULT, _) :: t => parseDefs(modifier).run(t)
    case tokens =>
      parseMembers(modifier).run(tokens)
  }

  def takeDotSeparated: TokenListState[List[String]] = TokenListState(tokens => {
    def loop(acc: List[String]): TokenListState[List[String]] = TokenListState {
      case Nil => (acc, Nil)
      case h :: JavaSToken(DOT, _) :: t =>
        loop(acc :+ h.value).run(t)
      case h :: t =>
        TokenListState.unit(acc :+ h.value).run(t)
    }

    loop(Nil).run(tokens)
  })

  def parseArrayType(acc: String): TokenListState[String] = TokenListState {
    case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: t =>
      parseArrayType(acc + "[]").run(t)
    case JavaSToken(ETC_ARRAY, _) :: t =>
      parseArrayType(acc + "...").run(t)
    case t =>
      TokenListState.unit(acc).run(t)
  }

  def parseTypeDesignatorRemains(acc: List[JavaTypeDesignate]): TokenListState[List[JavaTypeDesignate]] = TokenListState {
    case JavaSToken(COMMA, _) :: t => (for {
      typeDesignate <- parseTypeDesignator
      remains <- parseTypeDesignatorRemains(acc :+ typeDesignate)
    } yield remains).run(t)
    case t => TokenListState.unit(acc).run(t)
  }

  def parseGenerics: TokenListState[List[JavaTypeDesignate]] = TokenListState {
    case JavaSToken(LT, _) :: t => (for {
      typeDesignate <- parseTypeDesignator
      list <- parseTypeDesignatorRemains(List(typeDesignate))
      _ <- assertToken(GT)
    } yield list).run(t)
    case t => TokenListState.unit(Nil).run(t)
  }

  def parseTypeRelation: TokenListState[Option[(String, String)]] = TokenListState {
    case JavaSToken(EXTENDS, _) :: JavaSToken(TOKEN, v) :: t => TokenListState.unit(Some(("extends", v))).run(t)
    case JavaSToken(SUPER, _) :: JavaSToken(TOKEN, v) :: t => TokenListState.unit(Some("super", v)).run(t)
    case t => TokenListState.unit(None).run(t)
  }

  def parseTypeDesignator: TokenListState[JavaTypeDesignate] = for {
    typename <- takeDotSeparated.map(_.mkString("."))
    extend <- parseTypeRelation
    generics <- parseGenerics
  } yield JavaTypeDesignate(typename, extend, generics)

  def parseType: TokenListState[JavaTypeUse] = for {
    typeDesignator <- parseTypeDesignator
    arrayNotations <- parseArrayType("")
  } yield JavaTypeUse(typeDesignator, arrayNotations)

  def takeToken[A](x: A): TokenListState[A] = TokenListState(tokens => (x, tokens.tail)) // 토큰 한개를 흘려보냄

  def parseArgs: TokenListState[List[JavaArgument]] = TokenListState(tokens => {
    def parseArgName(typename: JavaTypeUse, arg: JavaArgument): TokenListState[JavaArgument] =
      TokenListState(tokens => (arg.copy(argumentType = typename, name = tokens.head.value), tokens.tail))

    def parseArg(javaArg: JavaArgument): TokenListState[JavaArgument] = TokenListState {
      case JavaSToken(ANNOTATION, _) :: t => (for {
        annotation <- parseAnnotation
        arg <- parseArg(javaArg.appendAnnotation(annotation))
      } yield arg).run(t)
      case JavaSToken(FINAL, _) :: t =>
        parseArg(javaArg.setFinal).run(t)
      case t => (for {
        typename <- parseType
        argName <- parseArgName(typename, javaArg)
      } yield argName).run(t)
    }

    def loop(acc: List[JavaArgument]): TokenListState[List[JavaArgument]] = TokenListState {
      case JavaSToken(RIGHT_PARENTHESIS, _) :: t =>
        TokenListState.unit(acc).run(t)
      case JavaSToken(COMMA, _) :: t =>
        loop(acc).run(t)
      case t => (for {
        arg <- parseArg(JavaArgument.empty)
        args <- loop(acc :+ arg)
      } yield args).run(t)
    }

    loop(Nil).run(tokens)
  })

  def takeTokenOrElse(token: JavaTokenEnum, default: String): TokenListState[String] = TokenListState {
    case JavaSToken(tk, v) :: t if token == tk =>
      TokenListState.unit(v).run(t)
    case t =>
      TokenListState.unit(default).run(t)
  }

  def parseMembers(modifier: JavaModifier): TokenListState[JavaMembers] = {
    def dropLater: TokenListState[Unit] = TokenListState {
      case Nil => throw new TokenNotAcceptedException("nil list!")
      case JavaSToken(SEMICOLON, _) :: t =>
        TokenListState.unit(()).run(t)
      case JavaSToken(LBRACE, _) :: t =>
        (for {
          _ <- parseParenthesisSkipHead(LBRACE, RBRACE)
        } yield ()).run(t)
      case JavaSToken(DEFAULT, _) :: t =>
        (for {
          _ <- parseUntil(SEMICOLON)
        } yield ()).run(t)
      case JavaSToken(THROWS, _) :: t =>
        (for {
          _ <- separateByType
          _ <- dropLater
        } yield ()).run(t)
      case h :: _ => throw new TokenNotAcceptedException(h.toString)
    }

    def parseMemberAfterName(modifier: JavaModifier, typename: JavaTypeUse, name: String): TokenListState[JavaMembers] = TokenListState {
      case Nil => throw new TokenNotAcceptedException("nil list!")
      case JavaSToken(LEFT_PARENTHESIS, _) :: t => (for {
        args <- parseArgs
        _ <- dropLater
      } yield JavaMethod(modifier, name, typename, args)).run(t)
      case JavaSToken(SUBSTITUTE, _) :: t =>
        (for {
          _ <- parseUntil(SEMICOLON)
        } yield JavaMember(modifier, name, typename)).run(t)
      case JavaSToken(SEMICOLON, _) :: t =>
        TokenListState.unit(JavaMember(modifier, name, typename)).run(t)
      case h :: _ => throw new TokenNotAcceptedException(h.toString)
    }

    for {
      typename <- parseType
      name <- takeTokenOrElse(TOKEN, typename.show)
      after <- parseMemberAfterName(modifier, typename, name)
    } yield after
  }

  // LL(1)의 한계...ㅠㅠ, commaSeparatedType := type "," commaSeparatedType
  //                                       | type
  def separateByType: TokenListState[List[JavaTypeUse]] = TokenListState(tokens => {
    def loop(acc: List[JavaTypeUse]): TokenListState[List[JavaTypeUse]] = for {
      typename <- parseType
      types <- TokenListState {
        case JavaSToken(COMMA, _) :: t =>
          loop(acc :+ typename).run(t)
        case t =>
          TokenListState.unit(acc :+ typename).run(t)
      }
    } yield types

    loop(Nil).run(tokens)
  })

  def takeString: TokenListState[String] = TokenListState(tokens => (tokens.head.value, tokens.tail))

  def parseClassInside(path: String, acc: List[JavaDefinition]): TokenListState[List[JavaDefinition]] = TokenListState {
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: t =>
      TokenListState.unit(acc).run(t)
    case t => (for {
      definition <- parseDefs(JavaModifier.empty(path))
      classInside <- parseClassInside(path, acc :+ definition)
    } yield classInside).run(t)
  }

  def parseClass(modifier: JavaModifier): TokenListState[JavaClass] = for {
    name <- takeString
    generics <- parseParenthesisList(LT, GT)
    extendInh <- parseTypesFrom(EXTENDS)
    implementH <- parseTypesFrom(IMPLEMENTS)
    _ <- assertToken(LBRACE)
    defs <- parseClassInside(s"${modifier.fullPath}.$name", Nil)
  } yield JavaClass(name, modifier.copy(generic = generics), defs, extendInh, implementH)

  def parseTypesFrom(targetToken: JavaTokenEnum): TokenListState[List[JavaTypeUse]] = TokenListState {
    case JavaSToken(token, _) :: t if token == targetToken =>
      separateByType.run(t)
    case tails => (Nil, tails)
  }

  def parseInterfaceDefs(path: String, acc: List[JavaDefinition]): TokenListState[List[JavaDefinition]] = TokenListState {
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: t => (acc, t)
    case t => (for {
      defs <- parseDefs(JavaModifier.empty(path))
      interfaceDef <- parseInterfaceDefs(path, acc :+ defs)
    } yield interfaceDef).run(t)
  }

  def parseAnnotationInterface(modifier: JavaModifier): TokenListState[JavaAnnotationInterface] = for {
    name <- takeString
    extendInh <- parseTypesFrom(EXTENDS)
    _ <- assertToken(LBRACE)
    defs <- parseInterfaceDefs(s"${modifier.fullPath}.$name", Nil)
  } yield JavaAnnotationInterface(name, modifier, defs, extendInh)

  def parseInterface(modifier: JavaModifier): TokenListState[JavaInterface] = for {
    name <- takeString
    generic <- parseParenthesisList(LT, GT)
    extendInh <- parseTypesFrom(EXTENDS)
    _ <- assertToken(LBRACE)
    defs <- parseInterfaceDefs(s"${modifier.fullPath}.$name", Nil)
  } yield JavaInterface(name, modifier.copy(generic = generic), defs, extendInh)

  def assertToken(token: JavaTokenEnum): TokenListState[Unit] = TokenListState {
    case JavaSToken(tk, _) :: t if tk == token =>
      TokenListState.unit(()).run(t)
    case t => throw new TokenNotAcceptedException(s"${t.head.tokenType} => ${t.head.value}, ${t.tail.take(5)}")
  }

  def parseParenthesis(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): TokenListState[String] = TokenListState(tokens => {
    def loop(cnt: Int, acc: String): TokenListState[String] = TokenListState {
      case Nil =>
        TokenListState.unit(acc).run(Nil)
      case JavaSToken(lp, _) :: t if lp == leftPar =>
        loop(cnt + 1, acc + lp.value).run(t)
      case JavaSToken(rp, _) :: t if cnt == 0 && rp == rightPar =>
        TokenListState.unit(acc + rp.value).run(t)
      case JavaSToken(rp, _) :: t if rp == rightPar =>
        loop(cnt - 1, acc + rp.value).run(t)
      case JavaSToken(_, v) :: t =>
        loop(cnt, acc + v).run(t)
    }

    tokens match {
      case JavaSToken(lp, _) :: t if lp == leftPar =>
        loop(0, lp.value).run(t)
      case _ =>
        TokenListState.unit("").run(tokens)
    }
  })


  def parseParenthesisList(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): TokenListState[List[String]] = TokenListState(tokens => {
    def loop(cnt: Int, works: String, acc: List[String]): TokenListState[List[String]] = TokenListState {
      case Nil =>
        TokenListState.unit(if (works != "") acc :+ works else acc).run(Nil)
      case JavaSToken(lp, _) :: t if lp == leftPar =>
        loop(cnt + 1, works + lp.value, acc).run(t)
      case JavaSToken(rp, _) :: t if cnt == 0 && rp == rightPar =>
        TokenListState.unit(acc).run(t)
      case JavaSToken(rp, _) :: t if rp == rightPar =>
        loop(cnt - 1, "", acc :+ works + rp.value).run(t)
      case JavaSToken(_, v) :: t =>
        loop(cnt, "", acc :+ v).run(t)
    }

    tokens match {
      case JavaSToken(lp, _) :: t if lp == leftPar =>
        loop(0, "", Nil).run(t)
      case _ =>
        TokenListState.unit(Nil).run(tokens)
    }
  })


  def parseParenthesisSkipHead(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): TokenListState[List[String]] = TokenListState(tokens => {
    def loop(cnt: Int, acc: List[String]): TokenListState[List[String]] = TokenListState {
      case Nil =>
        TokenListState.unit(acc).run(Nil)
      case JavaSToken(lp, _) :: t if lp == leftPar =>
        loop(cnt + 1, acc :+ lp.value).run(t)
      case JavaSToken(rp, _) :: t if cnt == 0 && rp == rightPar =>
        TokenListState.unit(acc).run(t)
      case JavaSToken(rp, _) :: t if rp == rightPar =>
        loop(cnt - 1, acc :+ rp.value).run(t)
      case JavaSToken(_, v) :: t =>
        loop(cnt, acc :+ v).run(t)
    }

    loop(0, Nil).run(tokens)
  })

  def parseUntil(until: JavaTokenEnum): TokenListState[String] = TokenListState(tokens => {
    def loop(acc: String): TokenListState[String] = TokenListState {
      case Nil =>
        TokenListState.unit(acc).run(Nil)
      case JavaSToken(target, _) :: t if target == until =>
        TokenListState.unit(acc).run(t)
      case h :: t =>
        loop(acc + h.value).run(t)
    }

    loop("").run(tokens)
  })

  def parseCode(bowl: JavaCode): TokenListState[JavaCode] = TokenListState {
    case Nil => (bowl, Nil)
    case JavaSToken(ANNOTATION, _) :: t => (for {
      annotation <- parseAnnotation
      code <- parseCode(bowl.appendAnnotationBuf(annotation))
    } yield code).run(t)
    case JavaSToken(PACKAGE, _) :: t => (for {
      pkgName <- parseUntil(SEMICOLON)
      code <- parseCode(bowl.setPackageName(pkgName).copyWithoutAnnotations)
    } yield code).run(t)
    case JavaSToken(IMPORT, _) :: t => (for {
      importName <- parseUntil(SEMICOLON)
      code <- parseCode(bowl.appendImport(importName).copyWithoutAnnotations)
    } yield code).run(t)
    case tokens => (for {
      definition <- parseDefs(JavaModifier.empty(bowl.packageName, bowl.annotationsBuf))
      code <- parseCode(bowl.appendDefinition(definition).copyWithoutAnnotations)
    } yield code).run(tokens)
  }

  def apply(tokens: List[JavaSToken]): JavaCode =
    parseCode(emptyCode).run(tokens.filterNot(x => List(COMMENT, COMMENT_BLOCK).contains(x.tokenType)))._1
}
