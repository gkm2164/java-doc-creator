package com.kakao.search.middle.javadoc

import com.kakao.search.middle.exceptions.TokenNotAcceptedException
import com.kakao.search.middle.functional.TokenListState
import com.kakao.search.middle.javalang.JavaTokenEnum
import com.kakao.search.middle.javalang.JavaTokenEnum._

import scala.annotation.tailrec

case class JavaCode(packageName: String,
                    imports: List[String],
                    defs: List[JavaDefinition]) {
  def setPackageName(str: String): JavaCode = copy(packageName = str)

  def appendImport(imp: String): JavaCode = this.copy(imports = imports :+ imp)

  def appendDefinition(javaDefinition: JavaDefinition): JavaCode = this.copy(defs = defs :+ javaDefinition)

  def show: String =
    StringBuilder.newBuilder
      .append(s"<h3>packagename: $packageName</h3>")
      .append(imports.map(x => s"<li>import $x</li>").mkString("<ul>", "", "</ul>"))
      .append(s"<p>total ${defs.length} number of definitions are contained</p>")
      .append(defs.map(_.show).mkString("\n"))
      .toString()
}

object JavaCode {
  import com.kakao.search.middle.functional.MonadSyntax._
  import com.kakao.search.middle.functional.TokenListStateBehavior._

  def emptyCode: JavaCode = JavaCode("", Nil, Nil)

  def parseAnnotationName(base: String): TokenListState[String] = TokenListState {
    case Nil => (base, Nil)
    case JavaSToken(DOT, _) :: JavaSToken(_, annName) :: t => parseAnnotationName(base + s".$annName").run(t)
    case JavaSToken(_, n) :: t => (base + n, t)
  }

  def parseAnnotation: TokenListState[String] = for {
    name <- parseAnnotationName("")
    _ <- parseParenthesis(LPAR, RPAR)
  } yield name

  def parseEnum(modifier: JavaModifier): TokenListState[JavaEnumClass] = {
    def parseEnumInside(name: String): TokenListState[JavaEnumClass] = TokenListState(state => {
      def loop(acc: List[String]): TokenListState[JavaEnumClass] = TokenListState {
        case JavaSToken(TOKEN, tokenName) :: tail => (for {
          _ <- parseParenthesis(LPAR, RPAR)
          enumClass <- TokenListState {
            case Nil => throw new TokenNotAcceptedException("nil list")
            case JavaSToken(COMMA, _) :: t => loop(acc :+ tokenName).run(t)
            case JavaSToken(SEMICOLON, _) :: t => parseClassInside(Nil).map(defs => JavaEnumClass(name, modifier, acc, defs)).run(t)
            case JavaSToken(RBRACE, _) :: t => (JavaEnumClass(name, modifier, acc, Nil), t)
            case h :: _ => throw new TokenNotAcceptedException(s"not allowed token: $h")
          }
        } yield enumClass).run(tail)
        case t => throw new TokenNotAcceptedException(s"unknown token: ${t.head}")
      }

      loop(Nil).run(state)
    })

    for {
      name <- takeString
      _ <- assertToken(LBRACE)
      cls <- parseEnumInside(name)
    } yield cls
  }

  def parseDefs(modifier: JavaModifier): TokenListState[JavaDefinition] = TokenListState {
    case Nil => throw new TokenNotAcceptedException("token is empty")
    case JavaSToken(ANNOTATION, _) :: t => parseAnnotation.flatMap(str => parseDefs(modifier.appendAnnotation(str))).run(t)
    case JavaSToken(access@(PRIVATE | PROTECTED | PUBLIC), _) :: t => parseDefs(modifier.setAccess(access)).run(t)
    case JavaSToken(LBRACE, _) :: t => parseParenthesisSkipHead(LBRACE, RBRACE).flatMap(_ => parseDefs(JavaModifier.empty)).run(t)
    case JavaSToken(STATIC, _) :: t => parseDefs(modifier.setStatic).run(t)
    case JavaSToken(LT, _) :: t => parseParenthesisSkipHead(LT, GT).flatMap(x => parseDefs(modifier.setGeneric(x))).run(t)
    case JavaSToken(FINAL, _) :: t => parseDefs(modifier.setFinal).run(t)
    case JavaSToken(ABSTRACT, _) :: t => parseDefs(modifier.setAbstract).run(t)
    case JavaSToken(COMMENT_MACRO, v) :: t => parseDefs(modifier.appendMacro(v)).run(t)
    case JavaSToken(CLASS, _) :: t => parseClass(modifier).run(t)
    case JavaSToken(ENUM, _) :: t => parseEnum(modifier).run(t)
    case JavaSToken(INTERFACE, _) :: t => parseInterface(modifier).run(t)
    case JavaSToken(ANNOTATION_INTERFACE, _) :: t => parseAnnotationInterface(modifier).run(t)
    case tokens@JavaSToken(TOKEN, _) :: _ => parseMembers(modifier).run(tokens)
    case h :: t => throw new TokenNotAcceptedException(s"${h.tokenType} => ${h.value}, ${t.take(10)}")
  }

  def takeSeparatorIterate(separator: JavaTokenEnum): TokenListState[List[String]] = TokenListState(tokens => {
    def loop(acc: List[String]): TokenListState[List[String]] = TokenListState {
      case Nil => (acc, Nil)
      case h :: JavaSToken(sp, _) :: t if sp == separator => loop(acc :+ h.value).run(t)
      case h :: t => (acc :+ h.value, t)
    }

    loop(Nil).run(tokens)
  })

  def parseGenericType: TokenListState[String] = TokenListState {
    case JavaSToken(LT, _) :: t => parseParenthesisSkipHead(LT, GT).run(t)
    case tokens => ("", tokens)
  }

  def parseArrayType(acc: String): TokenListState[String] = TokenListState {
    case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: t => parseArrayType(acc + "[]").run(t)
    case JavaSToken(ETC_ARRAY, _) :: t => parseArrayType(acc + "...").run(t)
    case tokens => (acc, tokens)
  }

  def parseType: TokenListState[String] = for {
    typename <- takeSeparatorIterate(DOT).map(x => x.mkString("."))
    genericName <- parseGenericType
    arrayNotations <- parseArrayType("")
  } yield typename + genericName + arrayNotations

  def takeToken[A](x: A): TokenListState[A] = TokenListState(tokens => (x, tokens.tail)) // 토큰 한개를 흘려보냄

  def parseArgs: TokenListState[List[JavaArgument]] = TokenListState(tokens => {
    def parseArgName(typename: String, arg: JavaArgument): TokenListState[JavaArgument] =
      TokenListState(tokens => (arg.copy(argumentType = typename, name = tokens.head.value), tokens.tail))

    def parseArg(javaArg: JavaArgument): TokenListState[JavaArgument] = TokenListState {
      case JavaSToken(ANNOTATION, _) :: t => parseAnnotation.flatMap(x => parseArg(javaArg.appendAnnotation(x))).run(t)
      case JavaSToken(FINAL, _) :: t => parseArg(javaArg.setFinal).run(t)
      case t => parseType.flatMap(t => parseArgName(t, javaArg)).run(t)
    }

    def loop(acc: List[JavaArgument]): TokenListState[List[JavaArgument]] = TokenListState {
      case JavaSToken(RPAR, _) :: t => (acc, t)
      case JavaSToken(COMMA, _) :: t => loop(acc).run(t)
      case t => parseArg(JavaArgument.empty).flatMap(arg => loop(acc :+ arg)).run(t)
    }

    loop(Nil).run(tokens)
  })

  def takeTokenOrElse(token: JavaTokenEnum, default: String): TokenListState[String] = TokenListState {
    case JavaSToken(tk, v) :: t if token == tk => TokenListState.unit(v).run(t)
    case t => TokenListState.unit(default).run(t)
  }

  def UnitObject: Unit = Unit

  def parseMembers(modifier: JavaModifier): TokenListState[JavaMembers] = {
    def dropLater: TokenListState[Unit] = TokenListState {
      case Nil => throw new TokenNotAcceptedException("nil list!")
      case JavaSToken(SEMICOLON, _) :: t => TokenListState.unit(UnitObject).run(t)
      case JavaSToken(LBRACE, _) :: t => parseParenthesisSkipHead(LBRACE, RBRACE).map(_ => UnitObject).run(t)
      case JavaSToken(DEFAULT, _) :: t => parseUntil(SEMICOLON).map(_ => UnitObject).run(t)
      case JavaSToken(THROWS, _) :: t => separateByType.flatMap(_ => dropLater).map(_ => UnitObject).run(t)
      case h :: _ => throw new TokenNotAcceptedException(h.toString)
    }

    def parseMemberAfterName(modifier: JavaModifier, typename: String, name: String): TokenListState[JavaMembers] = TokenListState {
      case Nil => throw new TokenNotAcceptedException("nil list!")
      case JavaSToken(LPAR, _) :: t => parseArgs.flatMap(args => dropLater.map(_ => JavaMethod(modifier, name, typename, args))).run(t)
      case JavaSToken(SUBSTITUTE, _) :: t => parseUntil(SEMICOLON).map(_ => JavaMember(modifier, name, typename)).run(t)
      case JavaSToken(SEMICOLON, _) :: t => TokenListState.unit(JavaMember(modifier, name, typename)).run(t)
      case h :: _ => throw new TokenNotAcceptedException(h.toString)
    }

    for {
      typename <- parseType
      name <- takeTokenOrElse(TOKEN, typename)
      after <- parseMemberAfterName(modifier, typename, name)
    } yield after
  }

  // LL(1)의 한계...ㅠㅠ, commaSeparatedType := type "," commaSeparatedType
  //                                       | type
  def separateByType: TokenListState[List[String]] = TokenListState(tokens => {
    def loop(acc: List[String]): TokenListState[List[String]] = for {
      typename <- parseType
      types <- TokenListState {
        case JavaSToken(COMMA, _) :: t => loop(acc :+ typename).run(t)
        case t => (acc :+ typename, t)
      }
    } yield types

    loop(Nil).run(tokens)
  })

  def takeString: TokenListState[String] = TokenListState(tokens => (tokens.head.value, tokens.tail))

  def parseClassInside(acc: List[JavaDefinition]): TokenListState[List[JavaDefinition]] = TokenListState {
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: t => TokenListState.unit(acc).run(t)
    case t => parseDefs(JavaModifier.empty).flatMap(d => parseClassInside(acc :+ d)).run(t)
  }

  def parseClass(modifier: JavaModifier): TokenListState[JavaClass] = for {
    name <- takeString
    extendInh <- parseTokensFrom(EXTENDS)
    implementH <- parseTokensFrom(IMPLEMENTS)
    _ <- assertToken(LBRACE)
    defs <- parseClassInside(Nil)
  } yield JavaClass(name, modifier, defs, extendInh, implementH)

  def parseTokensFrom(targetToken: JavaTokenEnum): TokenListState[List[String]] = TokenListState {
    case JavaSToken(token, _) :: t if token == targetToken => separateByType.run(t)
    case tails => (Nil, tails)
  }

  def parseInterfaceDefs(acc: List[JavaDefinition]): TokenListState[List[JavaDefinition]] = TokenListState {
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: t => (acc, t)
    case t => parseDefs(JavaModifier.empty).flatMap(x => parseInterfaceDefs(acc :+ x)).run(t)
  }

  def parseAnnotationInterface(modifier: JavaModifier): TokenListState[JavaAnnotationInterface] = for {
    name <- takeString
    extendInh <- parseTokensFrom(EXTENDS)
    _ <- assertToken(LBRACE)
    defs <- parseInterfaceDefs(Nil)
  } yield JavaAnnotationInterface(name, modifier, defs, extendInh)

  def parseInterface(modifier: JavaModifier): TokenListState[JavaInterface] = for {
    name <- takeString
    extendInh <- parseTokensFrom(EXTENDS)
    _ <- assertToken(LBRACE)
    defs <- parseInterfaceDefs(Nil)
  } yield JavaInterface(name, modifier, defs, extendInh)

  def assertToken(token: JavaTokenEnum): TokenListState[Unit] = TokenListState {
    case JavaSToken(tk, _) :: t if tk == token => (Unit, t)
    case t => throw new TokenNotAcceptedException(s"${t.head.tokenType} => ${t.head.value}")
  }

  def parseParenthesis(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): TokenListState[String] = TokenListState(tokens => {
    def loop(cnt: Int, acc: String): TokenListState[String] = TokenListState {
      case Nil => (acc, Nil)
      case JavaSToken(lp, _) :: t if lp == leftPar => loop(cnt + 1, acc + lp.value).run(t)
      case JavaSToken(rp, _) :: t if cnt == 0 && rp == rightPar => TokenListState.unit(acc + rp.value).run(t)
      case JavaSToken(rp, _) :: t if rp == rightPar => loop(cnt - 1, acc + rp.value).run(t)
      case JavaSToken(_, v) :: t => loop(cnt, acc + v).run(t)
    }

    tokens match {
      case JavaSToken(lp, _) :: t if lp == leftPar => loop(0, lp.value).run(t)
      case _ => TokenListState.unit("").run(tokens)
    }
  })

  def parseParenthesisSkipHead(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): TokenListState[String] = TokenListState(tokens => {
    def loop(cnt: Int, acc: String): TokenListState[String] = TokenListState {
      case Nil => (acc, Nil)
      case JavaSToken(lp, _) :: t if lp == leftPar => loop(cnt + 1, acc + lp.value).run(t)
      case JavaSToken(rp, _) :: t if cnt == 0 && rp == rightPar => (acc + rp.value, t)
      case JavaSToken(rp, _) :: t if rp == rightPar => loop(cnt - 1, acc + rp.value).run(t)
      case JavaSToken(_, v) :: t => loop(cnt, acc + v).run(t)
    }

    loop(0, leftPar.value).run(tokens)
  })

  def parseUntil(until: JavaTokenEnum): TokenListState[String] = TokenListState(tokens => {
    @tailrec
    def loop(tokens: List[JavaSToken], acc: String): (String, List[JavaSToken]) = tokens match {
      case Nil => (acc, Nil)
      case JavaSToken(target, _) :: t if target == until => (acc, t)
      case h :: t => loop(t, acc + h.value)
    }

    loop(tokens, "")
  })

  def parseCode(bowl: JavaCode): TokenListState[JavaCode] = TokenListState {
    case Nil => (bowl, Nil)
    case JavaSToken(ANNOTATION, _) :: t => parseAnnotation.flatMap(_ => parseCode(bowl)).run(t) // ignore annotation
    case JavaSToken(PACKAGE, _) :: t => parseUntil(SEMICOLON).flatMap(pkgName => parseCode(bowl.setPackageName(pkgName))).run(t)
    case JavaSToken(IMPORT, _) :: t => parseUntil(SEMICOLON).flatMap(imp => parseCode(bowl.appendImport(imp))).run(t)
    case tokens => parseDefs(JavaModifier.empty).flatMap(x => parseCode(bowl.appendDefinition(x))).run(tokens)
  }

  def apply(tokens: List[JavaSToken]): JavaCode = parseCode(emptyCode).run(tokens.filterNot(x => List(COMMENT, COMMENT_BLOCK).contains(x.tokenType)))._1
}
