package com.kakao.search.middle.javadoc

import com.kakao.search.middle.javalang.JavaTokenEnum
import JavaTokenEnum._
import com.kakao.search.middle.exceptions.TokenNotAcceptedException

import scala.annotation.tailrec

case class JavaCode(packageName: String,
                    imports: List[String],
                    defs: List[JavaDefinition]) {
  def setPackageName(str: String): JavaCode = copy(packageName = str)

  def appendImport(imp: String): JavaCode = this.copy(imports = imports :+ imp)

  def appendDefinition(javaDefininiton: JavaDefinition): JavaCode = this.copy(defs = defs :+ javaDefininiton)

  def show: String = {
    val sb = StringBuilder.newBuilder
    sb.append(s"<h3>packagename: $packageName</h3>")
    sb.append(imports.map(x => s"<li>import $x</li>").mkString("<ul>", "", "</ul>"))
    sb.append(s"<p>total ${defs.length} number of definitions are contained</p>")
    sb.append(defs.map(_.show).mkString("\n"))
    sb.toString()
  }
}

object JavaCode {
  case class TokenListState[+A](f: List[JavaSToken] => (A, List[JavaSToken])) {
    def run(tokens: List[JavaSToken]): (A, List[JavaSToken]) = {
      f(tokens)
    }
  }

  implicit class TokenListStateMonadExtend[+A](tls: TokenListState[A]) {
    def map[B](f: A => B): TokenListState[B] = TokenListState(tokens => {
      val (x, next) = tls.run(tokens)
      (f(x), next)
    })

    def flatMap[B](f: A => TokenListState[B]): TokenListState[B] = TokenListState(tokens => {
      val (v, next) = tls.run(tokens)
      f(v).run(next)
    })
  }

  def emptyCode: JavaCode = JavaCode("", Nil, Nil)

  def parseAnnotationName(base: String): TokenListState[String] = TokenListState({
    case Nil => (base, Nil)
    case JavaSToken(DOT, _) :: JavaSToken(_, annName) :: t => parseAnnotationName(base + s".$annName").run(t)
    case JavaSToken(_, n) :: t => (base + n, t)
  })

  def parseAnnotation: TokenListState[String] = TokenListState(tokens => {
    println(s"annotation: ${tokens.head}")
    (for {
      name <- parseAnnotationName("")
      _ <- parseParenthesis(LPAR, RPAR)
    } yield name).run(tokens)
  })

  def parseEnum(modifier: JavaModifier): TokenListState[JavaEnumClass] = {
    def parseEnumInside(name: String): TokenListState[JavaEnumClass] = TokenListState(tokens => {
      def loop(acc: List[String]): TokenListState[JavaEnumClass] = TokenListState({
        case JavaSToken(TOKEN, tokenName) :: t =>
          val (_, next) = parseParenthesis(LPAR, RPAR).run(t)
          next match {
            case JavaSToken(COMMA, _) :: t => loop(acc :+ tokenName).run(t)
            case JavaSToken(RBRACE, _) :: t => (JavaEnumClass(name, modifier, acc, Nil), t)
            case JavaSToken(SEMICOLON, _) :: t => parseClassInside(Nil).map(defs => JavaEnumClass(name, modifier, acc, defs)).run(t)
          }
        case t => throw new TokenNotAcceptedException(s"unknown token: ${t.head}")
      })

      loop(Nil).run(tokens)
    })

    for {
      name <- takeString
      _ <- assertToken(LBRACE)
      cls <- parseEnumInside(name)
    } yield cls
  }

  def parseDefs(modifier: JavaModifier): TokenListState[JavaDefinition] = TokenListState(tokens => {
    println(s"defs: ${tokens.head}")
    tokens match {
      case Nil => throw new TokenNotAcceptedException("token is empty")
      case JavaSToken(ANNOTATION, _) :: t => parseAnnotation.flatMap(str => parseDefs(modifier.appendAnnotation(str))).run(t)
      case JavaSToken(access@(PRIVATE | PROTECTED | PUBLIC), _) :: t => parseDefs(modifier.setAccess(access)).run(t)
      case tokens@JavaSToken(LBRACE, _) :: _ => parseParenthesis(LBRACE, RBRACE).flatMap(_ => parseDefs(JavaModifier.empty)).run(tokens)
      case JavaSToken(STATIC, _) :: t => parseDefs(modifier.setStatic).run(t)
      case JavaSToken(LT, _) :: t => parseUntil(GT).flatMap(x => parseDefs(modifier.setGeneric(x))).run(t)
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
  })

  def takeSeperatorIterate(separator: JavaTokenEnum): TokenListState[List[String]] = TokenListState(tokens => {
    @tailrec
    def loop(acc: List[String])(tokens: List[JavaSToken]): (List[String], List[JavaSToken]) = tokens match {
      case Nil => (acc, Nil)
      case h :: JavaSToken(sp, _) :: t if sp == separator => loop(acc :+ h.value)(t)
      case h :: t => (acc :+ h.value, t)
    }

    loop(Nil)(tokens)
  })

  def parseGenericType: TokenListState[String] = TokenListState({
    case tokens@JavaSToken(LT, _) :: _ => parseParenthesis(LT, GT).run(tokens)
    case tokens => ("", tokens)
  })

  def parseArrayType(acc: String): TokenListState[String] = TokenListState({
    case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: t => parseArrayType(acc + "[]").run(t)
    case tokens => ("", tokens)
  })

  def parseType: TokenListState[String] = for {
    typename <- takeSeperatorIterate(DOT).map(x => x.mkString("."))
    genericName <- parseGenericType
    arrayNotations <- parseArrayType("")
  } yield typename + genericName + arrayNotations

  def takeToken[A](x: A): TokenListState[A] = TokenListState(tokens => (x, tokens.tail)) // 토큰 한개를 흘려보냄

  def parseArgs: TokenListState[List[JavaArgument]] = TokenListState(tokens => {
    def parseArgName(typename: String, arg: JavaArgument): TokenListState[JavaArgument] =
      TokenListState(tokens => (arg.copy(argumentType = typename, name = tokens.head.value), tokens.tail))

    def parseArg(javaArg: JavaArgument): TokenListState[JavaArgument] = TokenListState({
      case JavaSToken(ANNOTATION, _) :: t => parseAnnotation.flatMap(x => parseArg(javaArg.appendAnnotation(x))).run(t)
      case JavaSToken(FINAL, _) :: t => parseArg(javaArg.setFinal).run(t)
      case t => parseType.flatMap(t => parseArgName(t, javaArg)).run(t)
    })

    def loop(acc: List[JavaArgument]): TokenListState[List[JavaArgument]] = TokenListState({
      case JavaSToken(RPAR, _) :: t => (acc, t)
      case JavaSToken(COMMA, _) :: t => loop(acc).run(t)
      case t => parseArg(JavaArgument.empty).flatMap(arg => loop(acc :+ arg)).run(t)
    })

    loop(Nil).run(tokens)
  })

  def takeTokenOrElse(token: JavaTokenEnum, default: String): TokenListState[String] = TokenListState({
    case JavaSToken(tk, v) :: t if token == tk => (v, t)
    case t => (default, t)
  })

  def parseMembers(modifier: JavaModifier): TokenListState[JavaMembers] = TokenListState(tokens => {
    def dropLater: TokenListState[Unit] = TokenListState({
      case Nil => throw new TokenNotAcceptedException("nil list!")
      case JavaSToken(SEMICOLON, _) :: tail => (Unit, tail)
      case tokens@JavaSToken(LBRACE, _) :: _ => (Unit, parseParenthesis(LBRACE, RBRACE).run(tokens)._2)
      case JavaSToken(DEFAULT, _) :: tail => (Unit, parseUntil(SEMICOLON).run(tail)._2)
      case tokens@JavaSToken(THROWS, _) :: _ => (Unit, parseFromToken(THROWS).flatMap(_ => dropLater).run(tokens)._2)
      case h :: _ => throw new TokenNotAcceptedException(h.toString)
    })

    def parseMemberAfterName(modifier: JavaModifier, typename: String, name: String): TokenListState[JavaMembers] = TokenListState({
      case Nil => throw new TokenNotAcceptedException("nil list!")
      case JavaSToken(LPAR, _) :: t => parseArgs.flatMap(args => dropLater.map(_ => JavaMethod(modifier, name, typename, args))).run(t)
      case tokens@JavaSToken(SUBSTITUTE, _) :: _ => parseUntil(SEMICOLON).map(_ => JavaMember(modifier, name, typename)).run(tokens)
      case JavaSToken(SEMICOLON, _) :: t => (JavaMember(modifier, name, typename), t)
      case h :: _ => throw new TokenNotAcceptedException(h.toString)
    })

    tokens match {
      case JavaSToken(LT, _) :: t => parseParenthesis(LT, GT).flatMap(_ => parseMembers(modifier)).run(t)
      case tks => parseType.flatMap(typename => takeTokenOrElse(TOKEN, "").flatMap(name => parseMemberAfterName(modifier, typename, name))).run(tks)
    }
  })

  def separateByType: TokenListState[List[String]] = TokenListState(tokens => {
    @tailrec
    def loop(tokens: List[JavaSToken], acc: List[String]): (List[String], List[JavaSToken]) = {
      if (tokens.head.tokenType != TOKEN) (acc, tokens)
      else {
        val (t, next) = parseType.run(tokens)
        if (next.head.tokenType == COMMA) loop(next.tail, acc :+ t)
        else (acc :+ t, next)
      }
    }

    loop(tokens, Nil)
  })

  def takeString: TokenListState[String] = TokenListState(tokens => (tokens.head.value, tokens.tail))

  def parseClassInside(acc: List[JavaDefinition]): TokenListState[List[JavaDefinition]] = TokenListState({
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: t => (acc, t)
    case t => parseDefs(JavaModifier.empty).flatMap(d => parseClassInside(acc :+ d)).run(t)
  })

  def parseClass(modifier: JavaModifier): TokenListState[JavaClass] = for {
    name <- takeString
    extendInh <- parseFromToken(EXTENDS)
    implementH <- parseFromToken(IMPLEMENTS)
    _ <- assertToken(LBRACE)
    defs <- parseClassInside(Nil)
  } yield JavaClass(name, modifier, defs, extendInh, implementH)

  def parseFromToken(token: JavaTokenEnum): TokenListState[List[String]] = TokenListState({
    case JavaSToken(tk, _) :: t if tk == token => separateByType.run(t)
    case tails => (Nil, tails)
  })

  def parseInterfaceDefs(acc: List[JavaDefinition]): TokenListState[List[JavaDefinition]] = TokenListState({
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: t => (acc, t)
    case t => parseDefs(JavaModifier.empty).flatMap(x => parseInterfaceDefs(acc :+ x)).run(t)
  })

  def parseAnnotationInterface(modifier: JavaModifier): TokenListState[JavaAnnotationInterface] = for {
    name <- takeString
    extendInh <- parseFromToken(EXTENDS)
    _ <- assertToken(LBRACE)
    defs <- parseInterfaceDefs(Nil)
  } yield JavaAnnotationInterface(name, modifier, defs, extendInh)

  def parseInterface(modifier: JavaModifier): TokenListState[JavaInterface] = for {
    name <- takeString
    extendInh <- parseFromToken(EXTENDS)
    _ <- assertToken(LBRACE)
    defs <- parseInterfaceDefs(Nil)
  } yield JavaInterface(name, modifier, defs, extendInh)


  def assertToken(token: JavaTokenEnum): TokenListState[Unit] = TokenListState({
    case JavaSToken(tk, _) :: t if tk == token => (Unit, t)
    case t => throw new TokenNotAcceptedException(s"${t.head.tokenType} => ${t.head.value}")
  })

  def parseParenthesis(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): TokenListState[String] = TokenListState(tokens => {
    def loop(cnt: Int, acc: String): TokenListState[String] = TokenListState({
      case Nil => (acc, Nil)
      case JavaSToken(lp, _) :: t if lp == leftPar => loop(cnt + 1, acc + lp.value).run(t)
      case JavaSToken(rp, _) :: t if cnt == 0 && rp == rightPar => (acc + rp.value, t)
      case JavaSToken(rp, _) :: t if rp == rightPar => loop(cnt - 1, acc + rp.value).run(t)
      case JavaSToken(_, v) :: t => loop(cnt, acc + v).run(t)
    })

    tokens match {
      case JavaSToken(lp, _) :: t if lp == leftPar => loop(0, lp.value).run(t)
      case tails => ("", tails)
    }
  })

  def parseUntil(until: JavaTokenEnum): TokenListState[String] = TokenListState(tokens => {
    @tailrec
    def loop(tokens: List[JavaSToken], acc: String): (String, List[JavaSToken]) = tokens match {
      case Nil => (acc, Nil)
      case h :: t if h.tokenType == until => (acc, t)
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
