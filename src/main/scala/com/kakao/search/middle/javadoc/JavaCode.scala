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
  type TokenListState[+A] = List[JavaSToken] => (A, List[JavaSToken])

  implicit class TokenListStateMonadExtend[+A](tls: TokenListState[A]) {
    def map[B](f: A => B): TokenListState[B] = tokens => {
      val (x, next) = tls(tokens)
      (f(x), next)
    }

    def flatMap[B](f: A => TokenListState[B]): TokenListState[B] = tokens => {
      val (v, next) = tls(tokens)
      f(v)(next)
    }
  }

  def emptyCode: JavaCode = JavaCode("", Nil, Nil)

  def parseAnnotationName(base: String): TokenListState[String] = {
    case Nil => (base, Nil)
    case JavaSToken(DOT, _) :: JavaSToken(_, annName) :: t => parseAnnotationName(base + s".$annName")(t)
    case JavaSToken(_, n) :: t => (base + n, t)
  }

  def parseAnnotation: TokenListState[String] = {
    for {
      name <- parseAnnotationName("")
      _ <- parseParenthesis(LPAR, RPAR)
    } yield name
  }

  def parseDefs(modifier: JavaModifier): TokenListState[JavaDefinition] = {
    case Nil => throw new TokenNotAcceptedException("token is empty")
    case JavaSToken(ANNOTATION, _) :: t => parseAnnotation.flatMap(str => parseDefs(modifier.appendAnnotation(str)))(t)
    case JavaSToken(access@(PRIVATE | PROTECTED | PUBLIC), _) :: t => parseDefs(modifier.setAccess(access))(t)
    case JavaSToken(STATIC, _) :: t => parseDefs(modifier.setStatic)(t)
    case JavaSToken(FINAL, _) :: t => parseDefs(modifier.setFinal)(t)
    case JavaSToken(ABSTRACT, _) :: t => parseDefs(modifier.setAbstract)(t)
    case JavaSToken(COMMENT_MACRO, v) :: t => parseDefs(modifier.appendMacro(v))(t)
    case JavaSToken(CLASS, _) :: t => parseClass(modifier)(t)
    case JavaSToken(INTERFACE, _) :: t => parseInterface(modifier)(t)
    case JavaSToken(ANNOTATION_INTERFACE, _) :: t => parseAnnotationInterface(modifier)(t)
    case tokens@JavaSToken(TOKEN, _) :: _ => parseMembers(modifier)(tokens)
    case h :: _ => throw new TokenNotAcceptedException(s"${h.tokenType} => ${h.value}")
  }

  def takeSeperatorIterate(separator: JavaTokenEnum): TokenListState[List[String]] = tokens => {
    @tailrec
    def loop(acc: List[String])(tokens: List[JavaSToken]): (List[String], List[JavaSToken]) = tokens match {
      case Nil => (acc, Nil)
      case h :: JavaSToken(sp, _) :: t if sp == separator => loop(acc :+ h.value)(t)
      case h :: t => (acc :+ h.value, t)
    }

    loop(Nil)(tokens)
  }

  def parseGenericType: TokenListState[String] = {
    case tokens@JavaSToken(LT, _) :: _ => parseParenthesis(LT, GT)(tokens)
    case tokens => ("", tokens)
  }

  def parseArrayType(acc: String): TokenListState[String] = {
    case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: t => parseArrayType(acc + "[]")(t)
    case tokens => ("", tokens)
  }

  def parseType: TokenListState[String] = for {
    typename <- takeSeperatorIterate(DOT).map(x => x.mkString("."))
    genericName <- parseGenericType
    arrayNotations <- parseArrayType("")
  } yield typename + genericName + arrayNotations

  def takeToken[A](x: A): TokenListState[A] = tokens => (x, tokens.tail) // 토큰 한개를 흘려보냄

  def parseArgs: TokenListState[List[JavaArgument]] = tokens => {
    def parseArgName(typename: String, arg: JavaArgument): TokenListState[JavaArgument] = tokens => (arg.copy(argumentType = typename, name = tokens.head.value), tokens.tail)

    def parseArg(javaArg: JavaArgument): TokenListState[JavaArgument] = {
      case JavaSToken(ANNOTATION, _) :: t => parseAnnotation.flatMap(x => parseArg(javaArg.appendAnnotation(x)))(t)
      case JavaSToken(FINAL, _) :: t => parseArg(javaArg.setFinal)(t)
      case t => parseType.flatMap(t => parseArgName(t, javaArg))(t)
    }

    def loop(acc: List[JavaArgument]): TokenListState[List[JavaArgument]] = {
      case JavaSToken(RPAR, _) :: t => (acc, t)
      case JavaSToken(COMMA, _) :: t => loop(acc)(t)
      case t => parseArg(JavaArgument.empty).flatMap(arg => loop(acc :+ arg))(t)
    }

    loop(Nil)(tokens)
  }

  def takeTokenOrElse(token: JavaTokenEnum, default: String): TokenListState[String] = {
    case JavaSToken(tk, v) :: t if token == tk => (v, t)
    case t => (default, t)
  }

  def parseMembers(modifier: JavaModifier): TokenListState[JavaMembers] = tokens => {
    def dropLater: TokenListState[Unit] = {
      case Nil => throw new TokenNotAcceptedException("nil list!")
      case JavaSToken(SEMICOLON, _) :: tail => (Unit, tail)
      case tokens@JavaSToken(LBRACE, _) :: _ => (Unit, parseParenthesis(LBRACE, RBRACE)(tokens)._2)
      case JavaSToken(DEFAULT, _) :: tail => (Unit, parseUntil(SEMICOLON)(tail)._2)
      case h :: _ => throw new TokenNotAcceptedException(h.toString)
    }

    def parseMemberAfterName(modifier: JavaModifier, typename: String, name: String): TokenListState[JavaMembers] = {
      case Nil => throw new TokenNotAcceptedException("nil list!")
      case JavaSToken(LPAR, _) :: t => parseArgs.flatMap(args => dropLater.map(_ => JavaMethod(modifier, name, typename, args)))(t)
      case tokens@JavaSToken(SUBSTITUTE, _) :: _ => parseUntil(SEMICOLON).map(_ => JavaMember(modifier, name, typename))(tokens)
      case JavaSToken(SEMICOLON, _) :: t => (JavaMember(modifier, name, typename), t)
      case h :: _ => throw new TokenNotAcceptedException(h.toString)
    }

    tokens match {
      case JavaSToken(LT, _) :: t => parseParenthesis(LT, GT).flatMap(_ => parseMembers(modifier))(t)
      case tks => parseType.flatMap(typename => takeTokenOrElse(TOKEN, "").flatMap(name => parseMemberAfterName(modifier, typename, name)))(tks)
    }
  }

  def separateByType: TokenListState[List[String]] = tokens => {
    @tailrec
    def loop(tokens: List[JavaSToken], acc: List[String]): (List[String], List[JavaSToken]) = {
      if (tokens.head.tokenType != TOKEN) (acc, tokens)
      else {
        val (t, next) = parseType(tokens)
        if (next.head.tokenType == COMMA) loop(next.tail, acc :+ t)
        else (acc :+ t, next)
      }
    }

    loop(tokens, Nil)
  }

  def takeString: TokenListState[String] = tokens => (tokens.head.value, tokens.tail)

  def parseClassInside(acc: List[JavaDefinition]): TokenListState[List[JavaDefinition]] = {
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: t => (acc, t)
    case t => parseDefs(JavaModifier.empty).flatMap(d => parseClassInside(acc :+ d))(t)
  }

  def parseClass(modifier: JavaModifier): TokenListState[JavaClass] = for {
    name <- takeString
    extendInh <- parseFromToken(EXTENDS)
    implementH <- parseFromToken(IMPLEMENTS)
    _ <- assertToken(LBRACE)
    defs <- parseClassInside(Nil)
  } yield JavaClass(name, modifier, defs, extendInh, implementH)

  def parseFromToken(token: JavaTokenEnum): TokenListState[List[String]] = {
    case JavaSToken(tk, _) :: t if tk == token => separateByType(t)
    case tails => (Nil, tails)
  }

  def parseInterfaceDefs(acc: List[JavaDefinition]): TokenListState[List[JavaDefinition]] = {
    case Nil => throw new TokenNotAcceptedException("")
    case JavaSToken(RBRACE, _) :: t => (acc, t)
    case t => parseDefs(JavaModifier.empty).flatMap(x => parseInterfaceDefs(acc :+ x))(t)
  }

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


  def assertToken(token: JavaTokenEnum): TokenListState[Unit] = {
    case JavaSToken(tk, _) :: t if tk == token => (Unit, t)
    case t => throw new TokenNotAcceptedException(s"${t.head.tokenType} => ${t.head.value}")
  }

  def parseParenthesis(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): TokenListState[String] = tokens => {
    def loop(cnt: Int, acc: String): TokenListState[String] = {
      case Nil => (acc, Nil)
      case JavaSToken(lp, _) :: t if lp == leftPar => loop(cnt + 1, acc + lp.value)(t)
      case JavaSToken(rp, _) :: t if cnt == 0 && rp == rightPar => (acc + rp.value, t)
      case JavaSToken(rp, _) :: t if rp == rightPar => loop(cnt - 1, acc + rp.value)(t)
      case JavaSToken(_, v) :: t => loop(cnt, acc + v)(t)
    }

    tokens match {
      case JavaSToken(lp, _) :: t if lp == leftPar => loop(0, lp.value)(t)
      case tails => ("", tails)
    }
  }

  def parseUntil(until: JavaTokenEnum): TokenListState[String] = tokens => {
    @tailrec
    def loop(tokens: List[JavaSToken], acc: String): (String, List[JavaSToken]) = tokens match {
      case Nil => (acc, Nil)
      case h :: t if h.tokenType == until => (acc, t)
      case h :: t => loop(t, acc + h.value)
    }

    loop(tokens, "")
  }

  def parseCode(bowl: JavaCode): TokenListState[JavaCode] = {
    case Nil => (bowl, Nil)
    case JavaSToken(ANNOTATION, _) :: t => parseAnnotation.flatMap(_ => parseCode(bowl))(t) // ignore annotation
    case JavaSToken(PACKAGE, _) :: t => parseUntil(SEMICOLON).flatMap(pkgName => parseCode(bowl.setPackageName(pkgName)))(t)
    case JavaSToken(IMPORT, _) :: t => parseUntil(SEMICOLON).flatMap(imp => parseCode(bowl.appendImport(imp)))(t)
    case tokens => parseDefs(JavaModifier.empty).flatMap(x => parseCode(bowl.appendDefinition(x)))(tokens)
  }

  def apply(tokens: List[JavaSToken]): JavaCode = parseCode(emptyCode)(tokens.filterNot(x => List(COMMENT, COMMENT_BLOCK).contains(x.tokenType)))._1
}
