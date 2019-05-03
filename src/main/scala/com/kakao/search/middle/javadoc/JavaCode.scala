package com.kakao.search.middle.javadoc

import com.kakao.search.middle.javalang.JavaTokenEnum
import JavaTokenEnum._
import com.kakao.search.middle.exceptions.TokenNotAcceptedException

import scala.annotation.tailrec

case class JavaCode(packageName: String,
                    imports: List[String],
                    defs: List[JavaDefininiton]) {
  def setPackageName(str: String): JavaCode = copy(packageName = str)

  def appendImport(imp: String): JavaCode = this.copy(imports = imports :+ imp)

  def appendDefinition(javaDefininiton: JavaDefininiton): JavaCode = this.copy(defs = defs :+ javaDefininiton)

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

  implicit class tlsMonadExtend[+A](tls: TokenListState[A]) {
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

  def parseAnnotation: TokenListState[String] = tokens => {
    def parseAnnotationName(tokens: List[JavaSToken]): (String, List[JavaSToken]) = {
      def loop(acc: String): TokenListState[String] = {
        case Nil => (acc, Nil)
        case suspectDot :: JavaSToken(_, annName) :: t if suspectDot.tokenType == DOT => loop(acc + s".$annName")(t)
        case JavaSToken(_, n) :: t => (acc + n, t)
      }

      loop("")(tokens)
    }

    val (name, afterName) = parseAnnotationName(tokens)
    (name, afterName match {
      case JavaSToken(LPAR, _) :: _ => takeParenthesis(LPAR, RPAR)(afterName)._2
      case _ => afterName
    })
  }

  def parseDefs(modifier: JavaModifier): TokenListState[JavaDefininiton] = {
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
    def loop(tokens: List[JavaSToken], acc: List[String]): (List[String], List[JavaSToken]) = tokens match {
      case Nil => (acc, Nil)
      case h :: JavaSToken(sp, _) :: t if sp == separator => loop(t, acc :+ h.value)
      case h :: t => (acc :+ h.value, t)
    }
    loop(tokens, Nil)
  }

  def parseType: TokenListState[String] = tokens => {
    val (typename, next) = takeSeperatorIterate(DOT).map(x => x.mkString("."))(tokens) // parse *.*.*
    val (gt, next2) = next match {
      case JavaSToken(LT, _) :: _ => takeParenthesis(LT, GT).map(x => typename + x)(next)
      case _ => (typename, next)
    }

    next2 match {
      case JavaSToken(LBRACKET, _) :: _ =>
        def takeLoop(acc: String): TokenListState[String] = {
          case Nil => (acc, Nil)
          case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: t if t.head.tokenType == LBRACKET => takeLoop(acc + "[]")(t)
          case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: t => (acc + "[]", t)
          case remains => (acc, remains)
        }

        takeLoop("")(next2)
      case _ => (gt, next2)
    }
  }

  def takeToken[A](x: A): TokenListState[A] = tokens => (x, tokens.tail) // 토큰 한개를 흘려보냄

  def parseArgs: TokenListState[List[JavaArgument]] = tokens => {
    def parseArgName(typename: String, arg: JavaArgument): TokenListState[JavaArgument] = tokens => (arg.copy(argumentType = typename, name = tokens.head.value), tokens.tail)

    def parseArg(javaArg: JavaArgument): TokenListState[JavaArgument] = {
      case tokens@JavaSToken(ANNOTATION, _) :: _ => parseAnnotation.flatMap(x => parseArg(javaArg.appendAnnotation(x)))(tokens)
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

  def parseMembers(modifier: JavaModifier): TokenListState[JavaMembers] = tokens => {
    import JavaTokenEnum._
    tokens match {
      case JavaSToken(LT, _) :: t => parseMembers(modifier)(takeParenthesis(LT, GT)(t)._2)
      case _ =>
        val (t, remain) = parseType(tokens)
        val (name, remain2) = if (remain.head.tokenType == TOKEN) {
          (remain.head.value, remain.tail)
        } else {
          (t, remain)
        }

        if (remain2.head.tokenType == LPAR) {
          val (args, remain3) = parseArgs(remain2.tail)
          remain3 match {
            case Nil => throw new TokenNotAcceptedException("nil list!")
            case JavaSToken(SEMICOLON, _) :: tail => (JavaMethod(modifier, name, t, args), tail)
            case JavaSToken(LBRACE, _) :: _ => (JavaMethod(modifier, name, t, args), takeParenthesis(LBRACE, RBRACE)(remain3)._2)
            case JavaSToken(DEFAULT, _) :: tail => (JavaMethod(modifier, name, t, args), parseUntil(SEMICOLON)(tail)._2)
            case h :: _ => throw new TokenNotAcceptedException(h.toString)
          }
        } else {
          if (remain2.head.tokenType == SUBSTITUTE) {
            (JavaMember(modifier, name, t), parseUntil(SEMICOLON)(remain2)._2)
          } else {
            (JavaMember(modifier, name, t), remain2.tail)
          }
        }
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

  def parseClass(modifier: JavaModifier): TokenListState[JavaClass] = tokens => {
    val (name, remain) = (tokens.head.value, tokens.tail)

    @tailrec
    def loop(tokens: List[JavaSToken], acc: List[JavaDefininiton]): (List[JavaDefininiton], List[JavaSToken]) = {
      tokens match {
        case Nil => throw new TokenNotAcceptedException("")
        case JavaSToken(RBRACE, _) :: t => (acc, t)
        case t =>
          val (d, next) = parseDefs(JavaModifier.empty)(t)
          loop(next, acc :+ d)
      }
    }

    val (extendInh, rem2) = if (remain.head.tokenType == EXTENDS) {
      separateByType(remain.tail)
    } else {
      (Nil, remain)
    }

    val (implementH, rem3) = if (rem2.head.tokenType == IMPLEMENTS) {
      separateByType(rem2.tail)
    } else {
      (Nil, rem2)
    }

    if (rem3.head.tokenType != LBRACE) throw new TokenNotAcceptedException(rem3.head.toString)

    val (defs, next) = loop(rem3.tail, Nil)
    (JavaClass(name, modifier, defs, extendInh, implementH), next)
  }

  def parseAnnotationInterface(modifier: JavaModifier): TokenListState[JavaInterface] = tokens => {
    val (name, remain) = (tokens.head.value, tokens.tail)

    @tailrec
    def loop(tokens: List[JavaSToken], acc: List[JavaDefininiton]): (List[JavaDefininiton], List[JavaSToken]) = {
      tokens match {
        case Nil => throw new TokenNotAcceptedException("")
        case JavaSToken(RBRACE, _) :: t => (acc, t)
        case t =>
          val (d, next) = parseDefs(JavaModifier.empty)(t)
          loop(next, acc :+ d)
      }
    }

    val (extendInh, rem2) = if (remain.head.tokenType == EXTENDS) {
      separateByType(remain.tail)
    } else {
      (Nil, remain)
    }

    if (rem2.head.tokenType != LBRACE) {
      throw new TokenNotAcceptedException(s"${rem2.head.tokenType} => ${rem2.head.value}")
    }

    val (defs, next) = loop(rem2.tail, Nil)
    (JavaInterface(name, modifier, defs, extendInh), next)
  }

  def parseInterface(modifier: JavaModifier): TokenListState[JavaInterface] = tokens => {
    val (name, remain) = (tokens.head.value, tokens.tail)

    @tailrec
    def loop(tokens: List[JavaSToken], acc: List[JavaDefininiton]): (List[JavaDefininiton], List[JavaSToken]) = {
      tokens match {
        case Nil => throw new TokenNotAcceptedException("")
        case JavaSToken(RBRACE, _) :: t => (acc, t)
        case t =>
          val (d, next) = parseDefs(JavaModifier.empty)(t)
          loop(next, acc :+ d)
      }
    }

    val (extendInh, rem2) = if (remain.head.tokenType == EXTENDS) {
      separateByType(remain.tail)
    } else {
      (Nil, remain)
    }

    if (rem2.head.tokenType != LBRACE) {
      throw new TokenNotAcceptedException(s"${rem2.head.tokenType} => ${rem2.head.value}")
    }

    val (defs, next) = loop(rem2.tail, Nil)
    (JavaInterface(name, modifier, defs, extendInh), next)
  }

  def takeParenthesis(leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): TokenListState[String] = tokens => {
    @tailrec
    def loop(rem: List[JavaSToken], cnt: Int, acc: String): (String, List[JavaSToken]) = {
      rem match {
        case Nil => (acc, Nil)
        case JavaSToken(lp, _) :: t if lp == leftPar => loop(t, cnt + 1, acc + lp.value)
        case JavaSToken(rp, _) :: t if cnt == 0 && rp == rightPar => (acc + rp.value, t)
        case JavaSToken(rp, _) :: t if rp == rightPar => loop(t, cnt - 1, acc + rp.value)
        case JavaSToken(_, v) :: t => loop(t, cnt, acc + v)
      }
    }

    if (tokens.head.tokenType != leftPar) throw new TokenNotAcceptedException(tokens.head.toString)
    loop(tokens.tail, 0, tokens.head.value)
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
    case JavaSToken(PACKAGE, _) :: t => parseUntil(SEMICOLON).flatMap(str => parseCode(bowl.setPackageName(str)))(t)
    case JavaSToken(IMPORT, _) :: t => parseUntil(SEMICOLON).flatMap(imp => parseCode(bowl.appendImport(imp)))(t)
    case tokens => parseDefs(JavaModifier.empty).flatMap(x => parseCode(bowl.appendDefinition(x)))(tokens)
  }

  def apply(tokens: List[JavaSToken]): JavaCode = parseCode(emptyCode)(tokens.filterNot(x => List(COMMENT, COMMENT_BLOCK).contains(x.tokenType)))._1
}
