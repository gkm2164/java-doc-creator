package com.kakao.search.middle.javadoc

import java.io.{File, FilenameFilter}

import com.kakao.search.middle.exceptions.TokenNotAcceptedException
import com.kakao.search.middle.javalang.{JavaTokenEnum, Tokenizer}
import JavaTokenEnum._

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.Source

case class JavaSToken(tokenType: JavaTokenEnum, value: String)

sealed trait JavaDefininiton {
  def show: String

  def modifier: JavaModifier
}

case class JavaModifier(commentMacros: List[String], annotations: List[String], access: JavaTokenEnum, isStatic: Boolean, isFinal: Boolean, isAbstract: Boolean) {
  def show: String = s"${commentMacros.mkString("\n")}"

  def appendMacro(v: String): JavaModifier = this.copy(commentMacros = commentMacros :+ v)

  def appendAnnotation(annotation: String): JavaModifier = this.copy(annotations = annotations :+ annotation)

  def setAccess(access: JavaTokenEnum): JavaModifier = this.copy(access = access)

  def setStatic: JavaModifier = this.copy(isStatic = true)

  def setFinal: JavaModifier = this.copy(isFinal = true)

  def setAbstract: JavaModifier = this.copy(isAbstract = true)
}

object JavaModifier {
  def empty: JavaModifier = JavaModifier(Nil, Nil, PUBLIC, isStatic = false, isFinal = false, isAbstract = false)
}

sealed trait JavaTypeDef extends JavaDefininiton {
  def inheritClass: List[String]

  def implementInterfaces: List[String]
}

case class JavaClass(name: String, modifier: JavaModifier,
                     definitions: List[JavaDefininiton],
                     inheritClass: List[String],
                     implementInterfaces: List[String]) extends JavaTypeDef {
  override def show: String = s"== class $name (${modifier.show}) ==\n\n" + definitions.map(_.show).mkString("\n")
}

case class JavaInterface(name: String, modifier: JavaModifier,
                         definition: List[JavaDefininiton],
                         inheritClass: List[String]) extends JavaTypeDef {
  override def implementInterfaces: List[String] = Nil

  override def show: String = s"== interface $name ==\n${this}"
}

case class JavaAnnotationInterface(name: String, modifier: JavaModifier,
                                   definitions: List[JavaDefininiton],
                                   inheritClass: List[String]) extends JavaTypeDef {
  override def implementInterfaces: List[String] = Nil

  override def show: String = s"== annotation interface $name ==\n${this}"
}

trait JavaMembers extends JavaDefininiton

case class JavaMember(modifier: JavaModifier, name: String, memberType: String) extends JavaMembers {
  def setName(n: String): JavaMember = this.copy(name = n)

  def setType(t: String): JavaMember = this.copy(memberType = t)

  override def show: String = s"== member variable $name ==\n${this}"
}

case class JavaMethod(modifier: JavaModifier, name: String, returnType: String, args: List[JavaArgument]) extends JavaMembers {
  override def show: String = s"== method $name ==\n$this"
}

case class JavaArgument(annotations: List[String], isFinal: Boolean, name: String, argumentType: String) {
  def setFinal: JavaArgument = copy(isFinal = true)

  def appendAnnotation(annos: String): JavaArgument = copy(annotations = annotations :+ annos)

}

object JavaArgument {
  def empty: JavaArgument = JavaArgument(Nil, isFinal = false, "", "")
}

case class JavaPackageDef(annotations: List[String], name: String)

case class JavaCode(packageName: String,
                    imports: List[String],
                    defs: List[JavaDefininiton]) {
  def setPackageName(str: String): JavaCode = copy(packageName = str)

  def appendImport(imp: String): JavaCode = this.copy(imports = imports :+ imp)

  def appendDefinition(javaDefininiton: JavaDefininiton): JavaCode = this.copy(defs = defs :+ javaDefininiton)

  override def toString: String = {
    val sb = StringBuilder.newBuilder
    sb.append(s"=== packagename: $packageName ===").append("\n\n")
    sb.append(imports.map(x => s"import $x").mkString("\n")).append("\n")
    sb.append(s"total ${defs.length} number of definitions are contained").append("\n")
    sb.append(defs.map(_.show).mkString("\n"))
    sb.toString()
  }
}

object JavaCode {
  def emptyCode: JavaCode = JavaCode("", Nil, Nil)

  def parseAnnotation(tokens: List[JavaSToken]): (String, List[JavaSToken]) = {
    def parseAnnotationName(tokens: List[JavaSToken]): (String, List[JavaSToken]) = {
      def loop(tokens: List[JavaSToken], acc: String): (String, List[JavaSToken]) = {
        tokens match {
          case Nil => (acc, Nil)
          case suspectDot :: JavaSToken(_, annName) :: t if suspectDot.tokenType == DOT => loop(t, acc + s".$annName")
          case JavaSToken(_, n) :: t => (acc + n, t)
        }
      }

      loop(tokens, "")
    }

    val (name, afterName) = parseAnnotationName(tokens)
    (name, afterName match {
      case JavaSToken(LPAR, _) :: _ => dropParenthesis(afterName, LPAR, RPAR)
      case _ => afterName
    })
  }

  def parseDefs(tokens: List[JavaSToken], modifier: JavaModifier): (JavaDefininiton, List[JavaSToken]) = {
    import JavaTokenEnum._
    tokens match {
      case Nil => throw new TokenNotAcceptedException("token is empty")
      case JavaSToken(ANNOTATION, _) :: t =>
        val (ann, next) = parseAnnotation(t)
        parseDefs(next, modifier.appendAnnotation(ann))
      case JavaSToken(access@(PRIVATE | PROTECTED | PUBLIC), _) :: t => parseDefs(t, modifier.setAccess(access))
      case JavaSToken(STATIC, _) :: t => parseDefs(t, modifier.setStatic)
      case JavaSToken(FINAL, _) :: t => parseDefs(t, modifier.setFinal)
      case JavaSToken(ABSTRACT, _) :: t => parseDefs(t, modifier.setAbstract)
      case JavaSToken(COMMENT_MACRO, v) :: t => parseDefs(t, modifier.appendMacro(v))
      case JavaSToken(CLASS, _) :: t => parseClass(t, modifier)
      case JavaSToken(INTERFACE, _) :: t => parseInterface(t, modifier)
      case JavaSToken(ANNOTATION_INTERFACE, _) :: t => parseAnnotationInterface(t, modifier)
      case JavaSToken(TOKEN, _) :: _ => parseMembers(tokens, modifier)
      case h :: _ => throw new TokenNotAcceptedException(s"${h.tokenType} => ${h.value}")
    }
  }

  def takeSeperatorIterate(tokens: List[JavaSToken], separator: JavaTokenEnum): (List[String], List[JavaSToken]) = {
    def loop(tokens: List[JavaSToken], acc: List[String]): (List[String], List[JavaSToken]) = tokens match {
      case Nil => (acc, Nil)
      case h :: JavaSToken(sp, _) :: t if sp == separator => loop(t, acc :+ h.value)
      case h :: t => (acc :+ h.value, t)
    }

    loop(tokens, Nil)
  }

  def parseType(tokens: List[JavaSToken]): (String, List[JavaSToken]) = {
    import JavaTokenEnum._
    val (tn, next) = takeSeperatorIterate(tokens, DOT) // parse *.*.*
    val typename = tn.mkString(".")
    val (gt, next2) = next match {
      case JavaSToken(LT, _) :: _ =>
        val (pars, n2) = takeParenthesis(next, LT, GT)
        (typename + pars, n2)
      case _ => (typename, next)
    }

    next2 match {
      case JavaSToken(LBRACKET, _) :: _ =>
        def takeLoop(tokens: List[JavaSToken], acc: String): (String, List[JavaSToken]) = tokens match {
          case Nil => (acc, Nil)
          case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: t if t.head.tokenType == LBRACKET => takeLoop(t, acc + "[]")
          case JavaSToken(LBRACKET, _) :: JavaSToken(RBRACKET, _) :: t => (acc + "[]", t)
          case remains => (acc, remains)
        }

        takeLoop(next2, "")
      case _ => (gt, next2)
    }
  }

  def parseArgs(tokens: List[JavaSToken]): (List[JavaArgument], List[JavaSToken]) = {
    import JavaTokenEnum._
    @tailrec
    def parseArg(tokens: List[JavaSToken], acc: JavaArgument): (JavaArgument, List[JavaSToken]) = {
      tokens match {
        case JavaSToken(ANNOTATION, _) :: _ =>
          val (annotation, remains) = parseAnnotation(tokens)
          parseArg(remains, acc.appendAnnotation(annotation))
        case JavaSToken(FINAL, _) :: t => parseArg(t, acc.setFinal)
        case _ =>
          val (t, rem) = parseType(tokens)
          (acc.copy(argumentType = t, name = rem.head.value), rem.tail)
      }
    }

    @tailrec
    def loop(tokens: List[JavaSToken], acc: List[JavaArgument]): (List[JavaArgument], List[JavaSToken]) =
      tokens match {
        case JavaSToken(RPAR, _) :: t => (acc, t)
        case JavaSToken(COMMA, _) :: t => loop(t, acc)
        case _ =>
          val (arg, next) = parseArg(tokens, JavaArgument.empty)
          loop(next, acc :+ arg)
      }

    loop(tokens, Nil)
  }

  def parseMembers(tokens: List[JavaSToken], modifier: JavaModifier): (JavaMembers, List[JavaSToken]) = {
    import JavaTokenEnum._
    //    println("members: " + tokens.head)
    tokens match {
      case JavaSToken(LT, _) :: t => parseMembers(dropParenthesis(t, LT, GT), modifier)
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
            case JavaSToken(LBRACE, _) :: _ => (JavaMethod(modifier, name, t, args), dropParenthesis(remain3, LBRACE, RBRACE))
            case JavaSToken(DEFAULT, _) :: tail => (JavaMethod(modifier, name, t, args), parseUntil(tail, SEMICOLON)._2)
            case h :: _ => throw new TokenNotAcceptedException(h.toString)
          }
        } else {
          if (remain2.head.tokenType == SUBSTITUTE) {
            (JavaMember(modifier, name, t), parseUntil(remain2, SEMICOLON)._2)
          } else {
            (JavaMember(modifier, name, t), remain2.tail)
          }
        }
    }
  }

  def separateByType(tokens: List[JavaSToken]): (List[String], List[JavaSToken]) = {
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

  def parseClass(tokens: List[JavaSToken], modifier: JavaModifier): (JavaClass, List[JavaSToken]) = {
    val (name, remain) = (tokens.head.value, tokens.tail)

    def loop(tokens: List[JavaSToken], acc: List[JavaDefininiton]): (List[JavaDefininiton], List[JavaSToken]) = {
      tokens match {
        case Nil => throw new TokenNotAcceptedException("")
        case JavaSToken(RBRACE, _) :: t => (acc, t)
        case t =>
          val (d, next) = parseDefs(t, JavaModifier.empty)
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

  def parseAnnotationInterface(tokens: List[JavaSToken], modifier: JavaModifier): (JavaInterface, List[JavaSToken]) = {
    val (name, remain) = (tokens.head.value, tokens.tail)

    def loop(tokens: List[JavaSToken], acc: List[JavaDefininiton]): (List[JavaDefininiton], List[JavaSToken]) = {
      tokens match {
        case Nil => throw new TokenNotAcceptedException("")
        case JavaSToken(RBRACE, _) :: t => (acc, t)
        case t =>
          val (d, next) = parseDefs(t, JavaModifier.empty)
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

  def parseInterface(tokens: List[JavaSToken], modifier: JavaModifier): (JavaInterface, List[JavaSToken]) = {
    val (name, remain) = (tokens.head.value, tokens.tail)

    def loop(tokens: List[JavaSToken], acc: List[JavaDefininiton]): (List[JavaDefininiton], List[JavaSToken]) = {
      tokens match {
        case Nil => throw new TokenNotAcceptedException("")
        case JavaSToken(RBRACE, _) :: t => (acc, t)
        case t =>
          val (d, next) = parseDefs(t, JavaModifier.empty)
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

  def takeParenthesis(tokens: List[JavaSToken], leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): (String, List[JavaSToken]) = {
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

  def dropParenthesis(tokens: List[JavaSToken], leftPar: JavaTokenEnum, rightPar: JavaTokenEnum): List[JavaSToken] =
    takeParenthesis(tokens, leftPar, rightPar)._2

  def parseUntil(tokens: List[JavaSToken], until: JavaTokenEnum): (String, List[JavaSToken]) = {
    def loop(tokens: List[JavaSToken], acc: String): (String, List[JavaSToken]) = tokens match {
      case Nil => (acc, Nil)
      case h :: t if h.tokenType == until => (acc, t)
      case h :: t => loop(t, acc + h.value)
    }

    loop(tokens, "")
  }

  def parsePackage(tokens: List[JavaSToken]): (String, List[JavaSToken]) = {
    parseUntil(tokens, SEMICOLON)
  }

  def parseImport(tokens: List[JavaSToken]): (String, List[JavaSToken]) = parseUntil(tokens, JavaTokenEnum.SEMICOLON)


  def parseCode(tokens: List[JavaSToken], bowl: JavaCode): JavaCode = {
    import JavaTokenEnum._
    tokens match {
      case Nil => bowl
      case JavaSToken(ANNOTATION, _) :: t =>
        val (_, tail) = parseAnnotation(t)
        parseCode(tail, bowl)
      case JavaSToken(PACKAGE, _) :: t =>
        val (str, tail) = parsePackage(t)
        parseCode(tail, bowl.setPackageName(str))
      case JavaSToken(IMPORT, _) :: t =>
        val (imp, tail) = parseImport(t)
        parseCode(tail, bowl.appendImport(imp))
      case _ =>
        val (definition, tail) = parseDefs(tokens, JavaModifier.empty)
        parseCode(tail, bowl.appendDefinition(definition))
    }
  }

  def apply(tokens: List[JavaSToken]): JavaCode = parseCode(tokens.filterNot(x => List(JavaTokenEnum.COMMENT, JavaTokenEnum.COMMENT_BLOCK).contains(x.tokenType)), emptyCode)
}

sealed trait CodeNode {
  def name: String
  def print(): Unit
}

case class CodeLeaf(name: String, packageName: String, tokens: List[JavaSToken]) extends CodeNode {
  override def print(): Unit = {
    println(s"at filename: $name, in package: $packageName")
    //    println(tokens.mkString(" "))
    println(analyze())
  }

  def analyze(): JavaCode = JavaCode(tokens)
}

case class CodeNonLeaf(name: String, codeNodes: Map[String, CodeNode]) extends CodeNode {
  override def print: Unit = {
    println(s"===$name===")
    codeNodes.foreach { case (childName, node) =>
      println(s"=> $childName")
      node.print
    }
  }
}

object Main {
  val onlyJavaAndDirFilter: FilenameFilter = (dir: File, name: String) => {
    println(name)
    dir.isDirectory || (dir.isFile && dir.getName.endsWith(".java"))
  }

  def main(args: Array[String]): Unit = {
    val baseDir = "/Users/ben.go/java/da-commons/da-intent-handler/src/main/java"
    val node = currentPackage(new File(baseDir))
    node.print
  }

  def currentPackage(currentHandle: File): CodeNonLeaf = {
    def loop(currentHandle: File, pkgNameAcc: Seq[String]): CodeNode = {
      if (currentHandle.isFile) {
        val filename = currentHandle.getName
        val packageName = pkgNameAcc.mkString(".")
        println(s"file name: ${pkgNameAcc.mkString(".")}.$filename")
        val src = Source.fromFile(currentHandle.getAbsolutePath)
        val tokens = Tokenizer.tokenize(src.mkString("")).asScala
        CodeLeaf(filename, packageName, tokens.map(x => JavaSToken(x.getE, x.getValue)).toList)
      } else {
        println(s"package name: ${pkgNameAcc.mkString(".")}.${currentHandle.getName}")

        CodeNonLeaf(currentHandle.getName, currentHandle.listFiles(onlyJavaAndDirFilter).map(x => x.getName -> loop(x, pkgNameAcc :+ currentHandle.getName)).toMap)
      }
    }

    CodeNonLeaf("", currentHandle.listFiles(onlyJavaAndDirFilter).map(x => x.getName -> loop(x, Nil)).toMap)
  }

  def getType(currentHandle: File): String = if (currentHandle.isFile) "FILE" else if (currentHandle.isDirectory) "DIRECTORY" else "UNKNOWN"
}
