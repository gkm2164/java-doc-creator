package com.kakao.search.middle.javadoc

import java.io.{File, FilenameFilter}

import com.kakao.search.middle.javadoc.JavaAccessType.JavaAccessType
import com.kakao.search.middle.javalang.{JavaTokenEnum, Tokenizer}

import scala.collection.JavaConverters._
import scala.io.Source

object JavaAccessType extends Enumeration {
  type JavaAccessType = Value
  val PUBLIC, PROTECTED, PRIVATE = Value
}

case class JavaSToken(tokenType: JavaTokenEnum, value: String)

sealed trait JavaDefininiton

case class JavaClass(annotations: List[String], name: String, access: JavaAccessType, members: List[JavaMember], methods: List[JavaMethod]) extends JavaDefininiton
case class JavaInterface(annotations: List[String], name: String, access: JavaAccessType, members: List[JavaMember], methods: List[JavaMethod]) extends JavaDefininiton
case class JavaMember(annotations: List[String], name: String, accessType: JavaAccessType, memberType: String) extends JavaDefininiton
case class JavaMethod(annotations: List[String], name: String, accessType: JavaAccessType, returnType: String, args: List[JavaArgument])
case class JavaArgument(annotations: List[String], name: String, argumentType: String)

sealed trait CodeNode {
  def name: String

  def print: Unit
}

case class CodeLeaf(name: String, packageName: String, tokens: List[JavaSToken]) extends CodeNode {
  override def print: Unit = {
    println(s"at filename: $name, in package: $packageName")
    println(tokens.mkString(" "))

  }
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
  val onlyJavaAndDir: FilenameFilter = (dir: File, name: String) => dir.isDirectory || (dir.isFile && name.endsWith(".java"))

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

        CodeNonLeaf(currentHandle.getName, currentHandle.listFiles(onlyJavaAndDir).map(x => x.getName -> loop(x, pkgNameAcc :+ currentHandle.getName)).toMap)
      }
    }

    CodeNonLeaf("", currentHandle.listFiles(onlyJavaAndDir).map(x => x.getName -> loop(x, Nil)).toMap)
  }

  def getType(currentHandle: File): String = if (currentHandle.isFile) "FILE" else if (currentHandle.isDirectory) "DIRECTORY" else "UNKNOWN"
}
