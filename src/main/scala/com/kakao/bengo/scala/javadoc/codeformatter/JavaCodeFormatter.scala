package com.kakao.bengo.scala.javadoc.codeformatter

import com.kakao.bengo.javalang.JavaTokenEnum._
import com.kakao.bengo.scala.javadoc.JavaSToken
import com.kakao.bengo.scala.javadoc.codeformatter.syntax._

object JavaCodeFormatter {
  def printCode(codeName: String, tokens: Vector[JavaSToken]): Unit = {
    val reformatTokens = tokens.filterNot(x => List(COMMENT_BLOCK, COMMENT, COMMENT_MACRO_EXPLAIN, COMMENT_MACRO_CODE, COMMENT_MACRO_NAME).contains(x.tokenType))
    println(s"parse ${reformatTokens.map(_.value).mkString(" ")}")
    println(JavaParser.blockStmt(false).collect(reformatTokens.toList))
  }
}
