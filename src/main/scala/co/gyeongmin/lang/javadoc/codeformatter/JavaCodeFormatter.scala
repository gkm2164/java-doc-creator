package co.gyeongmin.lang.javadoc.codeformatter

import co.gyeongmin.lang.javadoc.JavaSToken
import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javadoc.config.DebugOption
import co.gyeongmin.lang.javalang.JavaTokenEnum
import co.gyeongmin.lang.javalang.JavaTokenEnum._

object JavaCodeFormatter {

  import syntax._

  private val CommentTokens: Seq[JavaTokenEnum] =
    Seq(COMMENT_BLOCK, COMMENT, COMMENT_MACRO_EXPLAIN, COMMENT_MACRO_CODE, COMMENT_MACRO_NAME)


  private def printCodeCommon[A](codeName: String, tokens: Vector[JavaSToken], startFrom: CodeWriter[A], debugOption: DebugOption): String = {
    val reformatTokens = tokens.filterNot(x => CommentTokens.contains(x.tokenType)).zipWithIndex.toList

    if (reformatTokens.nonEmpty) {
      println(debugOption)
      startFrom.enter()
        .collect(reformatTokens, CodeWriterConfig(debug = debugOption))
    }
    else {
      println("there's no code here")
      ""
    }
  }

  def printCodeBlock(codeName: String, tokens: Vector[JavaSToken], debugOption: DebugOption): String =
    printCodeCommon(codeName, tokens, JavaParser.blockStmt, debugOption)


  def printCode(codeName: String, tokens: Vector[JavaSToken], debugOption: DebugOption): String =
    printCodeCommon(codeName, tokens, JavaParser.javaCode, debugOption)
}
