package co.gyeongmin.lang.javadoc.codeformatter

import co.gyeongmin.lang.javadoc.JavaSToken
import co.gyeongmin.lang.javadoc.codeformatter.monad._
import co.gyeongmin.lang.javalang.JavaTokenEnum
import co.gyeongmin.lang.javalang.JavaTokenEnum._

object JavaCodeFormatter {
  import syntax._

  private val CommentTokens: Seq[JavaTokenEnum] =
    Seq(COMMENT_BLOCK, COMMENT, COMMENT_MACRO_EXPLAIN, COMMENT_MACRO_CODE, COMMENT_MACRO_NAME)

  def printCode(codeName: String, tokens: Vector[JavaSToken]): String = {
    val reformatTokens = tokens.filterNot(x => CommentTokens.contains(x.tokenType)).zipWithIndex.toList

    if (reformatTokens.nonEmpty) {
      JavaParser.blockStmt(false)
        .collect(reformatTokens, CodeWriterConfig(debug = Some(DebugOption(stackTrace = true))))
    }
    else {
      println("there's no code here")
      ""
    }
  }
}