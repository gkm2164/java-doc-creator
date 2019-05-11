package co.gyeongmin.scala.godoc.godoc.doc.writer

import co.gyeongmin.scala.godoc.godoc.DefBlock

trait FunctionWriter {
  def show(defBlock: DefBlock): String
}