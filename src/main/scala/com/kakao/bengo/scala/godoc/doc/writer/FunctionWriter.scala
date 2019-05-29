package com.kakao.bengo.scala.godoc.doc.writer

import com.kakao.bengo.scala.godoc.DefBlock

trait FunctionWriter {
  def show(defBlock: DefBlock): String
}