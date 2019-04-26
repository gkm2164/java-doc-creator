package com.kakao.search.middle.godoc.doc.writer

import com.kakao.search.middle.godoc.DefBlock

trait FunctionWriter {
  def show(defBlock: DefBlock): String
}