package com.kakao.bengo.scala.javadoc.writer

import com.kakao.bengo.scala.javadoc.JavaSToken

class NotIdentifiedYetException(h: JavaSToken) extends Exception(s"$h is not identified yet")
