package com.kakao.search.middle.godoc
import scala.collection.immutable


sealed trait ArrayElem

case class TypeName(name: String) extends ArrayElem

case class MacroName(name: String) extends ArrayElem

case class Text(txt: String) extends ArrayElem

object Text {
  // { ~ } 블럭 사이의 텍스트 파싱
  def parse(str: String): Text = {
    def recur(remain: String, escape: Boolean, acc: String): Option[Text] =
      if (remain == "") Some(Text(acc))
      else remain.head match {
        case '\\' if !escape => recur(remain.tail, escape = true, acc)
        case '\\' if escape => recur(remain.tail, escape = false, acc + '\\')
        case '}' if escape => recur(remain.tail, escape = false, acc + '}')
        case '}' if !escape => Some(Text(acc))
        case x if !escape => recur(remain.tail, escape = false, acc + x)
        case _ => None
      }

    recur(str.trim.dropWhile(_ == '{'), escape = false, "").getOrElse(Text(""))
  }
}

case class ArgList(args: List[Argument]) extends ArrayElem


object ArgList {
  def parse(code: String): TypeRequired = {
    def recur(it: Iterator[String], acc: Seq[(String, String => Argument)]): Seq[(String, String => Argument)] = {
      if (!it.hasNext) acc
      else {
        val arg = it.next()
        val (k, v) = arg.splitAt(arg.indexOf(':'))
        val key = k
        val desc = Text.parse(v.dropWhile(_ == ':')).txt
        println(s"$key :: $desc")
        recur(it, acc :+ (key -> ((t: String) => Argument(key, t, desc))))
      }
    }

    val ret = recur(new Iterator[String] {
      var r = code.dropWhile(_ == '[').trim

      override def hasNext: Boolean = !r.startsWith("]")

      override def next(): String = {
        def thisRecur(remain: String, escape: Boolean, insideBlock: Boolean, acc: String): (String, String) = {
          if (insideBlock) {
            remain.head match {
              case '\\' if !escape => thisRecur(remain.tail, escape = true, insideBlock = true, acc + '\\')
              case '\\' if escape => thisRecur(remain.tail, escape = false, insideBlock = true, acc + '\\')
              case '}' if escape => thisRecur(remain.tail, escape = false, insideBlock = true, acc + '}')
              case '}' if !escape => (remain.tail.trim.dropWhile(_ == ','), acc + '}')
              case ch => thisRecur(remain.tail, escape = false, insideBlock = true, acc + ch)
            }
          } else {
            remain.head match {
              case '{' => thisRecur(remain.tail, escape = false, insideBlock = true, acc + '{')
              case ch => thisRecur(remain.tail, escape = false, insideBlock = false, acc + ch)
            }
          }
        }

        val (rem, ret) = thisRecur(r, escape = false, insideBlock = false, "")
        r = rem.trim()
        ret
      }
    }, Nil)
    TypeRequired(ret.toMap)
  }
}

case class Argument(name: String, typeName: String, desc: String) extends ArrayElem

case class TypeRequired(f: Map[String, String => Argument]) extends ArrayElem {
  def apply(name: Map[String, String]): Seq[Argument] = (for {
    (n, t) <- name
  } yield f(n)(t)).toSeq
}

sealed trait SomeDef

case class TypeDef(typeName: String) extends SomeDef

case class ReceiverFunctionDef(receiverName: String, receiverType: String, name: String, arguments: Seq[Argument], returnType: String) extends SomeDef

case class FunctionDef(name: String, arguments: Seq[Argument], returnType: String) extends SomeDef