package com.kakao.search.middle.godoc.doc

package object elements {

  type VarName = String
  type VarType = String

  sealed trait ArrayElem

  sealed trait SomeDef

  case class TypeName(name: String) extends ArrayElem

  case class MacroName(name: String) extends ArrayElem

  case class Text(txt: String) extends ArrayElem

  case class ArgList(args: List[Argument]) extends ArrayElem

  case class Argument(name: String, typeName: String, desc: String) extends ArrayElem

  case class TypeRequired(f: Map[VarName, VarType => Argument]) extends ArrayElem {
    def resolveTypes(name: Map[VarName, VarType]): Seq[Argument] = (for {
      (n, t) <- name
    } yield f.getOrElse(n, Argument(n, _, ""))(t)).toSeq
  }

  case class TypeDef(typeName: String) extends SomeDef

  case class ReceiverFunctionDef(receiverName: String, receiverType: String, name: String, arguments: Seq[Argument], returnType: String) extends SomeDef

  case class FunctionDef(name: String, arguments: Seq[Argument], returnType: String) extends SomeDef

  object TypeRequired {
    def empty: TypeRequired = TypeRequired(Map.empty)
  }

  case object Builder extends ArrayElem

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

  object ArgList {
    def parse(code: String): TypeRequired = {
      def recur(it: Iterator[String], acc: Map[VarName, VarType => Argument]): Map[VarName, VarType => Argument] = {
        if (!it.hasNext) acc
        else {
          val arg = it.next()
          val (k, v) = arg.splitAt(arg.indexOf(':'))
          val key = k
          val desc = Text.parse(v.dropWhile(_ == ':')).txt
          recur(it, acc + (key -> ((t: String) => Argument(key, t, desc))))
        }
      }

      val ret = recur(new Iterator[String] {
        var remains: String = code.dropWhile(_ == '[').trim

        override def hasNext: Boolean = !remains.startsWith("]")

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

          val (rem, ret) = thisRecur(remains, escape = false, insideBlock = false, "")
          remains = rem.trim()
          ret.trim
        }
      }, Map.empty)

      TypeRequired(ret)
    }
  }

}
