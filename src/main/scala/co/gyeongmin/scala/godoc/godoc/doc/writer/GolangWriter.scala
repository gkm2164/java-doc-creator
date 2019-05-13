package co.gyeongmin.scala.godoc.godoc.doc.writer

import co.gyeongmin.scala.godoc.godoc.{DefBlock, FuncDefBlock, ReceiverFuncDefBlock, TypeDefBlock}
import co.gyeongmin.scala.godoc.godoc.doc.elements.Argument

object GolangWriter {
  implicit val goWriter: FunctionWriter = new FunctionWriter {
    def encapsulateWithTag(str: String): String = {
      if (str != "")
        s"""<span class="type-def">$str</span>"""
      else ""
    }

    def colorType(t: String): String = {
      def recur(rem: String, buf: String, acc: String): String = {
        if (rem == "") {
          if (buf != "") acc + s"""<span class="type-def">$buf</span>"""
          else acc
        }
        else {
          if ("().*[],".indexOf(rem.head) >= 0) recur(rem.tail, "", acc + encapsulateWithTag(buf) + rem.head)
          else recur(rem.tail, buf + rem.head, acc)
        }
      }

      recur(t, "", "")
    }

    def showArgument(arg: Argument): String = arg match {
      case Argument(name, typeName, _) => s"""$name ${colorType(typeName)}"""
    }

    def showDesc(desc: Option[String], ex: Option[String]): String = desc.map(x => s"""<p>$x</p>""").getOrElse("") + ex.map(x => s"""<pre class="code">$x</pre>""").getOrElse("")

    override def show(fd: DefBlock): String = fd match {
      case FuncDefBlock(name, argList, returnType, desc, ex) =>
        s"""<p><span class="reserved-keyword">func</span> $name(${argList.map(showArgument).mkString(", ")}) ${colorType(returnType)}</p>""" + showDesc(desc, ex)
      case ReceiverFuncDefBlock(rName, rType, FuncDefBlock(name, argList, returnType, desc, ex)) =>
        s"""<span class="reserved-keyword">func</span> ($rName ${colorType(rType)}) $name(${argList.map(showArgument).mkString(", ")}) ${colorType(returnType)}""" + showDesc(desc, ex)
      case TypeDefBlock(name, _, implements, desc, ex, _) => s"""<span class="reserved-keyword">type</span> $name ${implements.map(x => s"[$x]").mkString(s":implements =>", ", ", "")}""" + showDesc(desc, ex)
    }
  }
}
