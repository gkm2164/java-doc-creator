package com.kakao.search.middle.godoc

import java.io.{File, PrintWriter}

import scala.language.{higherKinds, postfixOps}

object Main {

  implicit class ShowDefBlock(x: DefBlock) {
    def show(implicit shower: FunctionWriter): String = {
      shower.show(x)
    }
  }

  def main(args: Array[String]): Unit = {
    //Folder 단위로 불러들임.
    val orgDirName = "/Users/ben.go/go/src/github.daumkakao.com/search-middle/dagore"

    def showFolder(dirName: String): Seq[(String, Seq[DefBlock])] = {
      val d = new File(dirName)
      if (d.exists() && d.isDirectory) {
        d.listFiles.toList.flatMap {
          case x if x.isDirectory =>
            showFolder(s"$dirName/${x.getName}")
          case x if x.isFile && x.getName.endsWith(".go") =>
            Seq(s"$dirName/${x.getName}" -> DocMacroParser.parseGoDoc(s"$dirName/${x.getName}"))
          case _ => Seq.empty
        }
      } else {
        Seq.empty
      }
    }

    implicit val goWriter: FunctionWriter = new FunctionWriter {
      def colorType(t: String) = t.replace("([\\.\\*\\[\\]]+)([a-zA-Z0-9_]+)(.*)", """$1<span class="type-def">$2</span>$3""")

      override def show(defBlock: DefBlock): String = defBlock match {
        case TypeDefBlock(tn, _, _, builder) => (if (builder) "*" else "") + s"""<span class="reserved-keyword">type</span> $tn"""
        case FuncDefBlock(name, argList, returnType, desc, _) =>
          s"""<p><span class="reserved-keyword">func</span> $name(${argList.map(x => s"""${x.name} <span class="type-def">${x.typeName}</span>""").mkString(", ")}) <span class="type-def">${returnType.trim}</span></p>""" +
            desc.map(x => s"\n-- $x").getOrElse("") +
            (if (argList.exists(_.desc != "")) argList.map(x => s"""<li>${x.name} <span class="type-def">${x.typeName}</span> :: ${x.desc}</li>""").mkString("\nhello<ul>", "", "</ul>") else "")
        case ReceiverFuncDefBlock(rName, rType, FuncDefBlock(name, argList, returnType, _, _)) =>
          s"""<span class="reserved-keyword">func</span> ($rName <span class="type-def">$rType</span>) $name(${argList.map(x => s"""${x.name} <span class="type-def">${x.typeName}</span>""").mkString(", ")}) <span class="type-def">${returnType.trim}</span>"""
      }
    }

    val ret = showFolder(orgDirName)

    val pw = new PrintWriter(s"doc.html")
    pw.write("""<!DOCTYPE html><html><head><title>Hello</title><link type="text/css" rel="stylesheet" href="doc.css"/></head><body>""")

    ret.foreach { case (filename, functionDefs) =>
      pw.write(s"<h2>=== at file $filename ===</h2>")
      functionDefs.map(_.show).foreach(println)
      pw.write(functionDefs.map(_.show).map(x => s"<div>$x</div>").mkString("<hr/>"))
    }

    pw.write("</body></html>")
    pw.close
//    println("!!!! only builder types !!!!")

    val graph = constructGraph(ret)

//    println("====")

    val orders = orderTypes(graph)
//    println()
//    orders.foreach(println)
  }

  def orderTypes(graph: Map[TypeDefBlock, List[TypeDefBlock]]): List[TypeDefBlock] = {
    def recur(queue: List[TypeDefBlock], visited: Map[TypeDefBlock, Boolean], acc: List[TypeDefBlock]): List[TypeDefBlock] = {
      queue match {
        case Nil => acc.reverse
        case h :: tail if !visited(h) =>
          val nextQueue = graph(h).filter(x => !visited(x))
          if (nextQueue.isEmpty) recur(tail, visited.updated(h, true), h :: acc)
          else recur(nextQueue ++ tail, visited.updated(h, true), h :: acc)
        case h :: tail if visited(h) => recur(tail, visited, acc)
      }
    }

    val degrees = calculateDegrees(graph)
    val roots = degrees.toList.filter(_._2 == 0).sortBy(_._1.name).map(_._1)

    recur(roots, graph.keys.map(x => x -> false).toMap, Nil)
  }

  def constructGraph(ret: Seq[(String, Seq[DefBlock])]): Map[TypeDefBlock, List[TypeDefBlock]] = {
    val builderTypes = ret.flatMap(_._2).flatMap {
      case x@TypeDefBlock(_, _, _, true) => Some(x)
      case _ => None
    }.toList

    val typeMap = builderTypes.map(x => x.name -> x).toMap

    def recur(types: List[TypeDefBlock], acc: Map[TypeDefBlock, List[TypeDefBlock]]): Map[TypeDefBlock, List[TypeDefBlock]] = types match {
      case Nil => acc
      case h :: tail =>
        val allDefs = ret.flatMap(_._2)
        val allMembers = allDefs.flatMap {
          case f@FuncDefBlock(fname, _, retType, _, _) if fname.startsWith("New") && retType.contains(h.name) => Some(f)
          case r@ReceiverFuncDefBlock(_, rt, _) if rt.contains(h.name) => Some(r)
          case _ => None
        }

        val typeNames = builderTypes.map(_.name)
        val allDependent = allMembers.flatMap {
          case FuncDefBlock(_, args, _, _, _) => args.map(_.typeName)
          case ReceiverFuncDefBlock(_, _, FuncDefBlock(_, args, _, _, _)) => args.map(_.typeName)
          case _ => Nil
        }.map(_.replaceAllLiterally("*", "").replaceAllLiterally("...", "")).distinct.filter(x => typeNames.contains(x))

//        println(s"type ${h.name} depends on => $allDependent")

        val t = h -> allDependent.map(x => typeMap(x)).toList
        recur(tail, acc + t)
    }

    recur(builderTypes, Map.empty)
  }

  def merge[T](a: Map[T, Int], b: Map[T, Int]): Map[T, Int] = {
    val aKey = a.keys.toSet
    val bKey = b.keys.toSet

    val keys = aKey ++ bKey
    keys.foldLeft(Map.empty[T, Int]) { (acc, elem) => acc.updated(elem, a.getOrElse(elem, 0) + b.getOrElse(elem, 0)) }
  }

  def calculateDegrees(typeMap: Map[TypeDefBlock, List[TypeDefBlock]]): Map[TypeDefBlock, Int] = {
    typeMap.keys.foldLeft(typeMap.mapValues(_ => 0)) { (acc, tb) =>
      merge(acc, typeMap(tb).foldLeft(Map.empty[TypeDefBlock, Int].withDefault(_ => 0)) { (a, t) =>
        a.updated(t, a(t) + 1)
      })
    }
  }
}

trait FunctionWriter {
  def show(defBlock: DefBlock): String
}