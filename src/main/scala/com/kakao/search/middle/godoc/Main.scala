package com.kakao.search.middle.godoc

import java.io.{File, PrintWriter}

import com.kakao.search.middle.godoc.doc.elements.Argument

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

    val ret = showFolder(orgDirName)

    val pw = new PrintWriter(s"doc.html")
    pw.write("""<!DOCTYPE html><html><head><title>Hello</title><link type="text/css" rel="stylesheet" href="doc.css"/></head><body>""")

    ret.foreach { case (_, functionDefs) =>
      pw.write(functionDefs.map(_.show).map(x => s"<div>$x</div>").mkString("<hr/>"))
    }

    pw.write("</body></html>")
    pw.close()

    val graph = constructGraph(ret)
    val orders = orderTypesBFS(graph)
    val allDefs = ret.flatMap(_._2).toList
    val bgs: List[BuilderGroup] = asBuilderGroups(orders, allDefs)

    val pw2 = new PrintWriter(s"dependencies.html")
    pw2.write("""<!DOCTYPE html><html><head><title>Hello</title><link type="text/css" rel="stylesheet" href="doc.css"/></head><body>""")

    for (bg <- bgs) {

      pw2.write(s"<h1>for ${bg.typeName}</h1>")
      bg.constructor.foreach(x => pw2.write(s"""<p>$x</p>"""))
      pw2.write("<br/>")
      pw2.write(s"== setters for ${bg.typeName} ==")
      pw2.write("<br/>")
      bg.setters.foreach(x => pw2.write(s"""<p>$x</p>"""))
      pw2.write("<br/>")
      pw2.write(s"== methods for ${bg.typeName} ==")
      pw2.write("<br/>")
      bg.methods.foreach(x => pw2.write(s"""<p>$x</p>"""))

      pw2.write("<br/>")
    }

    pw2.write("</body></html>")
    pw2.close()
  }

  def asBuilderGroups(elems: List[TypeDefBlock], allDefs: List[DefBlock]): List[BuilderGroup] = {
    def recur(elems: List[TypeDefBlock], acc: List[BuilderGroup]): List[BuilderGroup] = elems match {
      case Nil => acc
      case h :: tail if h.builder =>
        val allMembers: List[BuilderGroupElem] = allDefs.flatMap {
          case FuncDefBlock(name, argList, retType, desc, ex) if name.startsWith("New") && retType.contains(h.name) => Some(Constructor(h.name, name, argList, desc, ex))
          case ReceiverFuncDefBlock(_, rt, FuncDefBlock(name, argList, frt, desc, ex)) if rt.contains(h.name) =>
            Some(if (rt == frt && argList.length == 1 && name.updated(0, name(0).toLower) == argList.head.name) {
              Setter(rt, name, argList.head.typeName, desc, ex)
            } else {
              Method(rt, name, argList, desc, ex)
            })
          case _ => None
        }

        val constructors = allMembers.collect { case x: Constructor => x }
        val setters = allMembers.collect { case x: Setter => x }
        val methods = allMembers.collect { case x: Method => x }

        recur(tail, acc :+ BuilderGroup(h.name, constructors, setters, methods))
      case _ :: tail => recur(tail, acc) // ignore if it is not builder type
    }

    recur(elems, Nil)
  }

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
      case TypeDefBlock(name, _, implements, desc, ex, _) => s"""<span class="reserved-keyword">type</span> $name ${implements.map(x => s":implements => $x").getOrElse("")}""" + showDesc(desc, ex)
    }
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


  def orderTypesBFS(graph: Map[TypeDefBlock, List[TypeDefBlock]]): List[TypeDefBlock] = {
    def recur(queue: List[TypeDefBlock], visited: Map[TypeDefBlock, Boolean], acc: List[TypeDefBlock]): List[TypeDefBlock] = {
      queue match {
        case Nil => acc.reverse
        case h :: tail if !visited(h) =>
          val nextQueue = graph(h).filter(x => !visited(x))
          if (nextQueue.isEmpty) recur(tail, visited.updated(h, true), h :: acc)
          else recur(tail ++ nextQueue, visited.updated(h, true), h :: acc)
        case h :: tail if visited(h) => recur(tail, visited, acc)
      }
    }

    val degrees = calculateDegrees(graph)
    val roots = degrees.toList.filter(_._2 == 0).sortBy(_._1.name).map(_._1)

    recur(roots, graph.keys.map(x => x -> false).toMap, Nil)
  }

  implicit val intMergeMonoid: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0
    override def op(a: Int, b: Int): Int = a + b
  }

  def calculateDegrees(typeMap: Map[TypeDefBlock, List[TypeDefBlock]]): Map[TypeDefBlock, Int] = {
    typeMap.keys.foldLeft(typeMap.mapValues(_ => 0)) { (acc, tb) =>
      merge(acc, typeMap(tb).foldLeft(Map.empty[TypeDefBlock, Int].withDefault(_ => 0)) { (a, t) =>
        a.updated(t, a(t) + 1)
      })
    }
  }

  trait Monoid[V] {
    def op(a: V, b: V): V
    def zero: V
  }

  def merge[K, V: Monoid](a: Map[K, V], b: Map[K, V]): Map[K, V] = {
    val mn = implicitly[Monoid[V]]
    val aKey = a.keys.toSet
    val bKey = b.keys.toSet

    val keys = aKey ++ bKey
    keys.foldLeft(Map.empty[K, V]) { (acc, elem) => acc.updated(elem, mn.op(a.getOrElse(elem, mn.zero), b.getOrElse(elem, mn.zero))) }
  }

  def constructGraph(ret: Seq[(String, Seq[DefBlock])]): Map[TypeDefBlock, List[TypeDefBlock]] = {
    val builderTypes = ret.flatMap(_._2).flatMap {
      case x@TypeDefBlock(_, _, _, _, _, true) => Some(x)
      case _ => None
    }.toList

    val typeMap = builderTypes.map(x => x.name -> x).toMap

    def recur(types: List[TypeDefBlock], acc: Map[TypeDefBlock, List[TypeDefBlock]]): Map[TypeDefBlock, List[TypeDefBlock]] = types match {
      case Nil => acc
      case h :: tail =>
        val allDefs = ret.flatMap(_._2)
        val allMembers = allDefs.flatMap {
          case f@FuncDefBlock(functionName, _, retType, _, _) if functionName.startsWith("New") && retType.contains(h.name) => Some(f)
          case r@ReceiverFuncDefBlock(_, rt, _) if rt.contains(h.name) => Some(r)
          case _ => None
        }

        val typeNames = builderTypes.map(_.name)
        val allDependent = allMembers.flatMap {
          case FuncDefBlock(_, args, _, _, _) => args.map(_.typeName)
          case ReceiverFuncDefBlock(_, _, FuncDefBlock(_, args, _, _, _)) => args.map(_.typeName)
          case _ => Nil
        }.map(_.replaceAllLiterally("*", "").replaceAllLiterally("...", "")).distinct.filter(x => typeNames.contains(x))


        val t = h -> allDependent.map(x => typeMap(x)).toList
        recur(tail, acc + t)
    }

    val res1 = recur(builderTypes, Map.empty)

    val dependTypes = builderTypes.filter(x => x.implements.isDefined)

    val implementRelation: Map[TypeDefBlock, List[TypeDefBlock]] =
      dependTypes.map(x => typeMap(x.implements.get) -> x)
        .foldLeft(Map.empty[TypeDefBlock, List[TypeDefBlock]]) { (acc, pair) =>
          val (key, value) = pair
          acc.updated(key, acc.getOrElse(key, List()) :+ value)
        }.mapValues(_.distinct)

    implicit def listMergeWithDistinctMonoid[T]: Monoid[List[T]] = new Monoid[List[T]] {
      override def op(a: List[T], b: List[T]): List[T] = (a ++ b).distinct
      override def zero: List[T] = List()
    }

    merge(res1, implementRelation)
  }
}

trait FunctionWriter {
  def show(defBlock: DefBlock): String
}