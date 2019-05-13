package co.gyeongmin.scala.godoc.godoc

import co.gyeongmin.scala.godoc.functional.Monoid
import co.gyeongmin.scala.godoc.godoc.doc.elements.Argument

import scala.annotation.tailrec

trait BuildGroupWriter {
  def show(fd: BuilderGroupElem): String
}

sealed trait BuilderGroupElem {
  def typeName: String

  def desc: Option[String]

  def ex: Option[String]

  def show(implicit functionWriter: BuildGroupWriter): String = functionWriter.show(this)
}

case class Constructor(typeName: String, name: String, argList: List[Argument], desc: Option[String], ex: Option[String]) extends BuilderGroupElem

case class Method(typeName: String, method: String, argList: List[Argument], desc: Option[String], ex: Option[String]) extends BuilderGroupElem

case class Setter(typeName: String, memberName: String, memberType: String, desc: Option[String], ex: Option[String]) extends BuilderGroupElem

sealed trait DefBlock {
  def name: String
}

trait FDefBlock {
  def show: String
}

case class TypeDefBlock(name: String, interface: Boolean, implements: List[String], desc: Option[String], ex: Option[String], builder: Boolean) extends DefBlock

case class FuncDefBlock(name: String, argList: List[Argument], returnType: String, desc: Option[String], ex: Option[String]) extends DefBlock

case class ReceiverFuncDefBlock(recvName: String, recvType: String, func: FuncDefBlock) extends DefBlock {
  def name: String = func.name
}


object DocType {
  // 적당하게 Constructor, Method, Setter 타입으로 리턴
  def create(defBlocks: Seq[DefBlock]): DocType = {
    NoneType
  }
}

trait DocType

case class BuilderGroup(typeName: String, constructor: List[Constructor], setters: Seq[Setter], methods: Seq[Method]) extends DocType

case class GeneralGroup(methods: Seq[Method]) extends DocType

case object NoneType extends DocType

object BuilderGroup {
  import co.gyeongmin.scala.godoc.functional.MonoidSyntax._

  def constructBuilderGroups(ret: Seq[(String, Seq[DefBlock])]): List[BuilderGroup] = {
    val graph = constructGraph(ret)
    val orders = orderTypesBFS(graph)
    val allDefs = ret.flatMap(_._2).toList
    asBuilderGroups(orders, allDefs)
  }


  def asBuilderGroups(elems: List[TypeDefBlock], allDefs: List[DefBlock]): List[BuilderGroup] = {
    @tailrec
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

  def orderTypes(graph: Map[TypeDefBlock, List[TypeDefBlock]]): List[TypeDefBlock] = {
    @tailrec
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

  def calculateDegrees(typeMap: Map[TypeDefBlock, List[TypeDefBlock]]): Map[TypeDefBlock, Int] = {
    typeMap.keys.foldLeft(typeMap.mapValues(_ => 0)) { (acc, tb) =>
      Monoid.mergeMap(acc, typeMap(tb).foldLeft(Map.empty[TypeDefBlock, Int].withDefault(_ => 0)) { (a, t) =>
        a.updated(t, a(t) + 1)
      })
    }
  }

  def orderTypesBFS(graph: Map[TypeDefBlock, List[TypeDefBlock]]): List[TypeDefBlock] = {
    @tailrec
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

  def constructGraph(ret: Seq[(String, Seq[DefBlock])]): Map[TypeDefBlock, List[TypeDefBlock]] = {
    val builderTypes = ret.flatMap(_._2).flatMap {
      case x@TypeDefBlock(_, _, _, _, _, true) => Some(x)
      case _ => None
    }.toList

    val typeMap = builderTypes.map(x => x.name -> x).toMap

    @tailrec
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

    val dependTypes = builderTypes.filter(x => x.implements.nonEmpty)

    val implementRelation: Map[TypeDefBlock, List[TypeDefBlock]] =
      dependTypes.flatMap(x => x.implements.map(t => typeMap(t) -> x))
        .foldLeft(Map.empty[TypeDefBlock, List[TypeDefBlock]]) { (acc, pair) =>
          val (key, value) = pair
          acc.updated(key, acc.getOrElse(key, List()) :+ value)
        }.mapValues(_.distinct)

    Monoid.mergeMap(res1, implementRelation)
  }
}