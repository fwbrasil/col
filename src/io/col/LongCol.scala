package io.col

import scala.annotation.tailrec
import java.util.Arrays
import java.util.concurrent.atomic.AtomicBoolean
import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import java.util.Arrays
import scala.reflect.api.TreeCreator
import scala.reflect.api.Universe

trait LongCol {

  import LongCol.Impl._

  def +:(v: Long): LongCol = HeadTail(v, this)

  def :+(v: Long): LongCol =
    this match {
      case HeadTail(head, tail) =>
        val tailSize = tail.size
        val n = new Array[Long](tailSize + 2)
        n(0) = head
        tail.copyTo(n, 1)
        n(tailSize + 1) = v
        Compact(n)
      case Compact(array) =>
        val length = array.length + 1
        val n = Arrays.copyOf(array, length)
        n(length - 1) = v
        Compact(n)
    }

  def :::(v: LongCol): LongCol =
    this match {
      case HeadTail(head, tail) =>
        val tailSize = tail.size
        val n = new Array[Long](1 + tailSize + v.size)
        n(0) = head
        tail.copyTo(n, 1)
        v.copyTo(n, tailSize + 1)
        Compact(n)
      case Compact(array) =>
        val n = Arrays.copyOf(array, array.length + v.size)
        v.copyTo(n, array.length)
        Compact(n)
    }
  def head: Long =
    this match {
      case HeadTail(head, tail) => head
      case Compact(array)       => array(0)
    }
  def tail: LongCol =
    this match {
      case Nil                  => ???
      case HeadTail(head, tail) => tail
      case Compact(array) =>
        val length = array.length - 1
        val n = new Array[Long](length)
        System.arraycopy(array, 1, n, 0, length)
        Compact(n)
    }

  def isEmpty: Boolean = this eq Nil
  def nonEmpty: Boolean = !isEmpty

  def map(f: Long => Long): LongCol = {
    @tailrec def loop(array: Array[Long], pos: Int, c: LongCol): Unit =
      c match {
        case HeadTail(head, tail) =>
          array(pos) = f(head)
          loop(array, pos + 1, tail)
        case Compact(a) =>
          var i = 0
          while (i < a.length) {
            array(pos + i) = f(array(i))
            i += 1
          }
      }
    val array = new Array[Long](size)
    loop(array, 0, this)
    Compact(array)
  }

  def foreach(f: Long => Unit): Unit = {
    @tailrec def loop(c: LongCol): Unit =
      c match {
        case HeadTail(head, tail) =>
          f(head)
          loop(tail)
        case Compact(array) =>
          var i = 0
          while (i < array.length) {
            f(array(i))
            i += 1
          }
      }
    loop(this)
  }

  def size: Int

  //  = {
  //    @tailrec def loop(c: LongCol, acc: Int): Int =
  //      c match {
  //        case HeadTail(head, tail) =>
  //          loop(tail, acc + 1)
  //        case Compact(array) =>
  //          acc + array.length
  //      }
  //    loop(this, 0)
  //  }

  private def copyTo(array: Array[Long], pos: Int): Unit = {
    @tailrec def loop(col: LongCol, pos: Int): Unit =
      col match {
        case Nil => Unit
        case HeadTail(head, tail) =>
          array(pos) = head
          loop(tail, pos + 1)
        case Compact(a) =>
          System.arraycopy(a, 0, array, pos, a.length)
      }
    loop(this, pos)
  }
}

object LongCol {

  def apply(): LongCol = Impl.Nil

  private[col] object Impl {
    final case object Nil extends LongCol {
      def size = 0
    }

    final case class HeadTail(_head: Long, _tail: LongCol) extends LongCol {
      val size = _tail.size + 1
    }

    final case class Compact(array: Array[Long]) extends LongCol {

      def size = array.length

      override def equals(other: Any) =
        other match {
          case Compact(a) => Arrays.equals(array, a)
          case _          => false
        }
      override def hashCode = Arrays.hashCode(array)
    }
  }

}

//private[col] class LongColImpl(val c: Context) {
//  import c.universe._
//  import LongCol._
//  def nonEmpty: Expr[Boolean] =
//    reify {
//      c.prefix.splice != LongCol.Nil
//    }
//
//  private def inlineFunction(block: Tree, f: Tree) =
//    (block, f) match {
//      case (q"{ $m; ..$rest }", q"{($p) => $body}") =>
//        c.untypecheck(q"{ def f($p) = $body; ..$rest }")
//    }
//
//  def foreach(f: Expr[Long => Unit]): Tree = {
//    val r =
//      reify {
//        def f(i: Long): Unit = ??? // replaced by inlineFunction
//
//      }
//    inlineFunction(r.tree, f.tree)
//  }
//}
