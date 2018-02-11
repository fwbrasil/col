package io.col

import scala.annotation.tailrec
import java.util.Arrays
import java.util.concurrent.atomic.AtomicBoolean
import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import java.util.Arrays
import scala.reflect.api.TreeCreator
import scala.reflect.api.Universe

trait ShortCol {

  import ShortCol.Ast._

  def +:(v: Short): ShortCol = Prepend(v, this)

  def :+(v: Short): ShortCol = Append(this, v)

  def :::(v: ShortCol): ShortCol = Concat(this, v)

  def size: Int

  def head: Short = {
    @tailrec def loop(c: ShortCol): Short =
      this match {
        case Nil           => throw new NoSuchElementException
        case Prepend(a, b) => a
        case Append(a, b)  => loop(a)
        case Concat(a, b)  => loop(a)
        case Compact(a)    => a(0)
      }
    loop(this)
  }

  //  def tail: ShortCol = {
  //    @tailrec def loop(c: ShortCol): ShortCol =
  //      this match {
  //        case Nil           => Nil
  //        case Prepend(a, b) => b
  //        case Append(a, b)  => loop(a)
  //        case Concat(a, b)  => loop(a)
  //        case Compact(a)    => a(0)
  //      }
  //    loop(this)
  //  }
  //  def tail: ShortCol =
  //    this match {
  //      case ShortCol.Nil        => ???
  //      case Prepend(head, tail) => tail
  //      case ArrayBased(array) =>
  //        val length = array.length - 1
  //        val n = new Array[Short](length)
  //        System.arraycopy(array, 1, n, 0, length)
  //        ArrayBased(n)
  //    }
  //
  //  def isEmpty: Boolean = this eq ShortCol.Nil
  //  def nonEmpty: Boolean = !isEmpty
  //
  //  def foreach(f: Short => Unit): Unit = {
  //    @tailrec def loop(c: ShortCol): Unit =
  //      c match {
  //        case Prepend(head, tail) =>
  //          f(head)
  //          loop(tail)
  //        case ArrayBased(array) =>
  //          var i = 0
  //          while (i < array.length) {
  //            f(array(i))
  //            i += 1
  //          }
  //      }
  //    loop(this)
  //  }
  //
  //  def size: Int = {
  //    @tailrec def loop(c: ShortCol, acc: Int): Int =
  //      c match {
  //        case Prepend(head, tail) =>
  //          loop(tail, acc + 1)
  //        case ArrayBased(array) =>
  //          acc + array.length
  //      }
  //    loop(this, 0)
  //  }
  //
  def copyTo(array: Array[Short], pos: Int): Unit = {
    @tailrec def loop(col: ShortCol, pos: Int): Unit =
      col match {
        case Nil => Unit
        case Append(a, b) =>
          array(pos + a.size) = b
          loop(a, pos)
        case Prepend(head, tail) =>
          array(pos) = head
          loop(tail, pos + 1)
        case Concat(a, b) =>
          // TODO stack safety
          a.copyTo(array, pos)
          b.copyTo(array, pos + a.size)
        case Compact(a) =>
          System.arraycopy(a, 0, array, pos, a.length)
      }
    loop(this, pos)
  }

  private def compact: Compact = {
    @tailrec def loop(array: Array[Short], col: ShortCol, pos: Int): Compact =
      col match {
        case Nil => Compact(array)
        case Append(a, b) =>
          array(pos + a.size) = b
          loop(array, a, pos)
        case Prepend(head, tail) =>
          array(pos) = head
          loop(array, tail, pos + 1)
        case Concat(a, b) =>
          // TODO stack safety
          ???
        case Compact(a) =>
          System.arraycopy(a, 0, array, pos, a.length)
          Compact(array)
      }
    loop(new Array[Short](size), this, 0)
  }

}

object ShortCol {

  private[col] object Ast {

    case object Nil extends ShortCol {
      def size = 0
    }
    case class Append(a: ShortCol, b: Short) extends ShortCol {
      val size = a.size + 1
    }
    case class Prepend(a: Short, b: ShortCol) extends ShortCol {
      val size = 1 + b.size
    }
    case class Concat(a: ShortCol, b: ShortCol) extends ShortCol {
      val size = a.size + b.size
    }

    case class Compact(array: Array[Short]) extends ShortCol {
      def size = array.length

      override def equals(other: Any) =
        other match {
          case Compact(a) => Arrays.equals(array, a)
          case _          => false
        }
      override def hashCode = Arrays.hashCode(array)
    }
  }

  def apply(): ShortCol = Ast.Nil

}
