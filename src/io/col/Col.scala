//package io.col
//
//import language.experimental.macros
//import scala.reflect.macros.blackbox.Context
//
//trait Col[@specialized T] {
//
//  def +:(v: T): Col[T]
//  def :+(v: T): Col[T]
//  def :::(v: Col[T]): Col[T]
//  def head: T
//  def tail: Col[T]
//  def isEmpty: Boolean
//  def nonEmpty: Boolean
//  def foreach(f: T => Unit): Unit
//  def size: Int
//  def copyTo(array: Array[T], pos: Int): Unit
//}
//
//private[col] class ColMacro(val c: Context) {
//  import c.universe._
//  
//  def apply[T]()(implicit t: WeakTypeTag[T]): Expr[Col[T]] =
//    if(t.tpe =:= c.typeOf[Double])
//      reify { DoubleCol.apply().asInstanceOf[Col[T]] }
//    else if(t.tpe =:= c.typeOf[Long])
//      reify { LongCol.apply().asInstanceOf[Col[T]] }
//    else
//      reify { AnyRefCol.apply().asInstanceOf[Col[T]] }
//}
//
//object Col {
//  def apply[T](): Col[T] = macro ColMacro.apply[T]
//}