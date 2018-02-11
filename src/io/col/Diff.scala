package io.col

import io.col.Test.Nest

object Diff extends App {
  
  println(diff("", new Test("a", 1, new Nest(false)), new Test("a", 1, new Nest(true))))
  
  def diff[T](p: String, a: T, b: T): List[String] = {
    require(a.getClass == b.getClass)
    
    val fields = a.getClass.getFields.toList
    
    fields.flatMap { f =>
      val va = f.get(a)
      val vb = f.get(b)
      if(va != vb) {
        val str = s"$p.${f.getName}: $va (hc ${va.hashCode}), $vb (hc ${vb.hashCode})"
        str +: diff(s"$p.${f.getName}", va, vb), 
      } else
        Nil
    }
  }
}