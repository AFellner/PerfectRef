package structure

import scala.collection.mutable.HashMap
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.collection.mutable.LinkedList

class Query(head: Nary, body: List[Atom]) {
  
  def contains(atom: Atom) = body.contains(atom)
  def replace(thisAtom: Atom, thatAtom: Atom):Query = {
    new Query(head, body.map(i => if (i == thisAtom) thatAtom else i))
    
  }
  override def toString:String = {
    head + " :- " + body.mkString(",")
  }
  def getHead = head
  def getBody = body
  override def equals(other: Any) = other match {
    case other: Query =>  head == other.getHead && body.toSet == other.getBody.toSet
    case _ => false
  }
  
  def toSQL:String = {
    val vars = head.getEntries
    val sourceMap = new HashMap[Var,String]
    var fromMap = List[String]()
    var out = ""
    var whereMap =  List[String]()
    var counter = 0
    body.foreach(atom => {
      fromMap ::= "\"" + atom.getExprName + "\"" + " as " + "T"+counter
      if (atom.isInstanceOf[Unary]) {
        if (atom.asInstanceOf[Unary].getEntry.isInstanceOf[Var])
        {
          val v = atom.asInstanceOf[Unary].getEntry.asInstanceOf[Var]
          if (!sourceMap.contains(v)) {
            sourceMap += (v -> ("T"+counter+".entry1"))
          }
          else
          {
            val source = sourceMap.get(v).get
            whereMap ::= "T" + counter + ".entry1 = "+source
          }
        }
        else
        {
          val c = atom.asInstanceOf[Unary].getEntry.asInstanceOf[Const]
          whereMap ::= "T"+counter +".entry1 = " + "\'" + c + "\'"
        }
      }
      else {
        if (atom.asInstanceOf[Binary].getEntry1.isInstanceOf[Var])
        {
          val v = atom.asInstanceOf[Binary].getEntry1.asInstanceOf[Var]
          if (!sourceMap.contains(v)) {
            sourceMap += (v -> ("T"+counter+".entry1"))
          }
          else
          {
            val source = sourceMap.get(v).get
            whereMap ::= "T"+counter + ".entry1 = "+source
          }
        }
        else
        {
          val c = atom.asInstanceOf[Binary].getEntry1.asInstanceOf[Const]
          whereMap ::= "T"+counter +".entry1 = " + "\'" + c + "\'"
        }
        if (atom.asInstanceOf[Binary].getEntry2.isInstanceOf[Var])
        {
          val v = atom.asInstanceOf[Binary].getEntry2.asInstanceOf[Var]
          if (!sourceMap.contains(v)) {
            sourceMap += (v -> ("T"+counter+".entry2"))
          }
          else
          {
            val source = sourceMap.get(v).get
            whereMap ::= "T"+counter+".entry2 = "+ source
          }
        }
        else
        {
          val c = atom.asInstanceOf[Binary].getEntry2.asInstanceOf[Const]
          whereMap ::= "T"+counter+".entry2 = " + "\'" + c + "\'"
        }
      }
      counter = counter +1
    }
    )
    out += "SELECT "
    var sourceTMP = List[String]()
    vars.foreach(v => {
      val source = sourceMap.get(v).get
      sourceTMP ::= source
    }
    )
    out += sourceTMP.mkString(",")
    out += "\nFROM " + fromMap.mkString(" , ")+"\n"
    out+= "WHERE " + whereMap.mkString(" and ")
    return out
  }
}