package structure

import scala.collection.mutable._
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.collection.mutable.LinkedList

class Query(head: Nary, body: List[Atom]) {
  // creates the binding pattern for the variables in the query
  def give_entries(a:Atom):List[Entry] = {
    if (a.isInstanceOf[Unary]) List(a.asInstanceOf[Unary].getEntry)
    else List(a.asInstanceOf[Binary].getEntry1, a.asInstanceOf[Binary].getEntry2)
  }
  
  var body_variables = body flatMap (give_entries(_))  
  for (body_variable <- body_variables) {
    if (body_variable.isInstanceOf[Var] && body_variables.count(_ == body_variable) == 1 && head.contains_variable(body_variable.asInstanceOf[Var]) == false) 
      body_variable.asInstanceOf[Var].unbind 
  }
  
  def contains(atom: Atom) = body.contains(atom)
  
  def replace(thisAtom: Atom, thatAtom: Atom):Query = {
    new Query(head, body.map(i => if (i == thisAtom) thatAtom else i))
    
  }
  override def toString:String = {
    head + " :- " + body.mkString(",")
  }
  def getHead = head
  def getBody = body
  def ==(other: Query):Boolean = {
    val this_body = this.getBody map (_.toString())
    val other_body = other.getBody map (_.toString())
    this.head == other.getHead && this_body.toSet == other_body.toSet
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
      val source = sourceMap.getOrElse(v, "")
      sourceTMP ::= source
    }
    )
    out += sourceTMP.mkString(",")
    out += "\nFROM " + fromMap.mkString(" , ")+"\n"
    if (!whereMap.isEmpty) out+= "WHERE " + whereMap.mkString(" and ")
    return out
  }
}

