package structure

import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._

trait Atom

class Unary(ofclass: OWLClass, e: Entry) extends Atom {
  override def toString() = ofclass.toString + "("+e+")"
  def getOWLClass:OWLClass = ofclass
  def getEntry = e
  def ==(other:Unary) = ofclass == other.getOWLClass && e == other.getEntry
}

class Binary(ofproperty: OWLObjectProperty, e1: Entry, e2: Entry) extends Atom {
  override def toString() = ofproperty.toString + "("+e1+","+e2+")"
  def getOWLOBjectProperty:OWLObjectProperty = ofproperty
  def getEntry1 = e1
  def getEntry2 = e2
  def ==(other:Binary) = (ofproperty == other.getOWLOBjectProperty) && (e1 == other.getEntry1) && (e2 == other.getEntry2)
}

/*class Binary[A <: OWLProperty[OWLObject,OWLObject]](ofproperty: A, e1: Entry, e2: Entry) extends Atom {
  override def toString() = ofproperty.toString + "("+e1+","+e2+")"
}*/

class Nary(name:String, e: List[Var]) extends Atom {
  override def toString() = name + "(" + e.mkString(",") + ")"
  def getName = name
  def getEntries = e
  def ==(other: Nary) = other.getName == name && other.getEntries == e
}