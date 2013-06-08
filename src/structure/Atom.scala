package structure

import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import logic.PerfReformulator
import scala.collection.JavaConversions._

trait Atom {
  def applicable(pI: (OWLObject,OWLObject), ontology: OWLOntology):Boolean
  def apply(pI: (OWLObject,OWLObject), ontology: OWLOntology):Atom
}

class Unary(ofclass: OWLClass, e: Entry) extends Atom {
  override def toString() = ofclass.toString + "("+e+")"
  def getOWLClass:OWLClass = ofclass
  def getEntry = e
  def ==(other:Unary) = ofclass == other.getOWLClass && e == other.getEntry
  def applicable(pI: (OWLObject,OWLObject),ontology: OWLOntology):Boolean = {
    return (pI._2 == ofclass)
  }
  
  //applicable has to be checked before calling this method
  //todo: deal with inverses properly!
  def apply(pI: (OWLObject,OWLObject), ontology: OWLOntology):Atom = {
    if (pI._1.isInstanceOf[OWLClass]) return new Unary(pI._1.asInstanceOf[OWLClass],e)
    else if (pI._1.isInstanceOf[OWLObjectSomeValuesFrom]) {
      PerfReformulator.incVC
      val newVar = new Var("x_"+PerfReformulator.getVC)
      return new Binary(pI._1.asInstanceOf[OWLObjectSomeValuesFrom].getObjectPropertiesInSignature.toList.get(0),e,newVar)
    }
    return this
  }
}

class Binary(ofproperty: OWLObjectProperty, e1: Entry, e2: Entry) extends Atom {
  override def toString() = ofproperty.toString + "("+e1+","+e2+")"
  def getOWLOBjectProperty:OWLObjectProperty = ofproperty
  def getEntry1 = e1
  def getEntry2 = e2
  def ==(other:Binary) = (ofproperty == other.getOWLOBjectProperty) && (e1 == other.getEntry1) && (e2 == other.getEntry2)
  def applicable(pI: (OWLObject,OWLObject), ontology: OWLOntology):Boolean = {
    
    //(3)I is a role inclusion assertion and its right-hand side is either P or P-
    if (pI._2.isInstanceOf[OWLObjectProperty])
    {
      if (pI._2 == ofproperty) return true
      if (pI._2.asInstanceOf[OWLObjectProperty].getInverses(ontology).contains(ofproperty)) return true
    }
    //(1) and (2)
    if (pI._2.isInstanceOf[OWLObjectSomeValuesFrom])
    {
      //(1) x2=_and the right-hand side of I is exist P
      if (pI._2.asInstanceOf[OWLObjectSomeValuesFrom] == ofproperty && (!e2.isbound)) return true
      
      //(2)x1=_and the right-hand side of I is exists P-
      if (pI._2.asInstanceOf[OWLObjectSomeValuesFrom].getProperty().getInverses(ontology).contains(ofproperty) && (!e1.isbound)) return true
    }
    return false
  }
  
  def apply(pI: (OWLObject,OWLObject), ontology: OWLOntology):Atom = {
    if (pI._1.isInstanceOf[OWLClass]) {
      return new Unary(pI._1.asInstanceOf[OWLClass],e1)
    }
    else if (pI._2.asInstanceOf[OWLObjectSomeValuesFrom] == ofproperty && (!e2.isbound)) {
      return new Binary(pI._1.getObjectPropertiesInSignature().toList.get(0),e1,e2)
    }
    else if (pI._2.asInstanceOf[OWLObjectSomeValuesFrom].getProperty().getInverses(ontology).contains(ofproperty) && (!e1.isbound)) {
      return new Binary(pI._1.getObjectPropertiesInSignature().toList.get(0),e2,e1)
    }
    else if (pI._2.isInstanceOf[OWLObjectProperty]) {
      if (pI._2 == ofproperty) return new Binary(pI._1.getObjectPropertiesInSignature().toList.get(0),e1,e2)
      //since it is applicable it has to be the inverse case
      else return new Binary(pI._1.getObjectPropertiesInSignature().toList.get(0),e2,e1)
    }
    return this
  }
}

class Nary(name:String, e: List[Var]){
  override def toString() = name + "(" + e.mkString(",") + ")"
  def getName = name
  def getEntries = e
  def ==(other: Nary) = other.getName == name && other.getEntries == e
}