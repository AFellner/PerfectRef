package structure

import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import logic.PerfReformulator
import scala.collection.JavaConversions._

trait Atom {
  def applicable(pI: OWLAxiom, ontology: OWLOntology):Boolean
  def apply(pI: OWLAxiom, ontology: OWLOntology):Atom
}

class Unary(ofclass: OWLClass, e: Entry) extends Atom {
  override def toString() = ofclass.toString + "("+e+")"
  def getOWLClass:OWLClass = ofclass
  def getEntry = e
  def ==(other:Unary) = ofclass == other.getOWLClass && e == other.getEntry
  def applicable(pI: OWLAxiom,ontology: OWLOntology):Boolean = {
    if (pI.isInstanceOf[OWLSubClassOfAxiom]) {
      return pI.asInstanceOf[OWLSubClassOfAxiom].getSuperClass == ofclass
    }
    else
      return false
  }
  
  //applicable has to be checked before calling this method
  //todo: deal with inverses properly!
  def apply(pI: OWLAxiom, ontology: OWLOntology):Atom = {
    if (pI.isInstanceOf[OWLSubClassOfAxiom]) {
      val left = pI.asInstanceOf[OWLSubClassOfAxiom].getSubClass
      if (left.isInstanceOf[OWLClass]) return new Unary(left.asInstanceOf[OWLClass],e)
      else if (left.isInstanceOf[OWLObjectSomeValuesFrom]) {
        PerfReformulator.incVC
        val newVar = new Var("x_"+PerfReformulator.getVC)
        val leftPropertyExpr = left.asInstanceOf[OWLObjectSomeValuesFrom].getProperty
        val prop = leftPropertyExpr.getNamedProperty()
        return if (leftPropertyExpr.isInstanceOf[OWLObjectInverseOf]) new Binary(prop,newVar,e) else new Binary(prop,e,newVar)
      }
      else return this
    }
    else return this
  }
}

class Binary(ofproperty: OWLObjectProperty, e1: Entry, e2: Entry) extends Atom {
  override def toString() = ofproperty.toString + "("+e1+","+e2+")"
  def getOWLOBjectProperty:OWLObjectProperty = ofproperty
  def getEntry1 = e1
  def getEntry2 = e2
  def ==(other:Binary) = (ofproperty == other.getOWLOBjectProperty) && (e1 == other.getEntry1) && (e2 == other.getEntry2)
  def applicable(pI: OWLAxiom, ontology: OWLOntology):Boolean = {
    
    //(3)I is a role inclusion assertion and its right-hand side is either P or P-
    if (pI.isInstanceOf[OWLSubObjectPropertyOfAxiom])
    {
      val propInclusionAxiom = pI.asInstanceOf[OWLSubObjectPropertyOfAxiom]
      val superPropertyExpr = propInclusionAxiom.getSuperProperty
      if (superPropertyExpr.getNamedProperty() == ofproperty) return true
      else return false
    }
    //(1) x2=_and the right-hand side of I is exist P
    //(2) x1=_and the right-hand side of I is exists P-
    else if (pI.isInstanceOf[OWLSubClassOfAxiom])
    {
      val right = pI.asInstanceOf[OWLSubClassOfAxiom].getSuperClass
      if (right.isInstanceOf[OWLObjectSomeValuesFrom]) {
        val propertyExpr = right.asInstanceOf[OWLObjectSomeValuesFrom].getProperty
        if (propertyExpr.getNamedProperty() == ofproperty)
          if (propertyExpr.isInstanceOf[OWLObjectInverseOf]) return e1.isbound
          else return e2.isbound
        else return false
      }
    }
    return false
  }
  
  //check applicable before applying this!
  def apply(pI: OWLAxiom, ontology: OWLOntology):Atom = {
    
    //if g = P(x1,x2) and either I = P1 < P or I = P1- < P- then gr(g,I) = P1(x1,x2)
    //if g = P(x1,x2) and either I = P1 < P- or P- < P then gr(g,I) = P1(x2,x1)
    if (pI.isInstanceOf[OWLSubObjectPropertyOfAxiom])
    {
      val propInclusionAxiom = pI.asInstanceOf[OWLSubObjectPropertyOfAxiom]
      val rightPropertyExpr = propInclusionAxiom.getSuperProperty
      val leftPropertyExpr = propInclusionAxiom.getSubProperty
      return {
      if (rightPropertyExpr.isInstanceOf[OWLObjectInverseOf] == leftPropertyExpr.isInstanceOf[OWLObjectInverseOf]) 
        new Binary(leftPropertyExpr.getNamedProperty(),e1,e2)
      else 
        new Binary(leftPropertyExpr.getNamedProperty(),e2,e1)
      }
    }

    else if (pI.isInstanceOf[OWLSubClassOfAxiom])
    {
      val left = pI.asInstanceOf[OWLSubClassOfAxiom].getSubClass()
      val right = pI.asInstanceOf[OWLSubClassOfAxiom].getSuperClass()
      
      //If g=P(x,_) and I= A < exist P  then gr(g,I)=A(x)
      //If g=P(_,x) and I= A < exist P- then gr(g,I)=A(x)
      if (left.isInstanceOf[OWLClass]) {
        val rightPropertyExpr = right.asInstanceOf[OWLObjectSomeValuesFrom].getProperty()
        val entry = if (rightPropertyExpr.isInstanceOf[OWLObjectInverseOf]) e2 else e1
        return new Unary(left.asInstanceOf[OWLClass],entry)
      }
      
      if (right.isInstanceOf[OWLObjectSomeValuesFrom] && left.isInstanceOf[OWLObjectSomeValuesFrom]) {
        val rightPropertyExpr = right.asInstanceOf[OWLObjectSomeValuesFrom].getProperty()
        val leftPropertyExpr = left.asInstanceOf[OWLObjectSomeValuesFrom].getProperty()
        if (rightPropertyExpr.isInstanceOf[OWLObjectInverseOf] == leftPropertyExpr.isInstanceOf[OWLObjectInverseOf]) 
          new Binary(leftPropertyExpr.getNamedProperty(),e1,e2)
        else 
          new Binary(leftPropertyExpr.getNamedProperty(),e2,e1)
      }
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