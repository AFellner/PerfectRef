package structure

import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import logic.PerfReformulator
import scala.collection.JavaConversions._

trait Atom {
  val arity = 0
  def applicable(pI: OWLAxiom):Boolean
  def apply(pI: OWLAxiom):Atom
  def getExprName:String
  def ==(other:Atom):Boolean = other match{
    case other:Unary if (this.isInstanceOf[Unary]) => this.asInstanceOf[Unary] == other.asInstanceOf[Unary]
    case other:Binary if (this.isInstanceOf[Binary]) => this.asInstanceOf[Binary] == other.asInstanceOf[Binary]
    case _ => false    
  }
}

class Unary(ofclass: OWLClass, e: Entry) extends Atom {
  override val arity = 1
  override def toString() = ofclass.toStringID.split("#")(1) + "("+e+")"
  def getOWLClass:OWLClass = ofclass
  def getEntry:Entry = e
  def ==(other:Unary):Boolean = (ofclass == other.getOWLClass && e == other.getEntry)
  def applicable(pI: OWLAxiom):Boolean = {
    if (pI.isInstanceOf[OWLSubClassOfAxiom]) {
      return pI.asInstanceOf[OWLSubClassOfAxiom].getSuperClass == ofclass
    }
    else
      return false
  }
  def getExprName:String = ofclass.toStringID
  
  //applicable has to be checked before calling this method
  //todo: deal with inverses properly!
  def apply(pI: OWLAxiom):Atom = {
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
  override val arity = 2
  override def toString() = ofproperty.toStringID.split("#")(1) + "("+e1+","+e2+")"
  def getOWLOBjectProperty:OWLObjectProperty = ofproperty
  def getEntry1 = e1
  def getEntry2 = e2
  def ==(other:Binary):Boolean = (ofproperty == other.getOWLOBjectProperty) && (e1 == other.getEntry1) && (e2 == other.getEntry2)
  def getExprName:String = ofproperty.toStringID
  
  def all_entries_bound:Boolean = e1.isbound && e2.isbound
  
  def applicable(pI: OWLAxiom):Boolean = {
    
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
  def apply(pI: OWLAxiom):Atom = {
    
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
  
  // method to compare two lists of variables; the regular '==' on lists doesn't seem to work
  def compare(e1:List[Var], e2:List[Var]):Boolean = (e1, e2) match {
    case (e1, e2) if (e1.length != e2.length) => false
    case (List(), List()) => true
    case (e1, e2)  if (e1.head == e2.head) => compare(e1.tail, e2.tail)
    case _ => false    
  }    
  def ==(other: Nary) = other.getName == name && compare(e, other.getEntries)
  def contains_variable(v:Var) = e.count(_ == v) > 0 
}