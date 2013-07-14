package logic

import structure._
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.collection.JavaConversions._

object Unifier {
  def e_unifiable(e1:Entry, e2:Entry): Boolean = (e1, e2) match {
    case (e1, e2) if (e1.isbound && e2.isbound && !(e1 == e2)) => false
    case _ => true
  }
  
  def unifiable(a1:Atom, a2:Atom):Boolean = (a1, a2) match {
    case (a1:Unary, a2:Binary) => false
    case (a1:Binary, a2:Unary) => false
    case (a1:Unary, a2:Unary) if (a1.getExprName != a2.getExprName) => false
    case (a1:Binary, a2:Binary) if (a1.getExprName != a2.getExprName) => false
    case (a1:Unary, a2:Unary) if 
    	(a1.asInstanceOf[Unary].getEntry.isbound && a2.asInstanceOf[Unary].getEntry.isbound && a1.asInstanceOf[Unary].getEntry != a2.asInstanceOf[Unary].getEntry)
    		=> false
    case (a1:Binary, a2:Binary) if 
    	(a1.all_entries_bound && a2.all_entries_bound && !(a1.getEntry1 == a2.getEntry1 && a1.getEntry2 == a2.getEntry2))
    	    => false
    case (a1:Binary, a2:Binary) if 
    	(e_unifiable(a1.getEntry1, a2.getEntry1) == false || e_unifiable(a1.getEntry2, a2.getEntry2) == false) => false
    case _ => true   
  }
  
  def unify_entries(e1:Entry, e2:Entry):Entry = (e1, e2) match{
    case (e1, e2) if (e1.isbound && e2.isbound && e1 == e2) => e1
    case (e1, e2) if (e1.isbound && !e2.isbound) => e1
    case(e1, e2) if (!e1.isbound && e2.isbound) => e2
    case(e1, e2) if (!e1.isbound && !e2.isbound) => {
      val nv = new Var(e1.getName + "#" + e2.getName)
      nv.unbind
      nv}
    }
  
  def unify_unary(u1:Unary, u2:Unary):Unary = new Unary(u1.getOWLClass, unify_entries(u1.getEntry, u2.getEntry))
  
  def unify_binary(b1:Binary, b2:Binary):Binary = 
    new Binary(b1.getOWLOBjectProperty, unify_entries(b1.getEntry1, b2.getEntry1), unify_entries(b1.getEntry2, b2.getEntry2))
  
  def unify_atoms(a1:Atom, a2:Atom):Atom = (a1, a2) match {
    case (a1:Unary, a2:Unary) => unify_unary(a1, a2)
    case (a1:Binary, a2:Binary) => unify_binary(a1, a2)
  }
  
  def reduce(q:Query, a1:Atom, a2:Atom):Query = {
    val a = unify_atoms(a1, a2)
    val pruned_body1 =  q.getBody.filterNot(_ == a1)
    val pruned_body2 = pruned_body1.filterNot(_ == a2)
    val new_body = a :: pruned_body2
    new Query(q.getHead, new_body)
  }
  
}