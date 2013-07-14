package logic

import scala.collection.JavaConversions._
import org.h2.jdbcx.JdbcConnectionPool
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._

import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import scala.collection.mutable.ListBuffer


object PerfReformulator{

  var varCount:Int = 0
  
  def incVC() = varCount = varCount +1
  
  def getVC = varCount
  
  def loadOntology(filename: String): OWLOntology = {
   val manager = OWLManager.createOWLOntologyManager()
   val file = new java.io.File(filename)
   manager.loadOntologyFromOntologyDocument(file)
  }
  
  def createDatabase(ontology: OWLOntology) = {
    val classes = collectionAsScalaIterable(ontology.getClassesInSignature(true))
    val properties = collectionAsScalaIterable(ontology.getObjectPropertiesInSignature(true))
    Database.forURL("jdbc:h2:~/ontology", driver = "org.h2.Driver") withSession {
      classes.foreach(c => {
        val query = "create table if not exists \""+c.toStringID+"\" (entry1 varchar)"
        //println(query)
        Q.updateNA(query).execute
      })
        
      properties.foreach(p => {
        val query = "create table if not exists \""+p.toStringID+"\" (entry1 varchar, entry2 varchar)"
        //println(query)
        Q.updateNA(query).execute
      })
    }
  }
  
  def insertAboxAssertions(ontology: OWLOntology) = {
    val aboxAxioms = collectionAsScalaIterable(ontology.getABoxAxioms(true))
    Database.forURL("jdbc:h2:~/ontology", driver = "org.h2.Driver") withSession {
    aboxAxioms.foreach(a => 
     if (a.isInstanceOf[OWLClassAssertionAxiom])
     {
       val c = a.asInstanceOf[OWLClassAssertionAxiom].getClassExpression
       if (c.isInstanceOf[OWLClass]) {
	       val i = a.asInstanceOf[OWLClassAssertionAxiom].getIndividual
	       val query = "insert into \"" + c.asInstanceOf[OWLClass].toStringID + "\" values (\'"+i.toStringID+"\')"
	       Q.updateNA(query).execute
	       //println(query)
     	}
     }
     else if (a.isInstanceOf[OWLObjectPropertyAssertionAxiom])
     {
         val s = a.asInstanceOf[OWLObjectPropertyAssertionAxiom].getSubject
         val o = a.asInstanceOf[OWLObjectPropertyAssertionAxiom].getObject
         val p = a.asInstanceOf[OWLObjectPropertyAssertionAxiom].getProperty.getNamedProperty()
         val query = "insert into \"" + p.toStringID + "\" values (\'"+s.toStringID + "\', \'" + o.toStringID +"\')"
         Q.updateNA(query).execute
         //println(query)
     }
    
    )  
    }
  }
  
  def readPIs(ontology: OWLOntology): Iterable[OWLAxiom] = {
    val axioms = collectionAsScalaIterable(ontology.getAxioms)
    axioms.filter(s => 
     if (s.isInstanceOf[OWLSubClassOfAxiom]) {
       val left = s.asInstanceOf[OWLSubClassOfAxiom].getSubClass
       val right = s.asInstanceOf[OWLSubClassOfAxiom].getSuperClass
       if (right.isInstanceOf[OWLObjectIntersectionOf]) false
       else if (left.isInstanceOf[OWLDataSomeValuesFrom]) false
       else if (right.isInstanceOf[OWLObjectComplementOf]) false
       else if (right.isInstanceOf[OWLDataSomeValuesFrom]) false
       else true
     }
     else if (s.isInstanceOf[OWLSubObjectPropertyOfAxiom])
       true
     else
         false
   )
  }
}