package logic

import structure._
import scala.collection.JavaConversions._
import org.h2.jdbcx.JdbcConnectionPool
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.slick.driver.H2Driver.simple._
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
    val classes = ontology.getClassesInSignature().toList
    val properties = ontology.getObjectPropertiesInSignature().toList
    val dataproperties = ontology.getDataPropertiesInSignature().toList
    Database.forURL("jdbc:h2:~/ontology", driver = "org.h2.Driver") withSession {
      classes.foreach(c => 
        Q.updateNA("create table if not exists "+c+" (entry varchar)"))
      properties.foreach(p =>
        Q.updateNA("create table if not exists "+p+" (entry1 varchar, entry2 varchar)"))
      dataproperties.foreach(dp =>
        Q.updateNA("create table if not exists "+dp+" (entry1 varchar, entry2 varchar)"))
    }
  }
  
  def insertAboxAssertions(ontology: OWLOntology) = {
    val aboxAxioms = ontology.getABoxAxioms(true).toList
    Database.forURL("jdbc:h2:~/ontology", driver = "org.h2.Driver") withSession {
    aboxAxioms.foreach(s => s.getAxiomType().getName() match {
      case "ClassAssertion" => Q.updateNA("insert into " + s.getClassesInSignature().toList.get(0) + " values ("+s.getIndividualsInSignature().toList.get(0)+")")
      case "ObjectPropertyAssertion" => Q.updateNA("insert into " + s.getObjectPropertiesInSignature().toList.get(0) + " values ("+s.getIndividualsInSignature().toList.mkString(",")+")")
      //case "DataPropertyAssertion" => Q.updateNA("insert into " + s.getDataPropertiesInSignature().toList.get(0) + " values ("+s.getIndividualsInSignature().toList.mkString(",")+")")
      //case "DifferentIndividuals" => println("these are different: " +s.getIndividualsInSignature().toList.mkString(","))
      case _ => println(s.getAxiomType().getName() + " not covered")
    })
    }
  }
  

  def goodSide(side: OWLObject):Boolean = {
    if (side.isInstanceOf[OWLClass]) return true
    else if(side.isInstanceOf[OWLObjectSomeValuesFrom]) return pureExist(side.asInstanceOf[OWLObjectSomeValuesFrom])
    else if(side.isInstanceOf[OWLObjectProperty]) return true
    else false
  }
  
  def pureExist(exists: OWLObjectSomeValuesFrom):Boolean = {
    if (exists.getClassesInSignature().isEmpty()) return true
    else if ((exists.getClassesInSignature().toList.length == 1) && (exists.getClassesInSignature().toList.get(0).isTopEntity())) return true
    else return false
  }
  
  def isPI(axiom: OWLSubClassOfAxiom):Boolean = {
    return goodSide(axiom.getSubClass()) && goodSide(axiom.getSuperClass())
  }
  
  def readPIs(ontology: OWLOntology): List[(OWLObject,OWLObject)] = {
    val axioms = ontology.getAxioms().toList
    var outList = new ListBuffer[(OWLObject,OWLObject)]
    var addAxiom = false
    axioms.foreach(a => 
      if (isPI(a.asInstanceOf[OWLSubClassOfAxiom]))
              outList.append((a.asInstanceOf[OWLSubClassOfAxiom].getSubClass,a.asInstanceOf[OWLSubClassOfAxiom].getSuperClass))
    )
    /*axioms.foreach(a => a.getAxiomType().getName() match {
      case "SubClassOf" => {
       a.toString().contains("Objj)
        outList.append((a.getClassesInSignature().toList.get(0),a.getClassesInSignature().toList.get(1),0))
      }
      case "EquivalentClasses" => {
        val classes = a.getClassesInSignature().toList
        outList.append((classes.get(0),classes.get(1),0))
        outList.append((classes.get(1),classes.get(0),0))
      }
      case "InverseObjectProperties" => {
        val properties = a.getObjectPropertiesInSignature().toList
        outList.append((properties.get(0),properties.get(1),2))
        outList.append((properties.get(0),properties.get(1),3))
        outList.append((properties.get(1),properties.get(0),3))
        outList.append((properties.get(1),properties.get(0),3))
      }
      case "SubObjectPropertyOf" => {
        val properties = a.getObjectPropertiesInSignature().toList
        outList.append((properties.get(0),properties.get(1),1))
      }
      case "EquivalentObjectProperties" => {
        val properties = a.getObjectPropertiesInSignature().toList
        outList.append((properties.get(0),properties.get(1),1))
        outList.append((properties.get(1),properties.get(0),1))
      }
      case "ObjectPropertyDomain" => {
        val properties = a.getObjectPropertiesInSignature().toList
        val classes = a.getClassesInSignature().toList
        outList.append((properties.get(0),classes.get(0),4))
      }
      case "ObjectPropertyRange" => {
        val properties = a.getObjectPropertiesInSignature().toList
        val classes = a.getClassesInSignature().toList
        outList.append((properties.get(0),classes.get(0),5))
      }
    }
    )*/
    outList.toList
  }
}