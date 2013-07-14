package logic

import structure._
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.collection.JavaConversions._

object OntologyLoader {
  
  def loadOntology(filename: String): OWLOntology = {
   val manager = OWLManager.createOWLOntologyManager()
   val file = new java.io.File(filename)
   manager.loadOntologyFromOntologyDocument(file)
  }

}