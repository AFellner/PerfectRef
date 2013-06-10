import structure._
import logic._
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.collection.JavaConversions._
object Test {

  def main(args: Array[String]) = {
    val ontology = loadOntology("PerfTest-functional.owl")
    val pIs = PerfReformulator.readPIs(ontology)
    //pIs.foreach(println)
    val P1 = ontology.getObjectPropertiesInSignature().toList.get(0)
    val P = ontology.getObjectPropertiesInSignature().toList.get(1)
    val A = ontology.getClassesInSignature().toList.get(1)
    val A1 = ontology.getClassesInSignature().toList.get(2)
    //println("P " + P)
    //println("P1 " + P1)
    //println("A " + A)
    //println("A1 " + A1)
    val x = new Var("x")
    val anon = new Var("anon")
    x.bind
    val Ax = new Unary(A,x)
    val Px1 = new Binary(P,x,anon)
    val Px2 = new Binary(P,anon,x)
    //println(Ax)
    //println(Px1)
    //println(Px2)
    pIs.foreach(pI =>
      if (Ax.applicable(pI)) println(Ax.apply(pI))
      else if (Px1.applicable(pI)) println(Px1.apply(pI))
      else if (Px2.applicable(pI)) println(Px2.apply(pI))
    )
  }
  
   def loadOntology(filename: String): OWLOntology = {
   val manager = OWLManager.createOWLOntologyManager()
   val file = new java.io.File(filename)
   manager.loadOntologyFromOntologyDocument(file)
  }
}