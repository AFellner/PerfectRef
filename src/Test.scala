import structure._
import logic._
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.collection.JavaConversions._
object Test {

  def main(args: Array[String]) = {
    val ontology = loadOntology("mytest.owl")
    val property = ontology.getObjectPropertiesInSignature().toList.get(0)
    val oneclass = ontology.getClassesInSignature().toList.get(0)
    val lala = oneclass.getReferencingAxioms(ontology).toList
    lala.foreach(println)
    var x = new Var("x")
    x.bind
    var y = new Var("y")
    val a = new Const("a")
    val b = new Const("b")
    val head = new Nary("q",List(x))
    val atom1 = new Unary(oneclass,x)
    val atom2 = new Binary(property,y,a)
    val atom3 = new Unary(oneclass,x)
    val body = List(atom1,atom2)
    val q = new Query(head,body)
    val body2 = List(atom2,atom1)
    val q2 = new Query(head,body)
    println(q)
    val q1 = q.replace(atom1,atom3)
    println(q1)
    val ont = PerfReformulator.loadOntology("pizza.owl")
    PerfReformulator.insertAboxAssertions(ont)
    println(q == q2)
    val testlist = Set(q)
    //testlist.foreach(println)
    val testlist2 = Set(q,q2)
    testlist2.foreach(println)
  }
  
   def loadOntology(filename: String): OWLOntology = {
   val manager = OWLManager.createOWLOntologyManager()
   val file = new java.io.File(filename)
   manager.loadOntologyFromOntologyDocument(file)
  }
}