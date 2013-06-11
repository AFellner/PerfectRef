import structure._
import logic._
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.collection.JavaConversions._

object Test {

  def main(args: Array[String]):Unit = {
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
//    val x = new Var("x")
//    val anon = new Var("anon")
//    x.bind
//    val Ax = new Unary(A,x)
//    val Px1 = new Binary(P,x,anon)
//    val Px2 = new Binary(P,anon,x)
//    //println(Ax)
//    //println(Px1)
//    //println(Px2)
//    pIs.foreach(pI =>
//      if (Ax.applicable(pI)) println(Ax.apply(pI))
//      else if (Px1.applicable(pI)) println(Px1.apply(pI))
//      else if (Px2.applicable(pI)) println(Px2.apply(pI))
//    )
    
    val c = new Const("c")
    val d = new Const("d")
    val x1 = new Var("x1")
    x1.bind
    val x2 = new Var("x2")
    x2.bind
    val y1 = new Var("y1")
    y1.bind
    val head = new Nary("q",List(x1,x2))
    val ax1 = new Unary(A,x1)
    val ax2 = new Unary(A,x2)
    val px2y1 = new Binary(P,x2,y1)
    val pcd = new Binary(P,c,d)
    val testquery = new Query(head,List(ax1,px2y1,ax2,pcd))
    val sql = testquery.toSQL
    println(sql)
  }
  
   def loadOntology(filename: String): OWLOntology = {
   val manager = OWLManager.createOWLOntologyManager()
   val file = new java.io.File(filename)
   manager.loadOntologyFromOntologyDocument(file)
  }
}