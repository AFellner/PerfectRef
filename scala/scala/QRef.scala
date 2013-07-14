package scala;

import logic._
import structure._
import scala.collection.JavaConversions._
import org.h2.jdbcx.JdbcConnectionPool
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import scala.collection.mutable.ListBuffer

object QRefTest {

  def main(args: Array[String]):Unit = {
    var ontology_name = ""
    var q = ""
    var mode = "0"
    for( i <- 0 to args.length - 1){
         println(args(i));
      }
    if (args.length == 2) {
      ontology_name = args(0) 
      q = args(1)
    }
    else if (args.length == 3) {
      ontology_name = args(0) 
      q = args(1)
      mode = args(2)
    }
    else {
	    println("Give us the name of an .owl file (e.g., pizza.owl): ")
	    ontology_name = readLine("> ")
	    //val ontology_name = "pizza.owl"
	    println("Ok, groovy. Loaded the ontology.")
	    println("Now give us a query.")
	    println("And please don't forget the syntax, ottherwise you'll get parsing errors: q(?x, ...) :- C(?x)^R(?x, ?y)^...")
	    println("E.g.:\n q(?x) :- Pizza(?x)^hasIngredient(?x, ?y)")
	    q = readLine("> ")
	    println("Now choose the display mode.")
	    println("0: results only")
	    println("1: results & ontology query")
	    println("2: results, ontology query & SQL query")
	    mode = readLine("> ")
	    //val q = "q(?x) :- Pizza(?x)^hasIngredient(?x, ?y)"
	    println("Ok, great. We'll see what we can do.")
    }
    val t = OntologyLoader.loadOntology(ontology_name)
    //println(t.getClassesInSignature.size)
    val p = new QueryParser(t)
    val query = p.parseAll(p.query, q).get
    val reformulated_query = QueryReformulator.reformulate(query, t)
    	
    
    val sql = reformulated_query.map(q => q.toSQL)
    val querySQL = sql.mkString("\nUNION\n")
    PerfReformulator.createDatabase(t)
    PerfReformulator.insertAboxAssertions(t)
    println("\nThese are the obtained answers:")
    Database.forURL("jdbc:h2:~/ontology", driver = "org.h2.Driver") withSession {
      Q.queryNA[String](querySQL) foreach { println}
    }
    if (mode.equals("1") | mode.equals("2")) {
    	println("\nPerfect reformulation of the query wrt to the input TBox")
    	println(reformulated_query.mkString("\n"))
    	if (mode.equals("2")) {
    	  println("\nSQL Query posted:")
    	  println(querySQL)
    	}
    }
  }

}