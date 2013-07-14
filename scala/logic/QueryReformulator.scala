package logic

import structure._
import scala.collection.mutable
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._

object QueryReformulator {
  
  def reformulate(q:Query, t:OWLOntology):List[Query] = {
    val pIs = PerfReformulator.readPIs(t)
    var pr:List[Query] = List(q)
    var pr_prime:List[Query] = List()
    
    while(pr_prime != pr){
    
	    pr_prime = pr
	    
	    for (query <- pr_prime){
	        for (g <- query.getBody){
	          for (pi <- pIs){
	            if (g.applicable(pi)) {
	              if (pr.count(_ == query.replace(g, g.apply(pi))) == 0) pr = query.replace(g, g.apply(pi))::pr
	              }
	          }
	        }
	        
	        for (g1 <- query.getBody){
	          for (g2 <- query.getBody if !(g1 == g2)){
	            if (Unifier.unifiable(g1, g2)) {
	             val candidate_query_by_unification = Unifier.reduce(query, g1, g2) 
	             if (pr.count(_ == candidate_query_by_unification) > 0) pr else pr = candidate_query_by_unification::pr 
	            }
	          }
	        }
	      }
	    }
    return pr.reverse    
  }

}