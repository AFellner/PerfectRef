package structure

import scala.util.parsing.combinator._
import scala.collection.mutable
import structure._
import logic._
import org.semanticweb.owlapi.apibinding._
import org.semanticweb.owlapi.model._
import scala.collection.JavaConversions._

class QueryParser(ontology: OWLOntology) extends RegexParsers{
	val classList = ontology.getClassesInSignature(true).toList
	val classMap = mutable.Map.empty[String, OWLClass] //empty map
	for (owlClass <- classList) classMap += (owlClass.toStringID().split("#")(1) -> owlClass) //creates map where elements are (Pizza -> blabla#Pizza)
	
	val propertyList = ontology.getObjectPropertiesInSignature(true).toList 
	val propertyMap = mutable.Map.empty[String, OWLObjectProperty]
	for (owlProperty <- propertyList) propertyMap += (owlProperty.toStringID().split("#")(1) -> owlProperty)
  
	val varMap = mutable.Map.empty[String, Var]
	
	def query: Parser[Query] = head~":-"~body ^^ {
	  case head~":-"~body => new Query(head, body)	
	}
	def head: Parser[Nary] = name~"("~repsep(variable, ",")~")"	^^ {
	  case name~"("~var_list~")" => new Nary(name, var_list) 
	}
	def body: Parser[List[Atom]] = repsep(atom, "^") ^^ (List() ++ _)	
	def atom:Parser[Atom] = unary_atom|binary_atom
	def unary_atom: Parser[Unary] = name~"("~entry~")" ^^ { 
	  case name~"("~entry~")" => new Unary(classMap(name), entry)
	  }	
	def binary_atom: Parser[Binary] = name~"("~entry~","~entry~")" ^^ {
	  case name~"("~e1~","~e2~")" => new Binary(propertyMap(name), e1, e2)
	}
	def entry: Parser[Entry] = variable|constant
	def variable: Parser[Var] = "?"~>"""[a-z]+""".r ^^ {
	  case name => {
	    varMap.getOrElse(name, {
	    val v = new Var(name)
	    varMap += (name -> v)
	    v
	    }
	  )}
	}
	def constant: Parser[Const] = """[a-z]+""".r ^^ (new Const(_))
	def name:Parser[String] = """\w+""".r ^^ (_.toString())
}