package structure

class Query(head: Nary, body: Set[Atom]) {
  
  def contains(atom: Atom) = body.contains(atom)
  def replace(thisAtom: Atom, thatAtom: Atom):Query = {
    new Query(head, body.map(i => if (i == thisAtom) thatAtom else i))
    
  }
  override def toString:String = {
    head + " :- " + body.mkString(",")
  }
  def getHead = head
  def getBody = body
  override def equals(other: Any) = other match {
    case other: Query =>  head == other.getHead && body.toSet == other.getBody.toSet
    case _ => false
  }
}