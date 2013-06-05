package structure

class Query(head: Nary, body: List[Atom]) {
  
  def contains(atom: Atom) = body.contains(atom)
  def replace(thisAtom: Atom, thatAtom: Atom):Query = {
    new Query(head, body.map(i => if (i == thisAtom) thatAtom else i))
    
  }
  override def toString:String = {
    head + " :- " + body.mkString(",")
  }
}