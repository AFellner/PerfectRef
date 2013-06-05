trait Atom

class Unary(name: String, e: Entry) extends Atom {
  override def toString() = name + "("+e+")"
}

class Binary(name: String, e1: Entry, e2: Entry) extends Atom {
  override def toString() = name + "("+e1+","+e2+")"
}
class Nary(name:String, e: List[Var]) extends Atom {
  override def toString() = name + "(" + e.mkString(",") + ")"
}