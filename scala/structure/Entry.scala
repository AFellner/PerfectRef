package structure

class Entry(name:String) {
  def isbound:Boolean = true
  def getName = name
  def ==(other:Entry):Boolean = other match {
    case other:Var if (this.isInstanceOf[Var]) => this.asInstanceOf[Var] == other.asInstanceOf[Var]
    case other:Const if (this.isInstanceOf[Const]) => this.asInstanceOf[Const] == other.asInstanceOf[Const]
    case _ => false
  }
  
}

class Var(name:String) extends Entry(name:String) { 
  var bound:Boolean = true
  def bind = bound = true
  def unbind = bound = false 
  override def isbound:Boolean = bound  
  override def toString:String = if (bound) "?" + name else "_"
  def ==(other:Var):Boolean = (/*this.isbound && other.isbound &&*/ name == other.getName)
}

class Const(name:String) extends Entry(name:String) {
  override def toString:String = return name
  def ==(other:Const):Boolean = name == other.getName

}