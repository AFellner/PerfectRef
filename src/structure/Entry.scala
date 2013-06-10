package structure

trait Entry {
  def isbound:Boolean = true
}

class Var(name:String) extends Entry {
  var bound:Boolean = false;
  
  def bind = bound = true
  def unbind = bound = false
  
  override def toString:String = {
    return if (bound) name else "_"
  }
  override def isbound:Boolean = bound
}

class Const(name:String) extends Entry {
  override def toString:String = return name
  //def isbound:Boolean = true
}