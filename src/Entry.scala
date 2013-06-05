trait Entry {

}

class Var(name:String) extends Entry {
  var bound:Boolean = false;
  
  def bind = bound = true
  def unbind = bound = false
  
  override def toString:String = {
    return if (bound) name else "_"
  }
}

class Const(name:String) extends Entry {
  override def toString:String = return name
}