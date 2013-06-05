object Test {

  def main(args: Array[String]) = {
    var x = new Var("x")
    x.bind
    var y = new Var("y")
    val a = new Const("a")
    val b = new Const("b")
    val head = new Nary("q",List(x))
    val atom1 = new Unary("A",x)
    val atom2 = new Binary("R",y,a)
    val atom3 = new Unary("C",y)
    val body = List(atom1,atom2)
    val q = new Query(head,body)
    println(q)
    val q1 = q.replace(atom1,atom3)
    println(q1)
  }
}