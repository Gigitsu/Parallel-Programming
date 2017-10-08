import reductions.ParallelParenthesesBalancing._

val foo = (x: Int) => x * 2
val boo = (x: Int) => x + 1

(1::2::3::4::Nil).map(boo.compose(foo))

def scanLeft(xs: List[Int])(x0: Int)(f: (Int, Int) => Int): List[Int] = {
  val out = new Array[Int](xs.size + 1)
  out(0) = x0
  var i = 0

  while(i < xs.length) {
    val x = f(out(i), xs(i))
    i += 1
    out(i) = x
  }

  out.toList
}

def scanLeftRec(xs: List[Int])(x0: Int)(f: (Int, Int) => Int): List[Int] = xs match {
  case Nil => List(x0)
  case x :: xs1 => x0 :: scanLeft(xs1)(f(x0, x))(f)
}

scanLeft(List(1,3,8))(100)(_ + _)

parBalance(Array('(', '(', ')', ')'), 1)
