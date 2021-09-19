import scala.annotation.tailrec

def lastNthRecursive[A](n: Int, ls: List[A]): A = {
  def lastNthR(count: Int, resultList: List[A], curList: List[A]): A =
    curList match {
      case Nil if count > 0 => throw new NoSuchElementException
      case Nil              => resultList.head
      case _ :: tail2        =>
        lastNthR(count - 1,
          if (count > 0) resultList else resultList.tail,
          tail2)
    }
  if (n <= 0) throw new IllegalArgumentException
  else lastNthR(n, ls, ls)
}

println(lastNthRecursive(1, List(3,5,7,11,66,777,4343434)))

val l = List(3,5,7,11,66,777,4343434)
println(l)

def pack[A](ls: List[A]): List[List[A]] = {
  if (ls.isEmpty) List(List())
  else {
    val (packed, next) = ls span { _ == ls.head }
    if (next == Nil) List(packed)
    else packed :: pack(next)
  }
}

val ls = List(3,5,7,11,66,777,4343434)
val (packed, next) = ls span( x => x == ls.head )
print("1 " +packed + " 2 "  + next)

@tailrec
def applyTransformations(initial: String, transformations: Seq[String => String]): String =
  transformations match {
    case head :: tail => applyTransformations(head(initial), tail)
    case Nil => initial
  }

val reverse = (s: String) => s.reverse

val toUpper = (s: String) => s.toUpperCase

val appendBar = (s: String) => s + "bar"

println(applyTransformations("foo", List(reverse, toUpper, appendBar)))

