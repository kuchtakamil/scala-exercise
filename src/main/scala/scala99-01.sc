//P01 (*) Find the last element of a list.
//Example:
//  scala> last(List(1, 1, 2, 3, 5, 8))
//res0: Int = 8
import java.util.NoSuchElementException

def last[A](list: List[A]): A = list match {
  case h :: Nil   => h
  case h :: tail  => last(tail)
  case Nil        => throw new NoSuchElementException
}

def last2[A](list: List[A]): A = list.last

println(last(List(1, 1, 2, 3, 5, 8)))