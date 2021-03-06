//P02 (*) Find the last but one element of a list.
//Example:
//  scala> penultimate(List(1, 1, 2, 3, 5, 8))
//res0: Int = 5

def penultimate[A](list: List[A]): A = list match{
  case h :: _ :: Nil => h
  case _ :: tail => penultimate(tail)
  case Nil => throw new NoSuchElementException
}

def penultimate2[A](list: List[A]): A = {
  list.takeRight(2).head
}

println(penultimate2(List(1, 1, 2, 3, 5, 8)))