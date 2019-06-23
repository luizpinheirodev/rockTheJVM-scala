package exercises

abstract class MyList[+A] {

  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B]

  def printElements: String

  override def toString: String = "[" + printElements + "]"

  def map[B](transformer: MyTransformer[A, B]): MyList[B]
  def flatMap [B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]

  def ++[B >: A](list: MyList[B]): MyList[B]

}

case object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: MyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)

  override def printElements: String = ""

  def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty
  def flatMap [B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty
  def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty

  def++[B >: Nothing](list: MyList[B]) : MyList[B] = list
}

 case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h

  def tail: MyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](element: B): MyList[B] = new Cons(element, this)

  def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  def filter(predicate: MyPredicate[A]): MyList[A] =
    if(predicate.test(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def map[B](transformer: MyTransformer[A, B]): MyList[B] =
    new Cons(transformer.transform(h), t.map(transformer))

  def ++[B >: A](list: MyList[B]) : MyList[B] = new Cons(h, t ++ list)

  def flatMap [B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)




}

/*
    1.  Generic trait MyPredicate[-T] with a little method test(T) => Boolean
    2.  Generic trait MyTransformer[-A, B] with a method transform(A) => B
    3.  MyList:
        - map(transformer) => MyList
        - filter(predicate) => MyList
        - flatMap(transformer from A to MyList[B]) => MyList[B]
        class EvenPredicate extends MyPredicate[Int]
        class StringToIntTransformer extends MyTransformer[String, Int]
        [1,2,3].map(n * 2) = [2,4,6]
        [1,2,3,4].filter(n % 2) = [2,4]
        [1,2,3].flatMap(n => [n, n+1]) => [1,2,2,3,3,4]
   */

trait MyPredicate[-T] {
  def test(elem: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(element: A) : B
}

object ListTest extends App {
  val listOfInteger: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val cloneOfInteger: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val anotherListOfInteger: MyList[Int] = new Cons(4, new Cons(5, Empty))
  val listOfStrings: MyList[String] = new Cons("Hello", new Cons("Scala", Empty))

  println(listOfInteger.toString)
  println(listOfStrings.toString)

  println(listOfInteger.map(new MyTransformer[Int, Int] {
    override def transform(element: Int): Int = element * 2
  }).toString)

  println(listOfInteger.filter(new MyPredicate[Int] {
    override def test(elem: Int): Boolean = elem % 2 == 0
  }))

  println((listOfInteger ++ anotherListOfInteger).toString)

  println(listOfInteger.flatMap(new MyTransformer[Int, MyList[Int]] {
    override def transform(element: Int): MyList[Int] = new Cons(element, new Cons(element +1, Empty))
  }).toString)

  println(cloneOfInteger == listOfInteger)


}
