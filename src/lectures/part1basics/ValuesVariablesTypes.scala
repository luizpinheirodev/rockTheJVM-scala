package lectures.part1basics

object ValuesVariablesTypes extends App {

  val x = 42
  println(x)

  //VAL ARE IMMUTABLE
  //COMPILE can infer types

  val aString: String = "Hello"; val anotherString: String = "Bye" // Unrecommended

  val aBoolean: Boolean = false
  val aChar: Char = 'a'
  val anInt: Int = x
  val aShort: Short = 1010
  val aLong: Long = 99999999999999L
  val aFloat: Float = 2.0f
  val aDouble: Double = 3.14


  //variable
  var aVariable: Int = 4
  aVariable = 5 // side affects


}
