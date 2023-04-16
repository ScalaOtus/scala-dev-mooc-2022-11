package part1.scala3testclassContext

/*
class GivenIntBox(using val usingParameter: Int)(using val usingParameterStr: String):
  def myInt = summon[Int](using  usingParameter)
  def myString = summon[String](using  usingParameterStr)

  object scala3testclassContext{
    @main def scala3testclassContextEx()={
      val b = GivenIntBox(using 42)(using "xdv42")
      import b.given

      //    println(summon[Int])
      println(summon[String])

      //  println(usingParameter)
      println(usingParameterStr)

    }

  }



*/
/*
class GivenIntBox(using val usingParameter: Int):
  def myInt = summon[Int](using  usingParameter)

object scala3testclassContext {
  @main def scala3testclassContextstart()={
    val b = GivenIntBox(using 23)
    import b.given

    //Снаружи GivenIntBox параметр usingParameter выглядит так,
    // как если бы он был определен в классе как заданный параметр
    // usingParameter: Int, в частности, он должен быть импортирован,
    println(summon[Int])
    println(usingParameter)


  }

}*/
