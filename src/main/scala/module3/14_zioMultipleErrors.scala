package module3

import zio.IO
import zio.ZIO
import zio.Cause
import zio.Cause.{Both, Internal}
import zio.console._


trait Error extends Product
case object E1 extends Error
case object E2 extends Error

object multipleErrors{
    val z1: IO[E1.type, Int] = ZIO.fail(E1)

    val z2: IO[E2.type, Int] = ZIO.fail(E2)

    val result = z1 zipPar z2

    val app = result.tapCause{
        case Both(left, right) =>
            ZIO.effect(println(left.failureOption)) *>
              ZIO.effect(println(right.failureOption))
    }
    
}
