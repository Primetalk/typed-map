package ru.primetalk.synapse.slick.lifted

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.reflect.runtime.universe._

/**
 * http://habrahabr.ru/post/248331/
 * @author zhizhelev, 11.02.15.
 */
@RunWith(classOf[JUnitRunner])
class YTest extends FunSuite {
  test("Y-combinator test"){
    type f[T,R] = Any => T => R
    def Y[T,R](fun:f[T,R]):T=>R = fun(fun)

    val fact0: f[Int,Int] = (f:Any) => (n:Int) => if(n<=1) 1 else f.asInstanceOf[f[Int,Int]](f)(n-1)*n

    val fact = Y(fact0)

    assert(fact(5) === 120)
  }
}
