///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2014                                   //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2014.
 * All rights reserved.
 * Authors: A.Zhizhelev
 *
 * Created: 26.06.14, zhizhelev
 */
package ru.primetalk.synapse.frames

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import shapeless.HNil


@RunWith(classOf[JUnitRunner])
class RelationsTest extends FunSuite {

  import ru.primetalk.synapse.frames.relations._

  //  implicit val int  = SimpleScheme[Int]
  //  implicit val long = SimpleScheme[Long]

  // Very small hierarchy of entities.
  trait Identified

  class Box extends Identified

  class Global

  // entity properties
  val longId = NamedProperty[Identified, Long]("id")

  // properties can be grouped into the companion object
  object Box {
    val width = NamedProperty[Box, Int]("width")
    val height = NamedProperty[Box, Int]("height")
  }

  import Box._

  val boxScheme = record[Box](simpleRel(width), simpleRel(height))

  val b0 = update(emptyInstance[Box], width, simple(10))
  val b1 = b0.set(height, simple(20))

  test("b0 doesn't have all properties of boxScheme") {
    assert(!boxScheme.hasAllProperties(b0))
  }

  test("b1 has all properties of boxScheme") {
    assert(boxScheme.hasAllProperties(b1))
  }

  val b2 = b1.set(longId, simple(10L))
  test("b2 has property from parent") {
    assert(simpify(b2.get(longId)) === 10L)
  }

  val boxes = NamedProperty[Global, Seq[Box]]("boxes")

  implicit val boxesScheme = CollectionScheme[Box](boxScheme)

  val globalScheme = record[Global](boxes)

  test("Get meta test") {
    val widthScheme = globalScheme / boxes / Element / width
    assert(widthScheme === SimpleScheme[Int])
  }

  test("b2 matches box scheme") {
    assert(isMatching(b2, boxScheme))
  }
  test("align raw data") {
    val b3 = align(List(10, 20), boxScheme)
    assert(b3 === b1)
  }
  test("align and unalign") {
    val b1data = unalign(InstanceWithMeta(b1, boxScheme))
    assert(b1data === List(10,20) )
    val b1restored = align(b1data, boxScheme)
    assert(b1restored === b1)
  }

  val globalInstance =
    emptyInstance[Global].set(boxes, seq(b0, b1))

  val path0width = boxes / 0 / width
  val path1width = boxes / 1 / width

  test("navigation through hierarchy") {
    val w0 = navigate(globalInstance, path0width)
    val w1 = navigate(globalInstance, path1width)
    assert(w0 === w1)
  }

  test("update") {
    val globalInstance2 = globalInstance.set(path0width, simple(33))
    assert(globalInstance2.get(boxes / 0 / width) === simple(33))
  }

  val boxBuilder = new Builder(boxScheme)
  boxBuilder.set(width, simple(10))
  boxBuilder.set(height, simple(20))
  val b4 = boxBuilder.toInstance

  test("builder"){
    assert(b1 === b4)
  }

  trait BoundingRectangleOnt {

    abstract class Shape

    trait BoundingRectangle

    final class Rectangle extends Shape with BoundingRectangle

    final class Circle extends Shape with BoundingRectangle

    object BoundingRectangleS extends PropertySeq[BoundingRectangle] {
      val width = simpleProperty[Int]("width")
      val height = simpleProperty[Int]("height")
    }

    import BoundingRectangleS._

    object CircleS extends PropertySeq[Circle] {
      importProperties(BoundingRectangleS.toScheme)
      val radius = NamedProperty[Circle, Int]("radius")
    }


    val name = NamedProperty[Shape, String]("name")

    val onlyBoundingRectScheme = BoundingRectangleS.toScheme
    val someInfoAboutACircle = record[Circle](simpleRel(CircleS.radius))
    val fullInfoAboutACircle = someInfoAboutACircle ++ onlyBoundingRectScheme

    val circ10 = new Builder(someInfoAboutACircle).
      set(CircleS.radius, simple(10)).
      toInstance

    val shape10 = new Builder(fullInfoAboutACircle).
      fillFromInstance(circ10).
      set(width, simple(10)).
      set(height, simple(10)).
      toInstance
  }
  test("longer hierarchy") {
    new BoundingRectangleOnt {
      import CircleS._
      assert(shape10.get(radius) === circ10.get(radius))

      assert(isMatching(shape10, someInfoAboutACircle))
      assert(isMatching(shape10, fullInfoAboutACircle))
      assert(isMatching(circ10, someInfoAboutACircle))
      assert(!isMatching(circ10, fullInfoAboutACircle))
    }
  }

  type MyType = List[Int] with Boolean

  test("type name"){
    assert(typeName[List[Int]]() === "List[Int]")
    assert(typeName[List[_]]() === "List[_]")
    assert(typeName[Map[_,_]]() === "Map[_,_]")
    assert(typeName[List[Int] with Boolean]() === "List[Int] with Boolean")
    assert(typeName[MyType]() === "MyType")
    assert(typeName[MyType](dealias = true) === "List[Int] with Boolean")
    assert(typeName[Boolean with List[Int]]() === "Boolean with List[Int]")
    assert(typeName[Boolean with List[Int]](sorted = true) === typeName[List[Int] with Boolean](sorted = true))
  }

  test("RShape"){
    new BoundingRectangleOnt {
      import BoundingRectangleS._
//      radius :: radius :: new RecordShapeOps[Circle, HNil, NilRShape[Circle]](rnil[Circle])
      val shape = height :: width :: rnil[BoundingRectangle]
      val svalue = shape := 10::20::HNil
      assert(svalue.value.tail.head === 20)
      assert(svalue.get(height) === Some(10))
      assert(svalue.get(width) === Some(20))
    }
  }
}
