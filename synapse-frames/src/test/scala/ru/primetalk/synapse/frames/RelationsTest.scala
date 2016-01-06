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
  val longId = Rel[Identified, Long]("id")

  // properties can be grouped into the companion object
  object Box {
    val width = Rel[Box, Int]("width")
    val height = Rel[Box, Int]("height")
  }

  import Box._

  val boxScheme = record[Box](simpleRel(width), simpleRel(height))

  val b0 = update(empty[Box], width, simple(10))
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

  val boxes = Rel[Global, Seq[Box]]("boxes")

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
    empty[Global].set(boxes, seq(b0, b1))

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

  abstract class Shape
  trait BoundingRectangle

  final class Rectangle extends Shape with BoundingRectangle
  final class Circle extends Shape with BoundingRectangle

  test("longer hierarchy") {
    object BoundingRectangleS extends PropertySeq[BoundingRectangle] {
      val width = simpleProperty[Int]("width")
      val height = simpleProperty[Int]("height")
    }
    import BoundingRectangleS._

    object CircleS extends PropertySeq[Circle] {
      importProperties(BoundingRectangleS.toScheme)
      val radius = Rel[Circle, Int]("radius")
    }
    import CircleS._

    val name = Rel[Shape, String]("name")

    val onlyBoundingRectScheme = BoundingRectangleS.toScheme
    val someInfoAboutACircle = record[Circle](simpleRel(radius))
    val fullInfoAboutACircle = someInfoAboutACircle ++ onlyBoundingRectScheme

    val circ10 = new Builder(someInfoAboutACircle).
      set(radius, simple(10)).
      toInstance

    val shape10 = new Builder(fullInfoAboutACircle).
      fillFromInstance(circ10).
      set(width, simple(10)).
      set(height, simple(10)).
      toInstance
    assert(shape10.get(radius) === circ10.get(radius))

    assert(isMatching(shape10, someInfoAboutACircle))
    assert(isMatching(shape10, fullInfoAboutACircle))
    assert(isMatching(circ10, someInfoAboutACircle))
    assert(!isMatching(circ10, fullInfoAboutACircle))
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
}
