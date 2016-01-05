package ru.primetalk.synapse.slick.lifted

import slick.ast._
import slick.lifted._
import scala.reflect.ClassTag
import slick.lifted.{TupleShape, FlatShapeLevel, MappedProductShape, Shape}
import slick.util.{TupleSupport, ProductWrapper}
import scala.language.implicitConversions
import ru.primetalk.synapse.map._
/**
 * Shape - represents types of rows in 3 different forms
 * - unpacked - actual row values
 * - packed - column names
 * - mixed - mixture of column names and ordinary types. This is used to represent constant column values within column sequence.
 *   buildParams is only available for Shapes where Mixed = Unpacked
 *   encodeRef is only available for Shapes where Mixed = Packed.
 * The conversion to tuple and from tuple is represented with TypeMapping (returned by #toNode)
 * @author zhizhelev, 31.01.15.
 */
class TypedMapShape[E,Level <: ShapeLevel, P <: Product](val keys: Seq[Key0])(
  /** The Shapes for the product elements. */
  val shapes: Seq[Shape[_, _, _, _]])
  extends Shape[Level, P, TypedMap[E], P] {


  /** Get an Iterator of a record value's element values. The default
    * implementation repeatedly calls `getElement`. */
  def getIterator(value: Any): Iterator[Any] =
    shapes.iterator.zipWithIndex.map(t => getElement(value, t._2))

  /** Convert a value of this Shape's (mixed) type to the fully packed type */
  def pack(value: Mixed) = {
    val elems = shapes.iterator.zip(getIterator(value)).
      map {
      case (p, f) => p.pack(f.asInstanceOf[p.Mixed])
    }
    buildValue(elems.toIndexedSeq).asInstanceOf[Packed]
  }

  /** Return the fully packed Shape. */
  def packedShape: Shape[Level, Packed, Unpacked, Packed] =
    new TypedMapShape(keys)(shapes.map(_.packedShape.asInstanceOf[Shape[_ <: ShapeLevel, _, _, _]])).asInstanceOf[Shape[Level, Packed, Unpacked, Packed]]

  /** Build a packed representation containing QueryParameters that can extract
    * data from the unpacked representation later.
    * This method is not available for shapes where Mixed and Unpacked are
    * different types. */
  def buildParams(extract: Any => Unpacked): Packed = {
    val elems = shapes.iterator.zipWithIndex.map { case (p, idx) =>
      def chExtract(u: Any): p.Unpacked = getElement(u, idx).asInstanceOf[p.Unpacked]
      p.buildParams(extract.andThen(chExtract))
    }
    buildValue(elems.toIndexedSeq).asInstanceOf[Packed]
  }

  /** Encode a reference into a value of this Shape.
    * This method may not be available for shapes where Mixed and Packed are
    * different types. */
  def encodeRef(value: Mixed, path: Node) = {
    val elems = shapes.iterator.zip(getIterator(value)).zipWithIndex.map {
      case ((p, x), pos) => p.encodeRef(x.asInstanceOf[p.Mixed], path)//???)///*new SimplyTypedNode(ElementSymbol(pos + 1)) ::*/ path)
    }
    buildValue(elems.toIndexedSeq)
  }

  /** Return an AST Node representing a mixed value. */
  def toNode(value: Mixed) =
    TypeMapping(
      ProductNode(shapes.iterator.zip(getIterator(value)).map {
        case (p, f) => p.toNode(f.asInstanceOf[p.Mixed])
      }.toSeq),
      MappedScalaType.Mapper(toBase, toMapped, None), classTag)

  def toBase(v: Any) = new ProductWrapper(getIterator(v).toIndexedSeq)

  def toMapped(v: Any) = buildValue(TupleSupport.buildIndexedSeq(v.asInstanceOf[Product]))

  def classTag = implicitly[ClassTag[TypedMap[E]]]

  /** Build a record value represented by this Shape from its element values. */
  def buildValue(elems: IndexedSeq[Any]): Any =
    TypedMap(
      keys.zip(elems).flatMap {
        case (OptionalKey0(key), Some(value)) =>
          Some(KeyValue(key.asInstanceOf[Key[E,Any]], value))
        case (OptionalKey0(key), None) =>
          None
        case (OptionalKey0(key), other) =>
          throw new IllegalArgumentException(s"Cannot convert $other to Option")
        case (key: Key[_,_], value) =>
          Some(KeyValue(key.asInstanceOf[Key[Any,Any]], value))
      }: _*
    )

  /** Get the element value from a record value at the specified index. */
  def getElement(value: Any, idx: Int): Any = value match {
    case t: TypedMap[_] =>
      t.get0(keys(idx)).get
    case p: Product =>
      p.productElement(idx)
  }

}

object TypedMapShape {

  implicit class ColumnsTupleEx[P <: Product](columns: P)(implicit shape: Shape[FlatShapeLevel, P, _, P]) {
    def typedMap[E](keys: Key[E,_]*) = new TypedMapShape[E, FlatShapeLevel, P](keys.toSeq)(shape.asInstanceOf[TupleShape[_, _, _, _]].shapes)

    def toProvenShape[E](keys: Key[E,_]*) = ProvenShape.proveShapeOf(columns)(typedMap(keys: _*))
  }

  /** Build a TypedMap from its element values. */
  def buildTypedMap(keys: Seq[Key0], elems: IndexedSeq[Any]): TypedMap[Any] =
    TypedMap(
      keys.zip(elems).flatMap {
        case (OptionalKey0(key), Some(value)) =>
          Some(KeyValue(key.asInstanceOf[Key[Any,Any]], value))
        case (OptionalKey0(key), None) =>
          None
        case (OptionalKey0(key), other) =>
          throw new IllegalArgumentException(s"Cannot convert $other to Option")
        case (key: Key[_,_], value) =>
          Some(KeyValue(key.asInstanceOf[Key[Any,Any]], value))
      }: _*
    )
  def typedMapToValues(keys: Seq[Key0])(map:TypedMap[_]):IndexedSeq[Any] =
    keys.map{
      case OptionalKey0(key) => map.get0(key)
      case key: Key[_,_] => map.get0(key).get
    }.toIndexedSeq

}
object TypedMapProjection{
  implicit def toShapedValue2[T](value:T):ToShapedValue2[T] = new ToShapedValue2[T](value)

  // Work-around for SI-3346
  final class ToShapedValue2[T](val value: T) extends AnyVal {
    @inline def toTypedMap[E,U<:Product](keys: Key0*)(//f: (U => R), g: (R => U))(
      implicit shape: Shape[_ <: FlatShapeLevel, T, U, _]) = {
      val g:TypedMap[_]=>U = (m:TypedMap[_]) => TupleSupport.buildTuple(TypedMapShape.typedMapToValues(keys)(m)).asInstanceOf[U]
      val f:U=>TypedMap[_] = u=> TypedMapShape.buildTypedMap(keys, u.productIterator.toIndexedSeq)
      new MappedProjection[TypedMap[E], U](shape.toNode(value),
        MappedScalaType.Mapper(g.asInstanceOf[Any => Any], f.asInstanceOf[Any => Any], None), implicitly[ClassTag[TypedMap[E]]])
    }
  }

}