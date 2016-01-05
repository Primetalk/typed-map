package ru.primetalk.synapse.slick.lifted

import ru.primetalk.synapse.map._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
 * Schema describes possible keys in a TypedMap
 * and their schemes.
 * @author zhizhelev, 13.02.15.
 */
sealed trait Schema[T] {
  def typeTag: TypeTag[T]
  def classTag:ClassTag[T]
}

case class SimpleTypeSchema[T](implicit val typeTag: TypeTag[T], val classTag:ClassTag[T]) extends Schema[T]

case class TypedMapSchema[T](keys:Key[T,_]*)(implicit val typeTag: TypeTag[TypedMap[T]]) extends Schema[TypedMap[T]] {
  def classTag:ClassTag[TypedMap[T]] = implicitly[ClassTag[TypedMap[Any]]].asInstanceOf[ClassTag[TypedMap[T]]]
}

/** Builder for the schema of type T.
  * T can be TypedMap[E]. In this case a TypedMapSchemaBuilder is preferred.
  * */
trait SchemaBuilder[T] {
  def typeTag:TypeTag[T]
}

trait TypedMapSchemaBuilder[E] extends SchemaBuilder[TypedMap[E]]{
  def key[V](name:String):Key[E,V]

}