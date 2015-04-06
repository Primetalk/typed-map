package ru.primetalk.synapse.map

import scala.collection.generic.CanBuildFrom

/**
 * Typeless type for a key. It is helpful to deal without generics.
 * @author zhizhelev, 31.01.15.
 */
sealed trait Key0 {
  def isOptional: Boolean
}

/** Keys address data elements in typed maps.
  * @tparam E Left type of a key denotes some type that corresponds to the meaning of the typed map.
  *           As far as typed maps are covariant, key's left type is contravariant because the
  *           key much is like a function from instance of TypedMap to value T.
  * @tparam T right type of the Key
  */
trait Key[-E, +T] extends Key0 {
//  type LeftType >: E contravariant type cannot be stored in invariant type
//  type RightType <: T covariant type cannot be stored in invariant type

  def isOptional = false

  def ? = new OptionalKey0[E, T](this)
}

case class StringKey[-E, +T](name: String) extends Key[E, T] { def value = name }

case class LongKey[-E, +T](id: Long) extends Key[E, T] { def value = id }

/** A key that is based on an arbitrary value*/
case class AnyKey[-E, +T, A](value: A) extends Key[E, T]

/** A variant of a key that has type Option[T].
  * This can be used to explicitly express the idea of an absent value.
  * For instance when we want to have a projection of a few properties,
  * we may use optional keys for those properties that can be empty.
  * */
case class OptionalKey0[-E, +T](key: Key[E, T]) extends Key[E,Option[T]] {

  override def isOptional = true
}

/**
 * Typed map contains data that is addressed with typed keys.
 * The map is immutable.
 *
 * @tparam E type that corresponds to the meaning of the typed map. Usually types E participate in
 *           a type hierarchy (a trait, an abstract class, a final abstract class).
 *           TypedMaps do nothing with instances of that type. It is used only at compile time
 *           to constrain key usage.
 */
trait TypedMap[+E] {

  def keys: Iterable[Key0]

  def keySet: Set[Key0]

  def apply[T](key: Key[E, T]): T

  def get[T](key: Key[E, T]): Option[T]

  def get0[T](k: Key0): Option[T] = k match {
    case OptionalKey0(key) =>
      get(key.asInstanceOf[Key[E, T]])
    case key: Key[_, _] =>
      get(key.asInstanceOf[Key[E, T]])
  }

  def updated[T](key: Key[E, T], value: T): TypedMap[E]

  def contains(k: Key0): Boolean

  def values:Iterable[Any]

}

sealed trait KeyValue00 {
  def key: Key0
  def value: Any
}
sealed trait KeyValue0[E] extends KeyValue00{
  def key: Key[E, _]
  def value: Any
}
/** Key-value pair can be used to traverse the whole typed map.*/
case class KeyValue[E, T](key: Key[E, T], value: T) extends KeyValue0[E]

case class TypedMapImpl[+E](map: Map[Key0, Any]) extends TypedMap[E] {

  def keys: Iterable[Key0] = map.keys

  def keySet: Set[Key0] = map.keySet

  def values:Iterable[Any] = map.values

  def apply[T](key: Key[E, T]): T = map(key).asInstanceOf[T]

  override def updated[T](key: Key[E, T], value: T): TypedMap[E] = new TypedMapImpl(map.updated(key, value))

  override def get[T](key: Key[E, T]): Option[T] = map.get(key).asInstanceOf[Option[T]]

  override def contains(k: Key0): Boolean = map.asInstanceOf[Map[Key0, Any]].contains(k)

//  def foreach[U](f: KeyValue0[E] => U): Unit = {
//    map.foreach{ case (k,v) => f(KeyValue(k.asInstanceOf[Key[E,Any]],v))}
//  }
//  map.map
//  def map[B, That](f: KeyValue[E,_] => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
//  ???
//  }
}

/** Another implementation of TypedMap that can be effectively used when traversing big tables.
  */
case class TypedMapImpl2[+E](keysArray:Array[Key0], valuesArray:Array[Any]) extends TypedMap[E] {

  override def keys: Iterable[Key0] = keysArray

  override def values: Iterable[Any] = valuesArray

  lazy val keySet: Set[Key0] = keysArray.toSet

  private
  lazy val reverseIndex: Map[Key0, Int] = keysArray.zipWithIndex.toMap

  private
  def keyIndexOpt(key:Key[E,_]) = reverseIndex.get(key)

  def apply[T](key: Key[E, T]): T = valuesArray(reverseIndex(key)).asInstanceOf[T]

  override def updated[T](key: Key[E, T], value: T): TypedMap[E] = new TypedMapImpl2(keysArray, valuesArray.updated(reverseIndex(key), value))

  override def get[T](key: Key[E, T]): Option[T] = keyIndexOpt(key).map(i => valuesArray(i).asInstanceOf[T])

  override def contains(k: Key0): Boolean = keySet.contains(k)

}


object TypedMap {
  def apply[E](keyValues: KeyValue[E, _]*) =
    new TypedMapImpl[E](Map(keyValues.map(kv => (kv.key, kv.value)): _*))

  implicit class KeyEx[E, T](key: Key[E, T]) {
    def ::=(value: T) = KeyValue(key, value)
  }

}

/** A class that represents a lot of instances with the same sequence of keys.
  *
  * This can be used to represent tables/relations.
  * */
case class RepTypedMap[+E](keys:Array[Key0], values:Iterable[Array[Any]]) extends Iterable[TypedMap[E]]{

  override def iterator: Iterator[TypedMap[E]] = values.iterator.map(TypedMapImpl2(keys,_))
}