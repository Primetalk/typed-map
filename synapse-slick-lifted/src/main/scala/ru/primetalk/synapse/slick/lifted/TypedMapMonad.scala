package ru.primetalk.synapse.slick.lifted

/**
 * @author zhizhelev, 01.02.15.
 */
object TypedMapMonad {
  def apply[E](keyValues: KeyValue[E,_]*) =
    new TypedMapImpl[E](Map(keyValues.map(kv => (kv.key, kv.value)): _*))

  implicit class KeyEx[E, T](key: Key[E, T]) {
    def ::=(value: T) = KeyValue(key, value)
  }

}

///**
// * A special kind of TypedMap for storing any data encapsulated in some monad.
// */
//trait TypedMapMonad[Monad[_]] {
//  def apply[T](key: Key[T]): Monad[T]
//
//  def get[T](key: Key[T]): Option[Monad[T]]
//
//  def get0(k: KeyInfo): Option[Monad[Any]] = k match {
//    case OptionalKeyInfo(key) =>
//      get(key)
//    case key: Key[_] =>
//      get(key.asInstanceOf[Key[Any]])
//  }
//
//  def updated[T](key: Key[T], value: Monad[T]): TypedMapMonad[Monad]
//}
//
//case class TypedMapMonadImpl[Monad[_]](map: Map[Key[_], Any]) extends TypedMapMonad[Monad] {
//  override def apply[T](key: Key[T]): Monad[T] = map(key).asInstanceOf[Monad[T]]
//
//  override def updated[T](key: Key[T], value: Monad[T]): TypedMapMonad[Monad] = new TypedMapMonadImpl(map.updated(key, value))
//
//  override def get[T](key: Key[T]): Option[Monad[T]] = map.get(key).asInstanceOf[Option[Monad[T]]]
//
//}