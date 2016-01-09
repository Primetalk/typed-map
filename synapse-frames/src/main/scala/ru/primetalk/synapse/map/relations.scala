/**L3
  * Here we create instruments to represent ontology relations.
  *
  * There exists the following notions;
  * E - An identifiable entity type
  * Id[E] - an entity identifier
  * Rel[E,T] - relation between an entity and a simple type T
  * Record[A] - a flexible record that contains relation values of type A. A can be artifitial type
  *  built by with from different entities
  * Graph - the container where all relations and values are stored
  * Schema -  the complete collection of relation ids.
  *
  */
package ru.primetalk.synapse.map

/** Here we create type classes to work with a particular storage kind.*/
import scala.language.higherKinds

/** Key class that allows to distinguish relations from other types.
  * This type is used to find appropriate implicit type-class.
  */
sealed trait RelationId0
/** An identifier of a binary relation between A and B.
  * A relational algebra can be parameterized by two classes:
  * - actual type that stores the data (TypedMap[E])
  * - the type of a key that can be used in that storage.
  * These types should come as a couple and can be represented as a typeclass.
  */
trait RelationId[A,B] extends RelationId0

/**  A construction of two subsequent relations.*/
case class RelationPair[A, B, C, R1 <:RelationId[A,B], R2<: RelationId[B,C]](l:R1, r:R2) extends RelationId[A,C]

/** A typeclass that contains basic operations on the storage type.*/
trait StorageTypeClass[Storage[_]] {
  type KeyKind[E,T] <: RelationId[Storage[E],T]

  /** Getter for the key.
    * @param key the key to get
    * @param s the storage that keeps the data
    * @tparam E the entity type
    * @tparam T the value type
    * @return the value at key.
    */
  def get[E,T](key:KeyKind[E,T])(s:Storage[E]):T

  /**
    * Gets the content of the key.
    * @return None if the key is absent.
    */
  def getOpt[E,T](key:KeyKind[E,T])(s:Storage[E]):Option[T]
  /**
    * Setter for the key.
    * @param key the key to be set
    * @param s the storage that keeps the data
    * @param v the value to insert into the storage at the key.
    * @tparam E the entity type
    * @tparam T the value type
    * @return the updated storage that has new value at key.
    */
  def set[E,T](key:KeyKind[E,T])(s:Storage[E])(v:T):Storage[E]

  /**
    * A reflection on the storage.
    * @param s the storage with some data
    * @tparam E the entity type
    * @return the sequence of keys that can be extracted from the storage
    */
  def keys[E](s:Storage[E]):Seq[KeyKind[E,_]]

  /** Checks whether the storage contains the key.
    *
    * @param key the key to check
    * @param s the storage
    * @tparam E the entity type
    * @tparam T the type of the value of the key
    * @return true if the key can be requested.
    */
  def contains[E,T](key:KeyKind[E,T])(s:Storage[E]):Boolean
}

/**
  * Typeclass for a particular relation kind.
  * @tparam Relation
  * tparam Container container type
  */
trait RelationTypeClass[Relation <: RelationId0]{
  type Container
  type KeyKind[X<:Container,Y] <: Relation
  def getter[E<:Container,T](rel:KeyKind[E, T]):E=>T
  def getterOpt[E<:Container,T](rel:KeyKind[E, T]):E=>Option[T]
  def setter[E<:Container,T](rel:KeyKind[E, T]):E=>T=>E
  def contains[E<:Container,T](rel:KeyKind[E, T]):E=>Boolean
  def relations[E<:Container]:Container=>Seq[KeyKind[E,_]]
}
/**Companion object for relations.*/
object RelationId{
  implicit class RelOps[A,B](rel:RelationId[A,B]){
//    def ~[C](rel2:RelationId[B,C]) = RelationPair(rel,rel2)
  }
  /*
  implicit def pairTypeClass[A, B, C, R1[BB] <:RelationId[A,BB], R2[CC]<: RelationId[B,CC]]
  (implicit tcA:RelationTypeClass[R1], tcB:RelationTypeClass[R2]):RelationTypeClass[RelationPair[A, B, _, R1, R2]] = new RelationTypeClass{
    def getter[T](rel:RelationPair[A, B, C, R1, R2]):S=>T
    def getterOpt[T](rel:RelationPair[S, T]):S=>Option[T]
    def setter[T](rel:RelationPair[S, T]):S=>T=>S
    def contains[T](rel:RelationPair[S, T]):S=>Boolean
    def relations:S=>Seq[RelationPair[S,_]]

  }
  */
}