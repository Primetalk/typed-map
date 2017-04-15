package ru.primetalk.typed.ontology.meta

import ru.primetalk.typed.ontology.meta.metameta.Record

import scala.language.higherKinds

/**
  * Meta-meta package contains instruments to define meta level of ontology.
  *
  * For instance here we define type classes that allows us to use arbitrary types as property identifiers.
  */
package object metameta extends TypeMappings {
  /** Phantom type that represents a record. */
  abstract final class Record[A]

  /** Phantom type that represents a collection of elements of type A. */
  abstract final class MetaSeq[+A]

  trait PropertyIdTypeClass[PropertyId[-_,_]]{
    def name[A,B](p: PropertyId[A,B]): String
  }

  trait RecordTypeClass[RecordImpl[_], PropertyIdImpl[-_,_]]{
    type PropertyId[-A,B] = PropertyIdImpl[A,B]
    type PropertyHelper[A,B]
//    type Record[+A] =
    trait RecordWrapper[A] {
      def record: RecordImpl[A]
      type Key[B] = PropertyIdImpl[Record[A], B]
      def apply[B,D](key: Key[B])(implicit bd: TypeMapping[B, D], helper: PropertyHelper[A,B]): D

      def get[B,D](key: Key[B])(implicit bd: TypeMapping[B, D], helper: PropertyHelper[A,B]): Option[D]

      def updated[B,D](key: Key[B], value: D)(implicit bd: TypeMapping[B, D], helper: PropertyHelper[A,B]): RecordImpl[A]

      def remove[B, D](key: Key[B])(implicit bd: TypeMapping[B, D], helper: PropertyHelper[A,B]): RecordImpl[A]
    }

    def apply[A](record: RecordImpl[A]): RecordWrapper[A]
  }

  implicit def toRecordWrapper[RecordImpl[_], PropertyIdImpl[-_,_], A](record: RecordImpl[A])(implicit r: RecordTypeClass[RecordImpl, PropertyIdImpl]): r.RecordWrapper[A] = {
    r(record)
  }
//  implicit class RecordOps[RecordImpl[_], PropertyIdImpl[-_,_], A](record: RecordImpl[A])(implicit val r: RecordTypeClass[RecordImpl, PropertyIdImpl]){
//
//    val wrapper: r.RecordWrapper[A] = r.apply(record)
//
//    def updated[B,D](key: wrapper.Key[B], value: D)(implicit bd: TypeMapping[B, D], helper: r.PropertyHelper[A,B]): RecordImpl[A] =
//      wrapper.updated(key, value)
//
//    def get[B,D](key: wrapper.Key[B])(implicit bd: TypeMapping[B, D], helper: r.PropertyHelper[A,B]): Option[D] =
//      wrapper.get(key)
//
//    def apply[B,D](key: wrapper.Key[B])(implicit bd: TypeMapping[B, D], helper: r.PropertyHelper[A,B]): D =
//      wrapper.apply(key)
//  }

  /** This mechanism is used to get the type of surrounding Schema in `property`.
    *
    * Inside [[SchemaBuilder]] we define single instance [[SchemaBuilder.ThisSchemaRecordTypeId]]
    * of this trait. And that instance binds RecordType with actual type for which we define schema.
    */
  sealed trait RecordTypeId {
    type RecordType
  }

  trait SchemaBuilder[A] {

    implicit object ThisSchemaRecordTypeId extends RecordTypeId {
      type RecordType = A
    }

  }


  trait Meta {

    type PropertyIdImpl[-A, B]

    implicit def propertyIdTypeClassInstance: PropertyIdTypeClass[PropertyIdImpl]
      /**
      * This trait allows to preserve type information to make it available at runtime.
      */
    trait RunTimeTypeInformation {
      type RunTimeTypeInfo[A,B]
      case class PropertyTypeInfo[A,B](propertyIdImpl: PropertyIdImpl[A,B], runTimeTypeInfo: RunTimeTypeInfo[A,B])
      case class PropertiesTypeInfo(propertyTypeInfos: Seq[PropertyTypeInfo[_,_]]){
        def map = propertyTypeInfos.map(p => p.propertyIdImpl -> p).toMap[PropertyIdImpl[_, _], PropertyTypeInfo[_,_]]
      }
      implicit def captureTypeInfo[A,B](propertyIdImpl: PropertyIdImpl[A,B])(implicit rt: RunTimeTypeInfo[A,B]): PropertyTypeInfo[A,B] =
        PropertyTypeInfo[A,B](propertyIdImpl, rt)

      implicit class PropertyIdImplOps[A,B](propertyIdImpl: PropertyIdImpl[A,B]) {
        def captureTypeInfo(implicit rt: RunTimeTypeInfo[A,B]): PropertyTypeInfo[A, B] =
          PropertyTypeInfo[A, B](propertyIdImpl, rt)
      }

      def preserveTypeInfo(propertyTypeInfos: PropertyTypeInfo[_,_]*) =
        PropertiesTypeInfo(propertyTypeInfos)
    }

    /** Uses TypeTag to capture type information. */
    object TypeTagRtti extends RunTimeTypeInformation {

      import scala.reflect.runtime.universe.TypeTag

      override type RunTimeTypeInfo[A,B] = TypeTag[B]

      implicit def rt[A,B](implicit tt: TypeTag[B]): RunTimeTypeInfo[A,B] = tt

    }

  }


}
