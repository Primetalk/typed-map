package ru.primetalk.typed.ontology

import ru.primetalk.typed.ontology.meta.SimplePropertiesMeta
import ru.primetalk.typed.ontology.meta.metameta
import SimplePropertiesMeta.PropertyId
import ru.primetalk.typed.ontology.meta.metameta.{AnyTypeMappings, SchemaBuilder, RecordRepresentation, PropertyIdTypeClass, Record, RecordTypeClass, RecordTypeId}

import scala.language.higherKinds

/**
  * Created by zhizhelev on 15.04.17.
  */
package object tmap extends RecordRepresentation {

  val meta = SimplePropertiesMeta

  implicit def recordSupport: RecordTypeClass[RecordImpl, meta.PropertyIdImpl] = TypedMap.TypedMapRecordTypeClassInstance

  type RecordImpl[A] = TypedMap[A]

  case class TypedMap[A](map: Map[PropertyId[_,_], _])

  object TypedMap {

    implicit object TypedMapRecordTypeClassInstance extends RecordTypeClass[TypedMap, PropertyId] {

      object NoPropertyHelper

      type PropertyHelper[A, B] = NoPropertyHelper.type

      implicit def noPropertyHelper[A, B]: PropertyHelper[A, B] = NoPropertyHelper

      class RecordWrapperImpl[A](val record: TypedMap[A]) extends RecordWrapper[A] {
        override def apply[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelper[A,B]): D =
          record.map(key).asInstanceOf[D]

        override def get[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelper[A,B]): Option[D] =
          record.map.get(key).asInstanceOf[Option[D]]

        override def updated[B, D](key: Key[B], value: D)(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelper[A,B]): TypedMap[A] =
          TypedMap(record.map.updated(key, value))

        override def remove[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelper[A,B]): TypedMap[A] = TypedMap(record.map.filterKeys(_ == key))
      }
      override def apply[A](record: TypedMap[A]):
      RecordWrapper[A] = new RecordWrapperImpl[A](record)
    }

  }

  case class PropertyValue[PropertyIdImpl[-_,_], A, B, D](propertyId: PropertyIdImpl[Record[A],B], value: Option[D], bd: metameta.TypeMapping[B, D], r: PropertyIdTypeClass[PropertyIdImpl])

  implicit class PropertyIdOps[PropertyIdImpl[-_, _],A,B](propertyId: PropertyIdImpl[Record[A],B])(implicit val r: PropertyIdTypeClass[PropertyIdImpl]) {

    def :=[D](value: D)(implicit bd: metameta.TypeMapping[B, D]): PropertyValue[PropertyIdImpl, A, B, D] =
      PropertyValue[PropertyIdImpl, A, B, D](propertyId, Some(value), bd, r)

    def ?=[D](value: Option[D])(implicit bd: metameta.TypeMapping[B, D]): PropertyValue[PropertyIdImpl, A, B, D] =
      PropertyValue[PropertyIdImpl, A, B, D](propertyId, value, bd, r)
  }

  implicit class SchemaBuilderOps[A](schemaBuilder: SchemaBuilder[A]){
    def empty: TypedMap[A] = TypedMap[A](Map())
    def record(propValueList: PropertyValue[PropertyId, A, _, _]*): TypedMap[A] =
      TypedMap(
        propValueList.collect{
          case PropertyValue(key, Some(value), _, _) => (key, value)
        }.toMap
      )
  }

  trait RecordTypeMappings extends AnyTypeMappings {
    implicit def mapRecordToTypedMap[A]: metameta.TypeMapping[Record[A], TypedMap[A]] = mapRecordToImpl
  }

  object RecordTypeMappings extends RecordTypeMappings

}
