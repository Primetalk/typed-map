package ru.primetalk.typed.ontology

import org.json4s.{JField, JObject, JValue}
import ru.primetalk.typed.ontology.meta.metameta.{AnyTypeMappings, PropertyIdTypeClass, Record, RecordRepresentation, RecordTypeClass, SchemaBuilder}
import ru.primetalk.typed.ontology.meta.SimplePropertiesMeta
import ru.primetalk.typed.ontology.meta.SimplePropertiesMeta.PropertyId
import ru.primetalk.typed.ontology.meta.metameta

import scala.language.{higherKinds, implicitConversions}
/**
  * Package `meta` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
package object json extends RecordRepresentation {

  val meta = SimplePropertiesMeta

  implicit val recordSupport: RecordTypeClass[RecordImpl, meta.PropertyIdImpl] = JObjectRecord.JObjectRecordTypeClassInstance

  type RecordImpl[A] = JObjectRecord[A]

  trait JsonConverter[B] {
    def toJson(value: B): JValue
    def fromJson(jvalue: JValue): B
  }

  case class JObjectRecord[A](jobject: JObject)

  object JObjectRecord {

//    implicit def f
    implicit def mapRecordToJObjectRecord[A]: metameta.TypeMapping[Record[A], JObjectRecord[A]] =
      metameta.typeMapping[Record[A], JObjectRecord[A]]

    implicit object JObjectRecordTypeClassInstance extends RecordTypeClass[JObjectRecord, PropertyId] {
      type PropertyHelper[A, B] = JsonConverter[B]
      class RecordWrapperImpl[A](val record: JObjectRecord[A]) extends RecordWrapper[A] {
        override def apply[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelper[A,B]): D =
          record.jobject.obj.collect{
            case (key.name, value) => helper.fromJson(value)
          }.
            head.
            asInstanceOf[D]

        override def get[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelper[A,B]): Option[D] =
          record.jobject.obj.collect{
            case (key.name, value) => helper.fromJson(value)
          }.
            headOption.
            asInstanceOf[Option[D]]

        override def updated[B, D](key: Key[B], value: D)(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelper[A,B]): JObjectRecord[A] =
          JObjectRecord(JObject((key.name, helper.toJson(value.asInstanceOf[B])) ::
            record.jobject.obj.filterNot(_._1 == key.name)))

        override def remove[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelper[A,B]): JObjectRecord[A] =
          JObjectRecord(JObject(record.jobject.obj.filterNot(_._1 == key.name)))
      }
      override def apply[A](record: JObjectRecord[A]):
        RecordWrapper[A] = new RecordWrapperImpl[A](record)

      class SchemaBuilderOps[A](schemaBuilder: SchemaBuilder[A]) extends RecordSchemaBuilderOps[A] {
        def empty: JObjectRecord[A] = JObjectRecord[A](JObject())
        def record(propValueList: PropertyValue[A, _, _]*): JObjectRecord[A] =
          JObjectRecord(JObject(
            propValueList.collect{
              case PropertyValue(key, Some(value), _, _, jsonConverter) =>
                (key.asInstanceOf[PropertyId[A,_]].name, jsonConverter.toJson(value)):JField
            }.toList
          ))

      }

      implicit def schemaBuilderOps[A](schemaBuilder: SchemaBuilder[A]): RecordSchemaBuilderOps[A] = new SchemaBuilderOps[A](schemaBuilder)

      case class PropertyValue[A, B, D](
        propertyId: PropertyId[Record[A],B],
        value: Option[D],
        bd: metameta.TypeMapping[B, D],
        r: PropertyIdTypeClass[PropertyId],
        jsonConverter: JsonConverter[D]
      )

      implicit class PropertyIdOps[A,B](propertyId: PropertyId[Record[A],B])(implicit val r: PropertyIdTypeClass[PropertyId]) {

        def :=[D](value: D)(implicit bd: metameta.TypeMapping[B, D], jsonConverter: JsonConverter[D]): PropertyValue[A, B, D] =
          PropertyValue[A, B, D](propertyId, Some(value), bd, r, jsonConverter)

        def ?=[D](value: Option[D])(implicit bd: metameta.TypeMapping[B, D], jsonConverter: JsonConverter[D]): PropertyValue[A, B, D] =
          PropertyValue[A, B, D](propertyId, value, bd, r, jsonConverter)
      }

    }

  }

  trait RecordTypeMappings extends AnyTypeMappings {
    implicit def mapRecordToJObjectRecord[A]: metameta.TypeMapping[Record[A], JObjectRecord[A]] =
      JObjectRecord.mapRecordToJObjectRecord
  }

  object RecordTypeMappings extends RecordTypeMappings

}
