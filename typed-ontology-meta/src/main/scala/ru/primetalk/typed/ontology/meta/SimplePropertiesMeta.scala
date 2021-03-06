package ru.primetalk.typed.ontology.meta

import ru.primetalk.typed.ontology.meta.metameta.{Meta, PropertyIdTypeClass, Record, RecordTypeId}

import scala.language.higherKinds

/**
  * Package `meta` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
object SimplePropertiesMeta extends Meta {

  type PropertyIdImpl[-A,B] = PropertyId[A,B]
  /**
    * Metainformation about property.
    * Contains unique name (within the type) and type of the value.
    * Might contain other metainformation about property, like Schema.
    */
  case class PropertyId[-A,B](name: String)

  implicit def propertyIdTypeClassInstance: PropertyIdTypeClass[PropertyIdImpl] = PropertyId.PropertyIdTypeClassInstance

  object PropertyId {
    implicit object PropertyIdTypeClassInstance extends PropertyIdTypeClass[PropertyId] {
      override def name[A, B](p: PropertyId[A, B]): String = p.name
    }
  }

  def property[B](implicit name: sourcecode.Name, r: RecordTypeId): PropertyId[Record[r.RecordType],B] =
    PropertyId(name.value)
}
