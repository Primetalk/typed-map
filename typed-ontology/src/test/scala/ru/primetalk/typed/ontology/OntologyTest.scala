package ru.primetalk.typed.ontology

import org.scalatest.FunSuite
import ru.primetalk.typed.ontology.meta.metameta.RecordRepresentation
import meta.metameta.toRecordWrapper

/**
  * Test of various ways of handling data with typed ontology.
  */
class OntologyTest extends FunSuite {

  test("Creating records and reading values"){
    import tmap.TypedMap.TypedMapRecordTypeClassInstance
    import ru.primetalk.typed.ontology.ontology.person
    import tmap.TypedMap.TypedMapRecordTypeClassInstance.schemaBuilderOps
    import tmap.RecordTypeMappings.anyTypeMapping
    val alice = person.empty.updated(person.name, "Alice")
    assertResult(Some("Alice"))(alice.get(person.name))
  }

  test("Creating complex records and reading values"){
    import tmap.TypedMap.TypedMapRecordTypeClassInstance.PropertyIdOps
    import ru.primetalk.typed.ontology.ontology.{address, person}

    import tmap.RecordTypeMappings._
    import tmap.TypedMap.TypedMapRecordTypeClassInstance.schemaBuilderOps
    import tmap.TypedMap.TypedMapRecordTypeClassInstance
    import meta.SimplePropertiesMeta.PropertyId.PropertyIdTypeClassInstance

    val alice = person.record(
      person.name := "Alice",
      person.address := address.record(
        address.postalIndex := "12345",
        address.street := "Blueberry street, 8"
      )
    )
    assertResult(Some("Alice"))(alice.get(person.name))
    assertResult("12345")(alice(person.address).apply(address.postalIndex))
  }

  trait AliceData {
    val meta: RecordRepresentation
    import meta.RecordImpl
  }

  trait AliceDataCheck {
    val meta: RecordRepresentation
    import meta.RecordImpl
    import ru.primetalk.typed.ontology.ontology.{address, person}
    import meta.recordSupport
//    def checkAlice(alice: RecordImpl[ontology.Person]) = {
//      assertResult(Some("Alice"))(alice.get(person.name))
//      assertResult("12345")(alice(person.address).apply(address.postalIndex))
//    }
  }

  test("Test data with another data representation"){
    import json._
    import json.RecordTypeMappings.anyTypeMapping
    import json.RecordTypeMappings.mapRecordToJObjectRecord
    import json.JsonConverters._
//    import json.toRecordWrapper
//    import meta.metameta.toRecordWrapper
//    import meta.PropertyIdOps
    import ru.primetalk.typed.ontology.ontology.{address, person}
    import json.JObjectRecord.JObjectRecordTypeClassInstance.schemaBuilderOps
    import json.JObjectRecord.JObjectRecordTypeClassInstance.PropertyIdOps
    import json.JObjectRecord.mapRecordToJObjectRecord
    import json.JObjectRecord.JObjectRecordTypeClassInstance


    def alice = person.record(
      person.name := "Alice",
      person.address := address.record(
        address.postalIndex := "12345",
        address.street := "Blueberry street, 8"
      )
    )
//    import ontology.m
   }
}
