package ru.primetalk.synapse.frames

import scala.reflect.runtime.universe._

/**
  * Created by zhizhelev on 09.01.16.
  */
trait Property00

/** Some property of a type L.
  * Contravariance means that whenever we need a property of a type
  * {{{
  * A &lt;: L
  * }}}
  * we can substitute any parent property.
  *
  * @tparam L entity type to which this property belongs.
  */
trait Property10[-L] extends Property00

/**
  * Property with particular value type.
  *
  * @tparam R the property value type.
  */
trait Property01[R] extends Property00 {
  def typeTag: TypeTag[R]
}

/** A universal property of a type L with value type R
  *
  * @tparam L entity type to which this property belongs.
  * @tparam R the property value type.
  */
trait Property[-L, R] extends Property10[L] with Property01[R]

/**
  * An arbitrary relation identifier. The synonym for Property.
  * TODO: Remove?
  */
trait Relation[-L, R] extends Property[L, R]

