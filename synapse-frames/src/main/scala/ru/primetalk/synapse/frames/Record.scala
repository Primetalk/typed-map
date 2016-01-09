package ru.primetalk.synapse.frames

/** Phantom type of a record of type L. Actual data is stored elsewhere.
  * For instance in TypedMap, or in HListShapedMap.
  * The Record itself is covariant since all properties available in parent are also available in children.
  * However, actual data container is usually invariant, because it supports updates.
  *
  * @tparam E is a phantom type of an entity.
  */
trait Record[+E]
