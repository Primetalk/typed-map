Typed map
=========

Typed map is a map in which keys have single type argument which is the type of value.

Have a look:

    class Key[T]
    val intKey1 = new Key[Int]
    val intKey2 = new Key[Int]
    val stringKey1 = new Key[String]
    val map = TypedMap(intKey1 ::= 10, intKey2 ::= 20, stringKey1 ::= "hello")
    val s = map(stringKey1)
    assert(s === "hello")

(AKA synapse-frames)

Typed map is a simplified version of synapse-frames. 
