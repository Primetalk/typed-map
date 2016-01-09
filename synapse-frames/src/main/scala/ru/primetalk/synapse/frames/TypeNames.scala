package ru.primetalk.synapse.frames

import scala.reflect.runtime.universe._

/**
  * Created by zhizhelev on 09.01.16.
  */
trait TypeNames {
  /**
    * Returns the type name as it was constructed.
    * @param dealias default true
    * @param sorted default false
    * @tparam T the type with typeTag which name is required.
    * @return text representation of the type. It can be used as a
    */
  def typeName[T: TypeTag]
  (dealias: Boolean = false,
   sorted: Boolean = false): String = {
    def _sort(list: List[String]): List[String] =
      if (sorted) list.sorted else list

    def _dealias(tpe: Type): Type =
      if (dealias) tpe.dealias else tpe

    def typeName0(typ: Type): String = typ match {
      case t: TypeRefApi =>
        t.sym match {
          case s: TypeSymbolApi =>
            if(s.isExistential)
              "_"+(if (t.args.nonEmpty) typeName0(t.args.head) else "")
            else
              s.name.decodedName.toString + (if (t.args.nonEmpty) _sort(t.args.map(typeName0)).mkString("[", ",", "]") else "")
        }
      case t: RefinedTypeApi =>
        _sort(t.parents.map(typeName0)).mkString(" with ")
      case t: ExistentialTypeApi =>
        typeName0(t.underlying)//+" forSome {"+t.quantified.mkString(",")+"}"
    }
    typeName0(_dealias(implicitly[TypeTag[T]].tpe))
  }
}
