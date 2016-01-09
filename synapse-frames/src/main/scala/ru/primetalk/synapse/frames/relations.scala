///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2014                                   //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2014.
 * All rights reserved.
 * Authors: A.Zhizhelev
 *
 * Created: 02.07.14, zhizhelev
 */
package ru.primetalk.synapse.frames

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.{existentials, higherKinds, implicitConversions, reflectiveCalls}
import scala.reflect._
import scala.reflect.runtime.universe._

/**
 * Relations are named binary relations between two types.
 *
 * The types are either artificial classes that form entity hierarchy,
 * or ordinary types that usually serve as values.
 *
 * Two generic arguments allow to do complete type-checked property assignements. Having
 * left type we know which entity has the property. Having right type we know what kind of
 * property it is. Thus we can represent type checked hierarchical data structures
 * that resemble JSON.
 *
 * The actual set of properties are described with Scheme's. Every scheme describes the type T.
 * - if the type is an ordinary type, then scheme is SimpleScheme and contains only runtime type information (RTTI).
 *
 * - if we want to associate some structured data with the type, we use RecordScheme.
 *
 * - to combine two schemes (compose/aggregate) we use Tuple2Scheme which constructs scheme that describes
 * a pair of types with the schemes for either types.
 *
 * The values of types according to scheme can be represented in different forms. However, general data processing
 * can be done with the set of Instance[T] descendents.
 *
 * It is also possible to have reflection-based implementation for storing data in fields of POJO.
 */
trait BaseRelationsDefs {


  /** A single instance of type R can be traversed to
    * from  an instance of type L using the given name.
    *
    * Usually this is referred to as an attribute or a property.
    */
  case class NamedProperty[-L, R](name: String)(implicit val typeTag: TypeTag[R]) extends Relation[L, R]

  sealed trait TypedMap0 {
    def get0[T](prop: Property01[T]): Option[T]
  }

}
trait RelationsDefs extends BaseRelationsDefs{
  class Entity[L] {
    def prop[R](name: String)(implicit typeTag: TypeTag[R]) = NamedProperty[L, R](name)
  }
  /** DSL for property creation.
    * Usage:
    * {{{
    *   trait Circle // phantom type
    *   object Circle { // namespace for properties
    *     val radius = entity[Circle].prop[Int]("radius")
    *   }
    * }}}
    */
  def entity[L] = new Entity[L]

  /** A single instance of type R can be obtained
    * from an  instance of type L using index.
    *
    * Usually this is referred to as a relative identifier of a child instance.
    * */
  case class LongId[T](id: Long)(implicit val typeTag:TypeTag[T]) extends Relation[Seq[T], T]

  case class IntId[T](id: Int)(implicit val typeTag:TypeTag[T]) extends Relation[Seq[T], T]

  /**
   * A special relation between the collection and its's element.
   * Can be used when we do not know exact identifier of an object within the collection.
   * This can be the case when we add elements to a collection, or we are interested
   * in the scheme of elements.
   **/
  case class ElemRel[T]()(implicit val typeTag:TypeTag[T]) extends Relation[Seq[T], T]

  /** Special relations for Tuple2Scheme */
  case class _1[T]()(implicit val typeTag:TypeTag[T]) extends Relation[(T, _), T]

  case class _2[T]()(implicit val typeTag:TypeTag[T]) extends Relation[(_, T), T]

  /** Analogous to Seq but uses TId as a key to access instance of type T. */
  trait IndexedCollection[TId, T]

//  /** Index is a special kind of property of a collection that
//    * allows to refer to an element by it's property value.
//    */
//  case class Index[TId, T](keyProperty: Relation[T, TId]) extends Relation[Seq[T], IndexedCollection[TId, T]]
//
//  /** It's a key in the indexed collection. */
//  case class IndexValue[TId, T](value: TId) extends Relation[IndexedCollection[TId, T], T]

}


trait RelationOps extends RelationsDefs {

  /** A composition of two relations. */
  case class Rel2[-L, M, R](_1: Relation[L, M], _2: Relation[M, R]) extends Relation[L, R] {
    def typeTag:TypeTag[R] = _2.typeTag
  }

  implicit class RelEx[-L, R](r: Relation[L, R]) {
    def /[R2](r2: Relation[R, R2]): Relation[L, R2] = Rel2[L, R, R2](r, r2)
  }

//  implicit class RelEx2[L, R](r: Relation[L, R]) {
//    def index = Index[R, L](r)
//  }

}

trait SchemeDefs extends RelationsDefs with RelationOps {

  /** an aggregate of a relation with the scheme for the right part. */
  case class RelWithRScheme[-L, R](rel: NamedProperty[L, R], scheme: Scheme[R])

  sealed trait Scheme[T] {
    def typeTag: TypeTag[T]
    def classTag = ClassTag[T](typeTag.mirror.runtimeClass(typeTag.tpe))
  }

  implicit def schemeToTypeTag[T](implicit scheme:Scheme[T]):TypeTag[T] = scheme.typeTag

  case class SimpleScheme[T](implicit val typeTag: TypeTag[T]) extends Scheme[T]

  /** Scheme that describes some record with properties. The properties can only belong to
    * the type T. However, there can be different set of properties.
    * */
  case class RecordScheme[T](props: Seq[RelWithRScheme[T, _]])(implicit val typeTag: TypeTag[T]) extends Scheme[T] {
    lazy val map = props.map(p => (p.rel.name, p.scheme/*.asInstanceOf[Scheme[_]]*/)).toMap[String, Scheme[_]]
    lazy val keySet = map.keySet
  }

  sealed class Tag[Tg, T]

  /** the instance is also tagged with the tag. */
  case class TaggedScheme[Tg, T](tag: Tg, scheme: Scheme[T])(implicit tagTypeTag:TypeTag[Tg]) extends Scheme[Tag[Tg, T]] {
    def typeTag: TypeTag[Tag[Tg, T]] = { implicit val tt = scheme.typeTag; implicitly[TypeTag[Tag[Tg, T]]]}
  }

  case class CollectionScheme[T](elementScheme: Scheme[T]) extends Scheme[Seq[T]] {
    def typeTag: TypeTag[Seq[T]] = { implicit val tt = elementScheme.typeTag; implicitly[TypeTag[Seq[T]]]}
  }

//  case class Tuple2Scheme[T1, T2](_1: Scheme[T1], _2: Scheme[T2]) extends Scheme[(T1, T2)] {
//    def classTag = scala.reflect.classTag[(T1, T2)]
//  }
//
  case class AnnotatedScheme[T](scheme: Scheme[T])(annotations: Any*) extends Scheme[T] {
    def typeTag = scheme.typeTag
  }

//  case class Gen1Scheme[T, S[_]](tag: Any, scheme: Scheme[T])(implicit val classTag: ClassTag[S[T]]) extends Scheme[S[T]]
//
//  /** The user may use another way of type description. */
//  case class CustomScheme[T, CS](tag: Any, cs: CS)(implicit val classTag: ClassTag[T]) extends Scheme[T]

  /** Helper trait for constructing record schemes.
    */
  abstract class PropertySeq[L:TypeTag] {
    val typeTag: TypeTag[L] = implicitly[TypeTag[L]]
    /** NB! classTag may not exist for phantom types.*/
    lazy val classTag: ClassTag[L] = ClassTag[L](typeTag.mirror.runtimeClass(typeTag.tpe))

    private
    val propertiesBuffer = mutable.ListBuffer[RelWithRScheme[L, _]]()

    protected
    def property[R](name: String)(implicit scheme: Scheme[R]) = {
      val r = NamedProperty[L, R](name)
      propertiesBuffer += RelWithRScheme(r, scheme)
      r
    }

    protected
    def simpleProperty[R](name: String)(implicit typeTag: TypeTag[R]) =
      property[R](name)(SimpleScheme[R]())


    protected
    def importProperties[L2 >: L](scheme: RecordScheme[L2]) {
      propertiesBuffer ++= scheme.props
    }

    def toScheme =
      RecordScheme[L](propertiesBuffer.toSeq)

  }

  implicit def propertySeqToScheme[L](ps: PropertySeq[L]): Scheme[L] =
    ps.toScheme
}

trait PropertyValueDefs extends RelationsDefs {
  sealed trait PropertyValue10[-L] {
    def prop: Property10[L]
    def value: Any
  }
  case class PropertyValue[-L,T](prop:Property[L,T], value:T) extends PropertyValue10[L]
  /** PropertyValue DSL.
    * {{{
    *   width := 10
    * }}}
    */
  implicit class PropertyEx[-L,T](prop:Property[L,T]){
    def :=(value:T) = Some(PropertyValue(prop,value))
    def ?=(valueOpt:Option[T]) = valueOpt.map(PropertyValue(prop,_))
  }

}
/** Shape - some way to describe the structure of a value.*/
trait ShapeDefs {

  /**
    * Marker trait to distinguish different Shape types via Shape type-classes.
    */
  trait ShapeKind
  /**
    * The structure of a value V.
    */
  trait Shape[V] extends ShapeKind {
    type ValueType = V
    def :=(value:V) = ShapedValue[V, this.type](this,value)
  }

  /** Value with shape.
    *
    * @param shape the structure of the value
    * @param value actual value
    * @tparam V the type of values.
    */
  case class ShapedValue[V, S<:Shape[V]](shape:S,value:V) {
    def get[T](prop:Property01[T])(implicit shapeGetter: ShapeGetter[S]):Option[T] =
      shapeGetter.get(shape)(prop)(value)
  }

  trait ShapeGetter[S<:ShapeKind] {
    def get[V,T](shape:S)(prop:Property01[T])(value:V):Option[T]
  }
  /**
    * A kind of a reflection on the shape structure.
    * A type-class for different Shape kinds.
    * This type-class defines some higher-order operations on values that all shapes should implement.
    * @tparam S the Shape kind.
    */
  trait ShapeReflection[S<:ShapeKind] {
    def properties(shape:S):Seq[Property00]
  }

}
trait SeqShapeDef extends ShapeDefs {

  /** A simple Seq-based shape. Available properties are listed in
    * a sequence and the value is the sequence of property values. If
    * a property doesn't have a value (None), then it simply missed from the list of properties.
    * @param properties the list of properties
    */
  case class SeqShape(properties:Seq[Property00]) extends Shape[Seq[Any]]


  implicit object SeqShapeTypeClass extends ShapeReflection[SeqShape] with ShapeGetter[SeqShape] {
    override def properties(shape: SeqShape): Seq[Property00] = shape.properties
    override def get[V,T](shape: SeqShape)(prop: Property01[T])(value: V): Option[T] = {
      val index = shape.properties.indexOf(prop)
      if (index == -1) None else Option(value.asInstanceOf[Seq[Any]](index)).asInstanceOf[Option[T]]
    }
  }

}
/** Core type for storing data - Record[L].*/
trait TypedMap2Defs extends PropertyValueDefs with SeqShapeDef {

  sealed trait TypedMap2[L] {
    def props:Seq[Property10[L]]
    def values:Seq[Any]
  }
  case class TypedMap2Impl[L](props:Seq[Property10[L]], values:Seq[Any]) extends TypedMap2[L] {
    def get0[T](prop:Property01[T]):Option[T] = {
      val index = props.indexOf(prop)
      if (index == -1) None else Option(values(index)).asInstanceOf[Option[T]]
    }

    def get[T](prop:Property[L,T]):Option[T] = get0(prop)

    def :+[T](optPropValue:Option[PropertyValue[L,T]]):TypedMap2[L] = optPropValue match {
      case None => this
      case Some(PropertyValue(prop, value)) => TypedMap2Impl[L](props :+ prop, values :+ value)
    }
    def :++[L2>:L](other:TypedMap2[L2]):TypedMap2[L] = TypedMap2Impl(props ++ other.props, values ++ other.values)
  }
  def empty[L]:TypedMap2[L] = TypedMap2Impl[L](Seq(),Seq())
  def record[L](propValues:Option[PropertyValue10[L]]*):TypedMap2[L] = {
    val flat = propValues.flatten
    TypedMap2Impl[L](flat.map(_.prop), flat.map(_.value))
  }
}
/** Representation of HList-based schema of a record. We call it a Shape (similar to Slick library).
  */
trait HListRelDefs extends PropertyValueDefs with TypedMap2Defs {
  import shapeless._

  case class RecordValue[L,V<:HList, R <: HListRecordShape[L,V]](shape:R, value:V)

  private
  object HListRecordShapeTypeClass extends ShapeGetter[HListRecordShape0] with ShapeReflection[HListRecordShape0] {
    override def get[V,T](shape:HListRecordShape0)(prop:Property01[T])(value:V):Option[T]= {
//    def get[T](prop:Property[L,T]):Option[T] = {
      import shapeless.HList.ListCompat._
      def get0(shape:Any, value:Any):Option[T] = shape match {
        case ConsRShape(prop1, tail) =>
          val head #: vtail = value
          if(prop1 == prop)
            head.asInstanceOf[Option[T]]
          else
            get0(tail, vtail)
        case MConsRShape(prop1, tail) =>
          val head #: vtail = value
          if(prop1 == prop)
            Some(head).asInstanceOf[Option[T]]
          else
            get0(tail, vtail)
        case NilRShape() =>
          None
      }
      get0(shape, value)
    }

    override def properties(shape: HListRecordShape0): Seq[Property00] = {
      def properties0(shape2:HListRecordShape0, res:ListBuffer[Property00]): Seq[Property00] = shape2 match {
        case NilRShape() => res.toSeq
        case ConsRShape(prop1, tail) =>
          properties0(tail, res += prop1)
        case MConsRShape(prop1, tail) =>
          properties0(tail, res += prop1)
      }
      properties0(shape, ListBuffer())
    }
  }
  implicit def hListRecordShape[S<: HListRecordShape0]:ShapeGetter[S] with ShapeReflection[S] =
    HListRecordShapeTypeClass.asInstanceOf[ShapeGetter[S] with ShapeReflection[S]]

  sealed trait HListRecordShape0 extends ShapeKind {
    type LType
    type VType <: HList
  }
  sealed trait HListRecordShape[L,V<:HList] extends HListRecordShape0 with Shape[V] {
    type LType = L
    type VType = V
//    def :=(value:VType) = RecordValue[L, V, this.type](this, value)
  }

  case class NilRShape[L]() extends HListRecordShape[L, HNil]
//  {
//    override def get[T](prop: Property01[T])(value: HNil): Option[T] = None
//  }

  /** Optional Cons for properties.*/
  case class ConsRShape[T,R <: HListRecordShape0](prop:Property[R#LType,T], tail:R) extends HListRecordShape[R#LType,Option[T]::R#VType]
  /** Mandatory Cons for properties. */
  case class MConsRShape[T,R <: HListRecordShape0](prop:Property[R#LType,T], tail:R) extends HListRecordShape[R#LType,T::R#VType]

  def rnil[L] = NilRShape[L]()

  /** Simple DSL for constructing RecordShape.*/
  implicit class HListRecordShapeOps[R<:HListRecordShape0](rs:R){
    def :?:[T](prop:Property[R#LType,T]) = ConsRShape[T,R](prop, rs)
    def :!:[T](prop:Property[R#LType,T]) = MConsRShape[T,R](prop, rs)
  }

}

trait InstanceDefs extends SchemeDefs {

  /** An instance that is associated with the type T. */
  sealed trait Instance[T] {
    type SchemeType <: Scheme[T]
  }

  case class SimpleInstance[T](value: T) extends Instance[T] {
    type SchemeType = SimpleScheme[T]
  }

  case class RecordInstance[T](map: Map[String, Instance[_]]) extends Instance[T] {
    type SchemeType = RecordScheme[T]
    //    lazy val map = values.toMap
    lazy val keySet = map.keySet
    lazy val values = map.toSeq

    def get[V](rel: Relation[T, V]): Instance[V] = rel match {
      case NamedProperty(name) =>
        map(name).asInstanceOf[Instance[V]]
      case _ =>
        throw new IllegalArgumentException(s"$rel is not supported.")
    }

  }

  case class CollectionInstance[T](values: Seq[Instance[T]]) extends Instance[Seq[T]] {
    type SchemeType = CollectionScheme[T]
  }

//  case class Tuple2Instance[T1, T2](value: (Instance[T1], Instance[T2])) extends Instance[(T1, T2)] {
//    type SchemeType = Tuple2Scheme[T1, T2]
//  }

  case class TaggedInstance[Tg, T](tag: Tg, value: Instance[T]) extends Instance[Tag[Tg, T]] {
    type SchemeType = TaggedScheme[Tg, T]
  }

  case class AnnotatedInstance[T](value: Instance[T]) extends Instance[T] {
    type SchemeType = AnnotatedScheme[T]
  }

//  case class Gen1Instance[T, S[_]](tag: Any, value: Instance[T]) extends Instance[S[T]] {
//    type SchemeType = Gen1Scheme[T, S]
//  }
//
//  case class CustomInstance[T, CS](tag: Any, value: Any) extends Instance[T] {
//    type SchemeType = CustomScheme[T, CS]
//  }

  case class InstanceWithMeta[T](i: Instance[T], s: Scheme[T])

}

trait SimpleOperationsDefs extends InstanceDefs {
  // NB! this implicit leads to poor behavior. It tries to convert everything to simpleInstance. Do not remove this comment
  //  implicit def toInstance[T](value: T)(implicit simpleScheme: SimpleScheme[T]) = SimpleInstance[T](value)

  implicit def toExistentialRelWithRScheme[T, T2](rel: NamedProperty[T, T2])(implicit scheme: Scheme[T2]): RelWithRScheme[T, _] = RelWithRScheme(rel, scheme)

  def simpleRel[T, T2](rel: NamedProperty[T, T2])(implicit typeTag: TypeTag[T2]): RelWithRScheme[T, T2] = RelWithRScheme(rel, SimpleScheme[T2]())

  def simpleScheme[T <: AnyVal](implicit typeTag: TypeTag[T]) = SimpleScheme[T]()

  implicit def intToIntId[T](id: Int)(implicit typeTag: TypeTag[T]):IntId[T] = IntId[T](id)

  def navigate[E, T](i: Instance[E], path: Relation[E, T]): Instance[T] =
    (
      (i, path) match {
        case (_, Rel2(_1, _2)) => navigate(navigate(i, _1), _2)
        case (r: RecordInstance[_], p: NamedProperty[_, _]) => r.map(p.name)
        case (c: CollectionInstance[_], IntId(id)) => c.values(id)
        case _ => throw new IllegalArgumentException(s"Couldn't navigate from $i by $path")
      }
      ).asInstanceOf[Instance[T]]

  def update[E, T](i: Instance[E], path: Relation[E, T], value: Instance[T]): Instance[E] =
    (
      (i, path) match {
        case (_, Rel2(_1, _2)) => update(i, _1, update(navigate(i, _1), _2, value))
        case (r: RecordInstance[_], NamedProperty(name)) =>
          RecordInstance(i.asInstanceOf[RecordInstance[Any]].map.updated(name, value))
        case (c: CollectionInstance[_], IntId(id)) => CollectionInstance(c.values.asInstanceOf[Seq[Instance[Any]]].updated(id, value))
        case _ => throw new IllegalArgumentException(s"Couldn't navigate from $i by $path")
      }
      ).asInstanceOf[Instance[E]]

  implicit class InstanceOps[T](i: Instance[T]) {
    def set[T2](prop: Relation[T, T2], value: Instance[T2]): Instance[T] =
      update(i, prop, value)

    def get[T2](prop: Relation[T, T2]): Instance[T2] =
      navigate(i, prop)
  }

  //  implicit class RecordInstanceOps[T](i: RecordInstance[T]) {
  //    def set[T2, Anc >: T](prop: Relation[Anc, T2], value: Instance[T2]): RecordInstance[T] = prop match {
  //      case Rel(name) =>
  //        i.copy(i.map + ((name, value)))
  //      case Rel2(r1, r2) =>
  //        val i1 = navigate(i, r1)
  //        val i1changed = new RecordInstanceOps(i1).set(r2, value)
  //        set(r1, i1changed)
  //    }
  //
  //    def set[T2, Anc >: T](prop: Rel[Anc, T2], value: Instance[T2]): RecordInstance[T] =
  //      i.copy(i.map + ((prop.name, value)))
  //
  //  }

  def simple[T](value: T) = SimpleInstance(value)

  def simpify[T](i: Instance[T]) = i match {
    case SimpleInstance(value) =>
      value.asInstanceOf[T]
    case _ => throw new IllegalArgumentException(s"$i cannot be simplified")
  }

  def emptyInstance[T](implicit typeTag: TypeTag[T]): RecordInstance[T] = RecordInstance[T](Map())

  def record[T](props: RelWithRScheme[T, _]*)(implicit typeTag: TypeTag[T]) = RecordScheme[T](props.toSeq)

  def seq[T](values: Instance[T]*): CollectionInstance[T] = CollectionInstance[T](values.toSeq)

  class Builder[T](scheme: RecordScheme[T]) {
    private
    val map = mutable.Map[String, Instance[_]]()

    def set[T2, Anc >: T](prop: NamedProperty[Anc, T2], value: Instance[T2]) = {
      if (scheme.map.keySet.contains(prop.name))
        map(prop.name) = value
      else
        throw new IllegalArgumentException(s"Scheme $scheme doesn't contain ${prop.name}")
      this
    }

    def fillFromInstance(i: RecordInstance[T]) = {
      val schemeNames = scheme.map.keySet
      for {
        propName <- i.keySet
        if schemeNames.contains(propName)
        value = i.map(propName)
      }
        map(propName) = value
      this
    }

    def toInstance = {
      val diff = scheme.map.keySet -- map.keySet
      if (diff.isEmpty)
        RecordInstance[T](map.toMap)
      else
        throw new IllegalArgumentException(s"The builder doesn't yet contain the following properties: $diff")
    }
  }

}

/** operations with scheme instances. */
trait OperationsDefs extends SimpleOperationsDefs {

  implicit class RecordSchemeEx[T](scheme: RecordScheme[T]) {
    def hasAllProperties(i: Instance[T]) = i match {
      case r: RecordInstance[_] => scheme.props.map(_.rel.name).forall(r.keySet.contains)
      case _ => throw new IllegalArgumentException(s"only RecordInstance is supported. Given $i")
    }

    def ++[T2 >: T](other: RecordScheme[T2]): RecordScheme[T] =
      RecordScheme(scheme.props ++ other.props)(scheme.typeTag)

  }

  def isMatching[T](i: Instance[T], scheme: Scheme[T]) =
    unmatches(i, scheme).isEmpty

  def nonMatching[T](i: Instance[T], scheme: Scheme[T]) =
    unmatches(i, scheme).nonEmpty

  /** Checks match and returns Seq() if matches. Otherwise returns the list of non-matching
    * elements. */
  def unmatches[T](i: Instance[T], scheme: Scheme[T]): Seq[InstanceWithMeta[_]] = {
    def unmatches0(p: InstanceWithMeta[_]): Seq[InstanceWithMeta[_]] = p match {
      case InstanceWithMeta(i: SimpleInstance[_], s: SimpleScheme[_]) =>

        if (s.classTag.runtimeClass.isPrimitive //TODO: implement for primitive types
          || s.classTag.runtimeClass.isAssignableFrom(i.value.getClass))
          Seq()
        else
          Seq(p)
      case InstanceWithMeta(r@RecordInstance(_), s@RecordScheme(props)) =>
        if ((s.keySet -- r.keySet).isEmpty)
          for {
            v <- r.values
            if s.map.contains(v._1)
            iwm = InstanceWithMeta(v._2.asInstanceOf[Instance[Any]], s.map(v._1).asInstanceOf[Scheme[Any]])
            matchResult <- unmatches0(iwm)
          } yield matchResult
        else
          Seq(p)
      case InstanceWithMeta(i0, AnnotatedScheme(s)) =>
        unmatches0(InstanceWithMeta(i0, s))
      case _ =>
        throw new IllegalArgumentException("isMatching is not implemented for " + p)
    }
    unmatches0(InstanceWithMeta(i, scheme))
  }


  /** Aligns raw tuple (list of any) with the scheme. Every data element
    * is attached to apropriate property of the scheme. */
  def align[T](data: List[Any], scheme: Scheme[T]): Instance[T] = {
    def align0(data: List[Any], scheme: Scheme[_]): (Instance[T], List[Any]) = scheme match {
      case s: SimpleScheme[_] =>
        (SimpleInstance(data.head.asInstanceOf[T]), data.tail)
      case RecordScheme(propSeq) =>
        def align1(data: List[Any],
                   props: List[RelWithRScheme[_, _]],
                   res: List[(String, Instance[T])]): (List[(String, Instance[_])], List[Any]) =
          props match {
            case Nil => (res.reverse, data)
            case RelWithRScheme(rel, scheme1) :: ptail =>
              val (prop, rest) = align0(data, scheme1)
              align1(rest, ptail, (rel.name, prop) :: res)
            case msg :: _ => throw new IllegalArgumentException(s"Alignment is not implemented for $msg")
          }
        val (props, tail) = align1(data, propSeq.toList, Nil)
        (RecordInstance(props.toMap), tail)
      case AnnotatedScheme(s) =>
        align0(data, s)
      case _ => throw new IllegalArgumentException(s"Alignment is not implemented for $scheme")
    }
    val (res, tail) = align0(data, scheme)
    if (tail.nonEmpty)
      throw new IllegalArgumentException(s"$data cannot be aligned to $scheme completely")
    res
  }

  /** Converts scheme-based instance to a raw tuple (list of any).
    */
  def unalign[T](data: InstanceWithMeta[T]): List[Any] = data match {
    case InstanceWithMeta(SimpleInstance(v), _) =>
      List(v)
    case InstanceWithMeta(i@RecordInstance(values), RecordScheme(propSeq)) =>
      propSeq.toList.flatMap { case RelWithRScheme(rel, s) =>
        val value = i.get(rel)
        unalign(InstanceWithMeta(value, s))
      }
    case InstanceWithMeta(i, AnnotatedScheme(s)) =>
      unalign(InstanceWithMeta(i, s))
    case _ =>
      throw new IllegalArgumentException(s"Unalignment is not implemented for $data")
  }

  def flatten[T](data: InstanceWithMeta[T]): List[Any] = unalign(data)
}

trait Navigation extends InstanceDefs {

  implicit class SchemeEx[T](scheme: Scheme[T]) {
    def /[T2, Anc >: T](prop: NamedProperty[Anc, T2]): Scheme[T2] = scheme match {
      case r: RecordScheme[T] => r.map(prop.name).asInstanceOf[Scheme[T2]]
      case _ => throw new IllegalArgumentException(s"cannot proceed hierachically with other schemes except RecordScheme or CollectionScheme: $scheme ")
    }

  }

  sealed trait SpecialProperties

  object Element extends SpecialProperties

  implicit class SchemeEx2[T](scheme: Scheme[Seq[T]]) {

    def /(e: Element.type): Scheme[T] = scheme match {
      case cs: CollectionScheme[T] =>
        cs.elementScheme//.asInstanceOf[Scheme[T]]
      case _ =>
        throw new IllegalArgumentException(s"Cannot proceed hierarchically with other schemes except CollectionScheme: $scheme ")
    }

  }

}

object relations
  extends RelationsDefs
  with SchemeDefs
  with InstanceDefs
  with Navigation
  with OperationsDefs
  with TypeNames
  with HListRelDefs
