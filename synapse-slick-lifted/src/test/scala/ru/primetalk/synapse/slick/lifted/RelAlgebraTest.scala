package ru.primetalk.synapse.slick.lifted

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.implicitConversions

import scala.reflect.runtime.universe._
/**
 * Есть мысль - представить каждое отношение, состоящее даже из единственного атрибута в виде отдельного trait'а. Тогда
 * по идее, будет легко сформировать объединение (join) - путём соединения типов по принципу with.
 * проекцию, теоретически, тоже можно сформировать - достаточно привести к родительскому подмножеству trait'ов.
 * переименование атрибутов (может потребоваться для объединения нескольких отношений) можно сделать путём
 * создания новых trait'ов с новыми именами, реализация которых ссылается на переименовываемые.
 * https://ru.wikipedia.org/wiki/Алгебра_Кодда
 * В этом тесте попробуем проверить эту идею.
 * @author zhizhelev, 08.01.15.
 */
@RunWith(classOf[JUnitRunner])
class RelAlgebraTest extends FunSuite {
  test("Relational algebra test"){

    /** Произвольное реляционное отношение.*/
    trait Relation {}
    trait RelUnit extends Relation
    /** Отношение, состоящее из одного атрибута.*/
    trait SingleAttributeRelation extends Relation


    sealed trait Attribute[T]{
      def name:String
      type Type = T
      def typeTag:TypeTag[Type]

      /** Тип отношения, соответствующий этому атрибуту. Используется для формирования типа отношения, состоящего из единственного атрибута */
      trait RInstance extends SingleAttributeRelation
      type R = RInstance

    }
    abstract class TableAttribute[T](val name:String)(implicit val typeTag:TypeTag[T]) extends Attribute[T]
    /** Используется абстрактный класс, чтобы внутри экземпляра класса был сформирован правильный тип отношения, не совпадающий с типом исходного атрибута.*/
    abstract class RenamedAttribute[T, A<:Attribute[T]](val name:String, val originalAttribute:A) extends Attribute[T]{ //self:Attribute[A#Type]=>
      def typeTag:TypeTag[A#Type] = originalAttribute.typeTag
    }
//    def rename[A<:Attribute[_]](a:A) = new RenamedAttribute[A#Type, A](a.name+ "Renamed", a){}

    object Id1 extends TableAttribute[Long]("id1")

    object Column1 extends TableAttribute[Long]("Column1")

    object Column2 extends TableAttribute[Long]("Column2")

//    val Column1Renamed = new RenamedAttribute("Column1Renamed", Column1){}

    /** Для выполнения операций над типами нам необходим носитель runtime-информации
      * об этих типах. В частности, в ходе любой реляционной операции, меняющей тип отношения,
      * создаётся новый экземпляр носителя, который на этапе компиляции сохраняет тип, а
      * в runtime'е ав
      *
      **/
    sealed trait RelationHeader[R_ <:Relation]{ type R = R_ }

    case class SingleAttribute[A<:Attribute[_]](a:A) extends RelationHeader[A#R] {
      type Type = A#Type
    }

    case object EmptyRelation extends RelationHeader[RelUnit]
    /**
     * Построение таблицы путём объединения нескольких отношений, в т.ч., состоящих из отдельных колонок.
     * Отделено от обычного Join'а.
     */
    case class TableJoin[R1<:Relation, R2<:Relation](r1:RelationHeader[R1], r2:RelationHeader[R2]) extends RelationHeader[R1 with R2]


  /** Стандартная операция над отношениями - декартово произведение.*/
    case class Times[R1<:Relation, R2<:Relation](r1:RelationHeader[R1], r2:RelationHeader[R2]) extends RelationHeader[R1 with R2]

    case class Projection[R1<:Relation, R2<:R1](r1:RelationHeader[R1], r2:RelationHeader[R2]) extends RelationHeader[R2]

    sealed trait Row[R<:Relation] {
      def current:R
      def get[A<:Attribute[_]](a:A)(implicit ev:R<:<A#R):A#Type
    }

    implicit def toSingleAttribute[A<:Attribute[_]](a:A):SingleAttribute[A] = SingleAttribute(a)

    type Table1Rel = Id1.R with Column1.R

    def join[R1<:Relation, R2<:Relation](r1:RelationHeader[R1], r2:RelationHeader[R2]):RelationHeader[R1 with R2] = Times[R1, R2](r1,r2)
    def projection[R1<:Relation, R2<:R1](r1:RelationHeader[R1], r2:RelationHeader[R2]):RelationHeader[R2] = Projection[R1, R2](r1,r2)

    implicit class RelationOps[R<:Relation](r:RelationHeader[R]){
      def **[R2<:Relation](r2:RelationHeader[R2]):RelationHeader[R with R2] =
        TableJoin[R,R2](r,r2)
    }

    /** Отношение, состоящее из двух колонок.*/
    val myTable = EmptyRelation ** Id1 ** Column1

    def handle(data:Row[myTable.R]) = {
      val id = data.get(Id1)
      val c1 = data.get(Column1)

//      val c2 = data.get(Column2)// ошибка компиляции
      //      if(c1 ==c2) throw new IllegalArgumentException("c1==c2")
    }

//
//    trait Table1 extends Id with Column1
//      def join[A2<:AttributeName, T2<: HList, R2<:Relation[A2, T2]](r2:R2):Relation[A with A2, T#:::[T2]] = ???
//    object RNil extends Relation[Nothing, HNil]
//    val relation1:Id.Name with Column1.Name
  }
}
