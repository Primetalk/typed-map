package ru.primetalk.synapse.slick.lifted

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.slf4j.LoggerFactory

import slick.ast.{FieldSymbol, ProductNode, TableExpansion}
import slick.driver.PostgresDriver
import slick.lifted
import slick.lifted.ProvenShape
import slick.memory.MemoryDriver
import slick.memory.MemoryDriver.simple._
import ru.primetalk.synapse.slick.lifted.TypedMapShape._
import ru.primetalk.synapse.map._
import ru.primetalk.synapse.map.TypedMap._

import slick.util.TreeDump

/**
 * Идея заключается в том, чтобы сформировать Shape, соответствующий схеме Instance'а.
 * Для этого надо уметь
 * - сохранять в нашей схеме метаинформации Shape, соответствующий типу поля.
 * Экземпляр Shape добавляется с помощью implicit'ов. Поэтому надо найти, откуда для типа Column[Long]
 * берётся implicit -экземпляр Shape'а. implicit'ы ищутся в том числе в Companion-объектах
 * исходного типа. В частности, для класса Column есть объект  Column, в котором подключен trait ColumnLowPriority
 * в котором, в свою очередь, находится метод columnShape, возвращающий Shape для указанного типа колонки.
 *
 * Остаётся только сохранить все Shape'ы в нашей схеме и сформировать MappedScalaProductShape на основе этих Shape'
 * -
 *
 * Также полезной является возможность сохранения Shape колонок в отдельном map'е. и извлечение соответствующей
 * метаинформации налету. Кроме того, можно конструировать Shape'ы в runtime'е при создании экземпляра EntityTable,
 * на основе исходной схемы. Потребуется просто механизм отображения атрибутов в колонки.
 *
 * создавать экземпляры Column (это не очень сложно, но требуется tag родительской таблицы). Колонки, насколько
 * можно судить, используются автоматически при формировании Shape'ов для разных запросов. Поэтому, по-видимому,
 * нельзя сформировать Shape при создании таблицы. Однако, можно сделать метод
 * implicit def propertyToColumn, который будет конвертировать свойства в колонки налету.
 * Ещё бы сделать механизм, который заданную схему сконвертирует в Shape в runtime'е. Для этого ему потребуются
 * implicit экземпляры для соответствующих типов. Если колонка - простого типа, то для неё достаточно
 * вызвать RepShape.
 *
 * При создании колонки требуется экземпляр TypedType[T] типа колонки. Его, по-видимому, можно получить путём табличной
 * конвертации домена. Но, по-видимому, можно и увязать как аргумент к методу propertyToColumn.
 * Тогда, в принципе, можно будет формировать схему внутри таблицы. Это, однако, неудобно. Нам бы хотелось иметь
 * отдельно схему.
 * Для онтологии - должно быть удобно использовать табличную конвертацию TypeTag'а в TypedType. TypeTag в онтологии сохраняется.
 * Создание map'а с помощью импортированных implicit'ов должно быть достаточно простым. Типа def add[T:TypeTag:TypedType]
 * -
 *
 * @author zhizhelev, 07.01.15.
 */
@RunWith(classOf[JUnitRunner])
class SlickTest extends FunSuite {
  val logger = LoggerFactory.getLogger("test")
  test("Slick table definition") {
    final abstract class SomeEntity
//    class MappingBuilder {
//      private
//      val buf = ListBuffer[(TypeTag[_], TypedType[_])]()
//
//      def add[T: TypedType : TypeTag] = {
//        val p: (TypeTag[_], TypedType[_]) = (implicitly[TypeTag[T]], implicitly[TypedType[T]])
//        buf += p
//      }
//
//      def toMap: Map[TypeTag[_], TypedType[_]] = buf.toMap
//    }
//    val mapping = new MappingBuilder {
//      add[Boolean]
//      add[Int]
//      add[Long]
//      add[String]
//    }.toMap
//
//    abstract class EntityTable[E](tag: Tag, tableName: String) extends Table[Instance[E]](tag, tableName) {
//      implicit def propertyToColumn[T](prop: Rel[E, T])(implicit tt: TypedType[T]): Column[T] =
//        column[T](prop.name)
//    }
//    /** Типизированный map, в котором вместо значений полей хранятся Column'ы */
//    sealed trait ColumnInstance[T]
//    case class SimpleColumnInstance[T](value: Column[T]) extends ColumnInstance[T] {
//    }
//
//    case class RecordColumnInstance[T](map: Map[String, ColumnInstance[_]]) extends ColumnInstance[T] {
//      //    lazy val map = values.toMap
//      lazy val keySet = map.keySet
//      lazy val values = map.toSeq
//
//      def get[V](rel: Relation[T, V]): ColumnInstance[V] = rel match {
//        case Rel(name) =>
//          map(name).asInstanceOf[ColumnInstance[V]]
//        case _ => throw new IllegalArgumentException(s"RecordInstance supports only named relations (aka properties), but got $rel.")
//      }
//
//    }

    object idKey extends Key[SomeEntity, Long]
    object c1Key extends Key[SomeEntity, Long]

    class MyTable(tag: Tag) extends Table[TypedMap[SomeEntity]](tag, "table") {
      def id: lifted.Rep[Long] = column[Long]("id", O.PrimaryKey)
      def c1: lifted.Rep[Long] = column[Long]("c1")

      def * = (id,c1).toProvenShape[SomeEntity](idKey, c1Key)

    }

    case class My2(id:Long, c1:Long)
    class MyTable2(tag: Tag) extends Table[My2](tag, "table2") {
      def id: lifted.Rep[Long] = column[Long]("id", O.PrimaryKey)
      def c1: lifted.Rep[Long] = column[Long]("c1")

      def * = ProvenShape.proveShapeOf( (id,c1) <> (My2.tupled,My2.unapply))
    }

    import TypedMapProjection._
    class MyTable3(tag: Tag) extends Table[TypedMap[SomeEntity]](tag, "table") {
      def id: lifted.Rep[Long] = column[Long]("id", O.PrimaryKey)
      def c1: lifted.Rep[Long] = column[Long]("c1")

      def * = (id,c1).toTypedMap[SomeEntity,(Long, Long)](idKey, c1Key)

    }


    TableExpansion
    ProductNode
    val myTable = new TableQuery[MyTable](new MyTable(_))

    println("myTable.toNode:\n"+TreeDump.get(myTable.toNode))
    println("myTable.map(_.id).toNode:\n"+TreeDump.get(myTable.map(_.id).toNode))
    println("myTable.baseTableRow.toNode:\n"+TreeDump.get(myTable.baseTableRow.toNode))
    println("myTable.baseTableRow.id.toNode:\n"+TreeDump.get(myTable.baseTableRow.id.toNode))
    println("###0")
    def selectById(id:Rep[Long]) = myTable.filter(_.id === id)
    val myTableCompiled = Compiled{selectById _ }//.compiled//{myTable}
    println("###1")
    val myTableInsert = myTable.insertInvoker
//    println("Compiled(myTable):\n"+TreeDump.get(myTableCompiled.toNode))

    println("###1.5")
    val db = Database()
    db.withSession{implicit session =>
      myTable.schema.create
//      myTable.filter(_ => true).map(_.c1).deleteIn
      val i1 = TypedMap(idKey ::= 1L, c1Key ::= 5L)
      myTableInsert += i1
      println("###2")
      val res = myTableCompiled(1L).run
      println("###3")
      val res2 = myTableCompiled(2L).run
      println("###4")
      // map(_.c1 + 10L)
//      myTable.filter(_.c1>0).run
//      assert(res === Seq(i1))
    }
//    myTable.insert(TypedMap(KeyValue(idKey,1)))
//    abstract final class ExampleData
//
//    val i: Instance[ExampleData] = new RecordInstance[ExampleData](Map())
//
//
//
//    type ShapeEx = Shape[_, _, _, _]

    //    /** Our meta information about columns. It includes the necessary information for constructing Shape.*/
    //    abstract class Meta[-E] extends PropertySeq[E] {
    //      case class PropWithSchema(p:Rel[E, _],shape:ShapeEx)
    //      private
    //      val propsList = ListBuffer[PropWithSchema]()
    //      /** A special helper method that stores Shape for the type T.*/
    //      def column[T](name:String)(implicit tt:TypeTag[T], scheme: Scheme[T]):Rel[E, T] = {
    //        val prop = property[T](name)
    //        propsList += PropWithSchema(prop, RepShape)
    //        prop
    //      }
    //
    //      def shapes:Seq[ShapeEx] = propsList.toSeq.map(_.shape)
    //    }
    //
    //
    //    A Shape implementation for Instance
    //    final class InstanceShape[Level <: ShapeLevel, E, M <: Instance[E], U <: ColumnInstance[E] : ClassTag, P <: Instance[_]](meta:Meta[E], val shapes:Seq[ShapeEx])
    //      extends MappedScalaProductShape[Level, Instance[_], M, U, P] {
    //
    //      /** shapes can contain less columns.*/
    //      def buildValue(elems: IndexedSeq[Any]) =
    //        align(elems.toList, meta)
    //      def copy(shapes: Seq[Shape[_ <: ShapeLevel, _, _, _]]) = new InstanceShape(meta, shapes)
    //
    //    }
    //
    ///    implicit def instanceShape[Level <: ShapeLevel, E](implicit s1: Meta[E]
    //                                                                         ) = new InstanceShape[Level, Instance[E], Pair[U1, U2], Pair[P1, P2]](Seq(s1, s2))
  }

  test("Slick + typed map + custom shape") {


    //    /** Column with associated key.*/
    //    class KeyColumn[T](val key:Key[T]) extends slick.lifted.Column[T]{
    //      override def toNode: Node = ???
    //    }

  }

  class UserTable(tag: Tag, tableName:String = "user") extends Table[(Long, String, Option[Int])](tag, tableName) {
    def id = column[Long]("id", O.AutoInc)

    def name = column[String]("name")

    //, O.DBType("text"))
    def ageOpt = column[Option[Int]]("age")

    def * = (id, name, ageOpt)
  }

  val exampleUsers = Seq(
    (1L, "Vasya", Some(1)),
    (2L, "Petya", Some(3))
  )
  test("Slick 3.0 for article") {

    val users = TableQuery[UserTable]
    def getUserNameQuery(id:Rep[Long]) = users.filter(_.id===id).map(_.name)
    logger.info("1")
    val getUserName = Compiled(getUserNameQuery _)
    logger.info("2")

    val db = Database()
    db.withSession { implicit session =>
      users.schema.create
      logger.info("3")
      users += (1, "Vasya", Some(1))
      logger.info("4")
      users += (2, "Petya", Some(3))
      logger.info("5")
      val all = users.run
      assert(all.size === 2)
      logger.info("5")
      val name = getUserName(1).run.head
      assert(name === "Vasya")
      logger.info("7")
      val name2 = getUserName(2).run.head
      assert(name2 === "Petya")
      logger.info("8")

      users.insertInvoker
    }

  }
  ignore("users 2") {
    val users = TableQuery[UserTable]
    val users2 = TableQuery(new UserTable(_,"user2"))
    def getUserNameQuery(id:Rep[Long]) = users.filter(_.id===id).map(_.name)
    logger.info("1")
    val getUserName = Compiled(getUserNameQuery _)
    logger.info("2")

    val db = Database()
    db.withSession { implicit session =>
      users.schema.create
      users2.schema.create

      users ++= exampleUsers
//      DBIO.seq(
//      users2.forceInsertQuery( users)
//      )
      logger.info("3")
      users += (1, "Vasya", Some(1))
      logger.info("4")
      users += (2, "Petya", Some(3))
      logger.info("5")
      val all = users.run
      assert(all.size === 2)
      logger.info("5")
      val name = getUserName(1).run.head
      assert(name === "Vasya")
      logger.info("7")
      val name2 = getUserName(2).run.head
      assert(name2 === "Petya")
      logger.info("8")

      users.insertInvoker
    }

  }
  test("MyDriver") {
    trait MyDriver extends PostgresDriver {
      override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
      override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)
      class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
        override def createPhase1 = super.createPhase1
//        ++ columns.flatMap {
//          case cb: ColumnDDLBuilder => cb.createLobTrigger(table.tableName)
//        }
        override def dropPhase1 = {
//          val dropLobs = columns.flatMap {
//            case cb: ColumnDDLBuilder => cb.dropLobTrigger(table.tableName)
//          }
//          if(dropLobs.isEmpty) super.dropPhase1
//          else Seq("delete from "+quoteIdentifier(table.tableName)) ++ dropLobs ++
            super.dropPhase1
        }
      }
    }
  }
}
