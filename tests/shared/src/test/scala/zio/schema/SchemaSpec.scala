package zio.schema

import scala.annotation.nowarn
import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.CaseSet._
import zio.schema.SchemaAssertions.hasSameSchema
import zio.schema.validation._
import zio.schema.annotation.ValidationAnnotation
import zio.test.Assertion._
import zio.test._

object SchemaSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] = suite("Schema Spec")(
    suite("Should have valid equals")(
      test("primitive") {
        assert(schemaUnit)(hasSameSchema(schemaUnit))
      },
      test("sequence") {
        assert(Schema.chunk(schemaUnit))(hasSameSchema(Schema.chunk(schemaUnit)))
      } @@ TestAspect.scala2Only,
      test("tuple") {
        assert(Schema.Tuple2(schemaUnit, schemaUnit))(hasSameSchema(Schema.Tuple2(schemaUnit, schemaUnit))) &&
        assert(Schema.Tuple2(schemaTransform, schemaTransform))(
          hasSameSchema(Schema.Tuple2(schemaTransform, schemaTransform))
        )
      },
      test("record") {
        assert(schemaRecord("key"))(hasSameSchema(schemaRecord("key"))) &&
        assert(schemaRecord("key1"))(not(hasSameSchema(schemaRecord("key2"))))
      },
      test("transform") {
        assert(schemaTransform)(hasSameSchema(schemaTransform)) &&
        assert(schemaTransformMethod)(hasSameSchema(schemaTransformMethod))
      } @@ TestAspect.scala2Only,
      test("optional") {
        assert(Schema.Optional(schemaUnit))(hasSameSchema(Schema.Optional(schemaUnit)))
      },
      test("enumeration") {
        assert(schemaEnum("key"))(hasSameSchema(schemaEnum("key"))) &&
        assert(schemaEnum("key1"))(not(hasSameSchema(schemaEnum("key2"))))

      } @@ TestAspect.scala2Only
    ),
    test("Tuple.toRecord should preserve annotations") {
      val left        = Schema.primitive(StandardType.StringType)
      val right       = Schema.primitive(StandardType.StringType)
      val tupleSchema = Schema.Tuple2(left, right, Chunk("some Annotation"))
      val record      = tupleSchema.toRecord
      assert(record.annotations)(hasFirst(equalTo("some Annotation")))
    },
    suite("Schema validation")(
      test("validate method should attach validation to schema") {
        val schema              = Schema[String]
        val minLengthValidation = Validation.minLength(3)
        val validatedSchema     = schema.validate(minLengthValidation)
        assert(validatedSchema.annotations)(contains(ValidationAnnotation(minLengthValidation)))
      },
      test("validate should return an error if value does not satisfy validation") {
        val schema      = Schema[String].validate(Validation.minLength(3))
        val validValue  = "abc"
        val validResult = Schema.validate(validValue)(schema)
        assert(validResult)(isEmpty) && // No errors for valid input
        {
          val invalidValue  = "ab"
          val invalidResult = Schema.validate(invalidValue)(schema)
          assert(invalidResult)(hasSize(equalTo(1)))
        }
      },
      test("validate should handle multiple validations") {
        val schema = Schema[String]
          .validate(Validation.minLength(3))
          .validate(Validation.pattern("^[a-z]+$"))

        val validValue  = "abc"
        val validResult = Schema.validate(validValue)(schema)
        assert(validResult)(isEmpty) && {
          val invalidValue  = "ab1"
          val invalidResult = Schema.validate(invalidValue)(schema)
          assert(invalidResult)(hasSize(equalTo(1))) // One error for pattern mismatch
        }
      }
    )
  )
  def schemaUnit: Schema[Unit] = Schema[Unit]
  def schemaInt: Schema[Int]   = Schema[Int]

  def schemaRecord(key: String): Schema[ListMap[String, _]] =
    Schema.record(
      TypeId.Structural,
      Schema.Field(
        key,
        schemaUnit.asInstanceOf[Schema[Any]],
        get0 = (p: ListMap[String, _]) => p(key),
        set0 = (p: ListMap[String, _], v: Any) => p.updated(key, v)
      )
    )

  @nowarn def schemaEnum(key: String): Schema[Any] =
    Schema.enumeration[Any, CaseSet.Aux[Any]](
      TypeId.Structural,
      caseOf[Unit, Any](key)(_ => ())(_.asInstanceOf[CaseSet.Aux[Any]])(
        _.isInstanceOf[Unit]
      )
    )

  val f: Unit => Either[String, Int] = _ => Right(0)
  val g: Int => Either[String, Unit] = _ => Right(())
  def schemaTransform: Schema[Int]   = schemaUnit.transformOrFail[Int](f, g)

  def tranformF(u: Unit): Either[String, Int] = Some(u).map(_ => 0).toRight("")
  def tranformG(i: Int): Either[String, Unit] = Some(i).map(_ => ()).toRight("")
  def schemaTransformMethod: Schema[Int]      = schemaUnit.transformOrFail(tranformF, tranformG)

}
