package zio.schema.annotation

import zio.schema.validation.Validation

/**
 * An annotation for attaching validation to a schema.
 *
 * @param validation the validation rule to attach to the schema
 */
final case class ValidationAnnotation[A](validation: Validation[A]) extends scala.annotation.StaticAnnotation
