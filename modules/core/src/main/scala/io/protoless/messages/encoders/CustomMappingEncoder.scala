package io.protoless.messages.encoders

import scala.annotation.implicitNotFound

import shapeless.HList

import io.protoless.messages.Encoder

@implicitNotFound("No CustomMappingEncoder found for type ${A} and ${L}.")
@annotation.inductive
class CustomMappingEncoder[A, L <: HList](val underlying: Encoder[A])

/**
  * Utilities for [[CustomMappingEncoder]]
  */
final object CustomMappingEncoder {

  def apply[A, L <: HList](implicit instance: CustomMappingEncoder[A, L]): CustomMappingEncoder[A, L] = instance

}
