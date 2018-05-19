package io.protoless.generic

import shapeless.Nat

import io.protoless.messages.{Encoder, Decoder}
import io.protoless.messages.encoders.IncrementalEncoder
import io.protoless.messages.decoders.IncrementalDecoder

/**
  * Internal class allowing to restrict automatic derivation of type `A`.
  *
  * [[SemiAutoEncoder]] can only be retrieved with `io.protoless.generic.semiauto.deriveEncoder`.
  */
private[protoless] class SemiAutoEncoder[A](val underlying: Encoder[A])
private[protoless] class SemiAutoDecoder[A](val underlying: Decoder[A])

private[protoless] trait SemiAutoEncoderDecoderInstances extends IncrementalEncoderDecoderInstances {

  implicit def encodeSemiAutoInstance[A](implicit encoder: IncrementalEncoder[A, Nat._1]): SemiAutoEncoder[A] = {
    new SemiAutoEncoder[A](encoder)
  }

  implicit def decodeSemiAutoInstance[A](implicit decoder: IncrementalDecoder[A, Nat._1]): SemiAutoDecoder[A] = {
    new SemiAutoDecoder[A](decoder)
  }
}
