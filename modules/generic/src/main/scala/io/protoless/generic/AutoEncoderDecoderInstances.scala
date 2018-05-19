package io.protoless.generic

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import shapeless.{Generic, HList, Nat}

import io.protoless.messages.Decoder.Result
import io.protoless.messages.decoders.{AutoDecoder, IncrementalDecoder}
import io.protoless.messages.encoders.{AutoEncoder, IncrementalEncoder}

trait AutoEncoderDecoderInstances {

  implicit def decodeAuto[A, R <: HList](implicit
    gen: Generic.Aux[A, R],
    decoder: IncrementalDecoder[R, Nat._1]
  ): AutoDecoder[A] = new AutoDecoder[A] {
    override def decode(input: CodedInputStream): Result[A] = {
      decoder.underlying.decode(input) match {
        case Right(repr) => Right(gen.from(repr))
        case l @ Left(_) => l.asInstanceOf[Result[A]]
      }
    }
  }

  implicit def encodeAuto[A, R <: HList](implicit
    gen: Generic.Aux[A, R],
    encoder: IncrementalEncoder[R, Nat._1]
  ): AutoEncoder[A] = new AutoEncoder[A] {
    override def encode(a: A, output: CodedOutputStream): Unit = {
      encoder.underlying.encode(gen.to(a), output)
      output.flush()
    }
  }

}
