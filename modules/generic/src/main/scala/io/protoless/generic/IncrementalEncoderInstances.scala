package io.protoless.generic

import com.google.protobuf.CodedOutputStream
import com.google.protobuf.CodedInputStream
import shapeless.{::, Generic, HList, HNil, Nat, Succ}
import shapeless.ops.nat.ToInt

import io.protoless.messages.encoders.IncrementalEncoder
import io.protoless.fields.FieldEncoder
import io.protoless.messages.Decoder.Result
import io.protoless.messages.decoders.IncrementalDecoder
import io.protoless.fields.FieldDecoder

trait IncrementalEncoderDecoderInstances extends CustomMappingEncoderDecoderInstances {

  implicit def encodeIncrementalHNil[N <: Nat]: IncrementalEncoder[HNil, N] = new IncrementalEncoder[HNil, N] {
    override def encode(a: HNil, output: CodedOutputStream): Unit = {}
  }

  implicit def encodeIncrementalHList[H, T <: HList, N <: Nat](implicit
    hEncoder: FieldEncoder[H],
    index: ToInt[N],
    tEncoder: IncrementalEncoder[T, Succ[N]]
  ): IncrementalEncoder[H :: T, N] = new IncrementalEncoder[H :: T, N] {
    override def encode(a: H :: T, output: CodedOutputStream): Unit = {
      val (h :: t) = a
      hEncoder.write(index(), h, output)
      tEncoder.encode(t, output)
    }
  }

  implicit def encodeIncremental[A, N <: Nat, R <: HList](implicit
    gen: Generic.Aux[A, R],
    encoder: IncrementalEncoder[R, N]
  ): IncrementalEncoder[A, N] = new IncrementalEncoder[A, N] {
    override def encode(a: A, output: CodedOutputStream): Unit = {
      encoder.encode(gen.to(a), output)
      output.flush()
    }
  }

  implicit def decodeIncrementalHNil[N <: Nat]: IncrementalDecoder[HNil, N] = new IncrementalDecoder[HNil, N] {
    override def decode(input: CodedInputStream): Result[HNil] = Right(HNil)
  }

  implicit def decodeIncrementalHList[H, T <: HList, N <: Nat](implicit
    hDecoder: FieldDecoder[H],
    index: shapeless.ops.nat.ToInt[N],
    tDecoder: IncrementalDecoder[T, Succ[N]]
  ): IncrementalDecoder[H :: T, N] = new IncrementalDecoder[H :: T, N] {
    override def decode(input: CodedInputStream): Result[H :: T] = {
      for {
        h <- hDecoder.read(input, index()).right
        t <- tDecoder.decode(input).right
      } yield h :: t
    }
  }

  implicit def decodeIncremental[A, N <: Nat, R <: HList](implicit
    gen: Generic.Aux[A, R],
    decoder: IncrementalDecoder[R, N]
  ): IncrementalDecoder[A, N] = new IncrementalDecoder[A, N] {
    override def decode(input: CodedInputStream): Result[A] = {
      decoder.decode(input) match {
        case Right(repr) => Right(gen.from(repr))
        case l @ Left(_) => l.asInstanceOf[Result[A]]
      }
    }
  }

}
