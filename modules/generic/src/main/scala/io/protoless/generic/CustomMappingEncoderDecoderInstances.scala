package io.protoless.generic

import com.google.protobuf.{CodedInputStream, CodedOutputStream}

import shapeless.{::, Generic, HList, HNil, Nat}
import shapeless.ops.nat.ToInt

import io.protoless.messages.decoders.CustomMappingDecoder
import io.protoless.messages.encoders.CustomMappingEncoder
import io.protoless.fields.{FieldEncoder, FieldDecoder}
import io.protoless.messages.Decoder.Result

trait CustomMappingEncoderDecoderInstances extends IncrementalDecoderInstances {

  implicit val decodeCustomMappingHNil: CustomMappingDecoder[HNil, HNil] = new CustomMappingDecoder[HNil, HNil] {
    override def decode(input: CodedInputStream): Result[HNil] = Right(HNil)
  }

  // Decode mapping specified with Nat (Nat._1 :: Nat._3 :: HNil)
  implicit def decodeCustomMappingHList[H, T <: HList, L <: Nat, TN <: HList](implicit
    hDecoder: FieldDecoder[H],
    index: ToInt[L],
    tDecoder: CustomMappingDecoder[T, TN]
  ): CustomMappingDecoder[H :: T, L :: TN] = new CustomMappingDecoder[H :: T, L :: TN] {
    override def decode(input: CodedInputStream): Result[H :: T] = {
      for {
        h <- hDecoder.read(input, index()).right
        t <- tDecoder.decode(input).right
      } yield h :: t
    }
  }

  // Decode mapping specified with Literal types (-Yliteral-types) (1 :: 3 :: HNil)
  implicit def decodeCustomMappingHListLiteral[H, T <: HList, L <: Int, TN <: HList](implicit
    hDecoder: FieldDecoder[H],
    index: ValueOf[L],
    tDecoder: CustomMappingDecoder[T, TN]
  ): CustomMappingDecoder[H :: T, L :: TN] = new CustomMappingDecoder[H :: T, L :: TN] {
    override def decode(input: CodedInputStream): Result[H :: T] = {
      for {
        h <- hDecoder.read(input, valueOf[L]).right
        t <- tDecoder.decode(input).right
      } yield h :: t
    }
  }

  implicit def decodeCustomMapping[A, L <: HList, R <: HList](implicit
    gen: Generic.Aux[A, R],
    decoder: CustomMappingDecoder[R, L]
  ): CustomMappingDecoder[A, L] = new CustomMappingDecoder[A, L] {
    override def decode(input: CodedInputStream): Result[A] = {
      decoder.decode(input) match {
        case Right(repr) => Right(gen.from(repr))
        case l @ Left(_) => l.asInstanceOf[Result[A]]
      }
    }
  }

  implicit val encodeCustomMappingHNil: CustomMappingEncoder[HNil, HNil] = new CustomMappingEncoder[HNil, HNil] {
    override def encode(a: HNil, output: CodedOutputStream): Unit = {}
  }

  // Encode mapping specified with Nat (Nat._1 :: Nat._3 :: HNil)
  implicit def encodeCustomMappingHList[H, T <: HList, L <: Nat, TN <: HList](implicit
    hEncoder: FieldEncoder[H],
    index: ToInt[L],
    tEncoder: CustomMappingEncoder[T, TN]
  ): CustomMappingEncoder[H :: T, L :: TN] = new CustomMappingEncoder[H :: T, L :: TN] {
    override def encode(a: H :: T, output: CodedOutputStream): Unit = {
      val (h :: t) = a
      hEncoder.write(index(), h, output)
      tEncoder.encode(t, output)
    }
  }

  // Encode mapping specified with Literal (1 :: 3 :: HNil)
  implicit def encodeCustomMappingHListLiteral[H, T <: HList, L <: Int, TN <: HList](implicit
    hEncoder: FieldEncoder[H],
    index: ValueOf[L],
    tEncoder: CustomMappingEncoder[T, TN]
  ): CustomMappingEncoder[H :: T, L :: TN] = new CustomMappingEncoder[H :: T, L :: TN] {
    override def encode(a: H :: T, output: CodedOutputStream): Unit = {
      val (h :: t) = a
      hEncoder.write(valueOf[L], h, output)
      tEncoder.encode(t, output)
    }
  }

  implicit def encodeCustomMapping[A, L <: HList, R <: HList](implicit
    gen: Generic.Aux[A, R],
    encoder: CustomMappingEncoder[R, L]
  ): CustomMappingEncoder[A, L] = new CustomMappingEncoder[A, L] {
    override def encode(a: A, output: CodedOutputStream): Unit = {
      encoder.encode(gen.to(a), output)
      output.flush()
    }
  }

}
