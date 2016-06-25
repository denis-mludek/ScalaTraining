package JsonDecoder

trait Decoder[+A] { decoderA =>

  def decode(j: Json): Option[A]

  def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    override def decode(j: Json): Option[B] = {
      decoderA.decode(j).map(f)
    }
  }

  def map2[B, C](decoderB: Decoder[B])(f: (A, B) => C): Decoder[C] = new Decoder[C] {
    override def decode(j: Json): Option[C] = {
      for {
        x <- decoderA.decode(j)
        y <- decoderB.decode(j)
      } yield f(x,y)
    }
  }
}

object Decoder {
  val string: Decoder[String] =
    new Decoder[String] {
      override def decode(j: Json): Option[String] = j match {
        case JString(a) => Some(a)
        case _ => None
      }
    }

  val bigDecimal: Decoder[BigDecimal] =
    new Decoder[BigDecimal] {
      override def decode(j: Json): Option[BigDecimal] = j match {
        case JNumber(a) => Some(a)
        case _ => None
      }
    }

  val int: Decoder[Int] =
    new Decoder[Int] {
      override def decode(j: Json): Option[Int] = {
        bigDecimal.decode(j).map(_.intValue)
      }
    }

  val boolean: Decoder[Boolean] =
    new Decoder[Boolean] {
      override def decode(j: Json): Option[Boolean] = j match {
        case JBool(a) => Some(a)
        case _ => None
      }
    }

  val obj: Decoder[Map[String, Json]] =
    new Decoder[Map[String, Json]] {
      override def decode(j: Json): Option[Map[String, Json]] = j match {
        case JObject(a) => Some(a)
        case _ => None
      }
    }

  val aray: Decoder[Seq[Json]] =
    new Decoder[Seq[Json]] {
      override def decode(j: Json): Option[Seq[Json]] = j match {
        case JArray(a) => Some(a)
        case _ => None
      }
    }

  def field(name: String) = new Decoder[Json] {
    override def decode(j: Json): Option[Json] = j match {
      case JObject(o) => o.get(name)
      case _ => None
    }
  }

  def chain[A](first: Decoder[Json], second: Decoder[A]): Decoder[A] = new Decoder[A] {
    override def decode(j: Json): Option[A] = {
      first.decode(j).flatMap(second.decode)
    }
  }
}