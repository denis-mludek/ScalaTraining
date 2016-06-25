package Polymorph

object genericSeqMethods {

  def size[A](as: Seq[A]): Int = as match {
    case Nil => 0
    case a +: asTail => 1 + size(asTail)
  }

  def concat[A](as1: Seq[A], as2: Seq[A]): Seq[A] = as1 match {
    case Nil => as2
    case a +: as => a +: concat(as, as2)
  }

  def concatFoldRight[A](as1: Seq[A], as2: Seq[A]): Seq[A] = {
    as1.foldRight(as2)((value, acc) => value +: acc)
  }

  def reverse[A](as: Seq[A]): Seq[A] = as match {
    case Nil => as
    case a +: as => reverse(as) :+ a
  }

  def reverseFoldRight[A](as: Seq[A]): Seq[A] = {
    as.foldRight(Seq.empty[A])((value, acc) => acc :+ value )
  }
}

object MyOwnSeqType {

  sealed trait Col[A] {
    def size: Int = this match {
      case Empty() => 0
      case OneAnd(a, tail) => 1 + tail.size
    }

    def concat(as: Col[A]): Col[A] = this match {
      case Empty() => as
      case OneAnd(a, tail) => OneAnd(a, tail.concat(as))
    }

//    def reverse: Col[A] = {
//      this.foldRight(Empty())((value, acc) => OneAnd(value, acc))
//    }

    def map[B](f: A => B): Col[B] = this match {
      case Empty() => Empty()
      case OneAnd(a, tail) => OneAnd(f(a), tail.map(f))
    }

    def filter(p: A => Boolean): Col[A] = this match {
      case Empty() => Empty()
      case OneAnd(a, tail) => if(p(a)) OneAnd(a, tail.filter(p)) else Empty()
    }

    def forall(p: A => Boolean): Boolean = this match {
      case Empty() => true
      case OneAnd(a, tail) => if(p(a)) tail.forall(p) else false
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Empty() => false
      case OneAnd(a, tail) => if(p(a)) true else tail.exists(p)
    }

    def foldRight[B](b: B)(f: (A, B) => B): B = this match {
      case Empty() => b
      case OneAnd(a, tail) => f(a, tail.foldRight(b)(f))
    }
  }

  case class Empty[A]() extends Col[A]
  case class OneAnd[A](a: A, tail: Col[A]) extends Col[A]
}

object GenericSum {
  def sum[A](as: Seq[A])(implicit numeric: Numeric[A]): A =
    as.foldLeft(numeric.zero)((acc, value) => numeric.plus(acc, value))
}