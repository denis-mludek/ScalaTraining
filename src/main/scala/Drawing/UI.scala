package Drawing

import doodle.core.Image

object UI {

  trait Show[-A] {
    def toImage(a: A): Image
  }


  def genericImage[A](a: A)(implicit show: Show[A]) = show.toImage(a)

}
