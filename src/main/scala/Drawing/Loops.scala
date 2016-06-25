package Drawing

import doodle.core._

object Loops {

  def barbells(n: Int): Image = {
    val barbell = Barbell(15, 100)
    val unit = barbell.toImage
    if (n == 1) unit
    else unit above barbells(n - 1)
  }

  def circles(n: Int): Image = {
    if (n == 1) Circle(n*30)
    else Circle(n*30) on circles(n - 1)
  }

  def spiral(n: Int): Image = {
    val size = 10 + n * 2
    val dist = 50 + n * 5
    val angle = Angle.degrees((n * 36) % 360)
    val circle = Circle(size).at(dist * angle.sin, dist * angle.cos)
    if (n == 1) circle else circle on spiral(n - 1)
  }

  def sierpinski(n: Int): Image = {
    if(n==1) Triangle(10,10).fillColor(Color.black)
    else {
      val prev = sierpinski(n-1)
      prev above (prev beside prev)
    }
  }

  def stack(image: Int => Image, n: Int): Image =
    if (n == 1) image(n) else image(n) on stack(image, n - 1)

}
