package Drawing

import doodle.core.{Angle, Circle, Image, Rectangle}
import doodle.jvm._

object Forms {
  def cirlcess(n: Int): Seq[Circle] = {
    val circle = Circle(20 + 5 * n)
    if (n == 1) Seq(circle)
    else circle +: cirlcess(n - 1)
  }

  def spirals(n: Int): Seq[Image] = {
    val spi = Loops.spiral(3)
    if (n == 1) Seq(spi)
    else spi +: spirals(n - 1)
  }

  def stack(images: Seq[Image]): Image = images match {
    case Nil => Rectangle(0, 0)
    case image +: imageTail => image on stack(imageTail)
  }

  def duplicate(images: Seq[Image]): Seq[Image] =
    images.flatMap(image => Seq(image above image, image beside image))

  def drawAll(images: Seq[Image]): Unit =
    images.foreach(image => draw(image))

  def circlesMap(n: Int): Seq[Circle] =
    (1 to n).map(i => Circle(20 + 5 * i))


  def spiralMap(n: Int): Seq[Image] =
    (1 to n).map(_ => Loops.spiral(n))

  def stackFoldLeft(images: Seq[Image]): Image =
    images.foldLeft[Image](Rectangle(0,0))((acc, image) => acc on image)

  def smallerSmaller(mat: Mat): Option[Mat] =
    mat.smaller.flatMap(_.smaller)

  def circles(count: Int) = Loops.stack(n => Circle(25 + 15 * n), count)

  def createCircleForSpiral(n: Int): Image = {
    val size = 10 + n * 2
    val dist = 50 + n * 5
    val angle = Angle.degrees((n * 36) % 360)
    Circle(size).at(dist * angle.sin, dist * angle.cos)
  }

  def spiralWithNewStack(count: Int): Image =
    Loops.stack(n => createCircleForSpiral(n), count)
}