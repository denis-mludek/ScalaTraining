package Drawing

import Drawing.UI.Show
import doodle.core._

sealed trait FitnessDevice

case class Barbell(load: Int, length: Int) extends FitnessDevice {
  def weigh: Barbell = Barbell(load + 10, length + 10)
  def lighten: Barbell = Barbell(load - 10, length - 10)

  def toImage = {
    val bar = Fitness.drawRectangle(length, 20, Color.grey)
    val weight = Fitness.drawWeight(load)

    weight beside bar beside weight
  }
}

case class Mat(width: Int, length: Int) extends FitnessDevice {
  def toImage = Fitness.drawRectangle(width, length, Color.blue)

  def smaller: Option[Mat] = {
    if(width <= 15 || length <= 100) None
    else Some(Mat(width, length))
  }
  def smallerWidth(mat: Mat): Option[Int] =
    mat.smaller.map(smallerMat => smallerMat.width)

  def keepHugeMats(maybeMat: Option[Mat]): Option[Mat] =
    maybeMat.filter(mat => mat.width > 100 && mat.length > 200)

  def largeEnoughMap(mats: Seq[Mat]): Seq[Int] = {
    mats.map(mat => mat.length * mat.width).filter(area => area > 1000)
  }

  def largeEnough(mats: Seq[Mat]): Seq[Int] = mats match {
    case Nil => Seq.empty[Int]
    case mat +: matTail => {
      val area: Int = mat.length * mat.width
      if(area > 1000) area +: largeEnough(matTail)
      else largeEnough(matTail)
    }
  }
}

object Mat {
  def smallerButLargeEnough(mat: Mat): Option[Int] = {
    mat.smaller.filter(mat => mat.length * mat.width > 1000).map(mat => mat.length * mat.width)
  }

  implicit val matOrdering: Ordering[Mat] = new Ordering[Mat] {
    def getArea(m: Mat): Int = m.width * m.length

    override def compare(x: Mat, y: Mat): Int = {
      if(getArea(x) < getArea(y)) -1
      else if(getArea(x) > getArea(y)) 1
      else 0
    }
  }
}

object Fitness {

  def drawRectangle(width: Int, height: Int, color: Color): Image = {
    Rectangle(width, height) fillColor color
  }

  def drawWeight(width: Int) = Rectangle(width, 100) fillColor Color.black

  implicit val fitnessDeviceShow = new Show[FitnessDevice] {
    def toImage(device: FitnessDevice): Image = device match {
      case barbell: Barbell => barbell.toImage
      case mat: Mat => mat.toImage
    }
  }

}
