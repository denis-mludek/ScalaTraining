package Drawing

import doodle.core.{Color, Image, Triangle}

object HigherOrder {

  def stack(image: Int => Image, count: Int) =
    layout((img1, img2) => img1 on img2, image, count)

  def spiral(count: Int): Image = ???

  def layout(op: (Image, Image) => Image, image: Int => Image, n: Int): Image = {
    if(n == 1) image(n)
    else op(image(n), layout(op, image, n-1))
  }

  def sierpinski(count: Int): Image =
    layout((img1, img2) => img2 above (img2 beside img2), n => Triangle(10,10).fillColor(Color.black), count)


  def barbells(count: Int) =
    layout((img1, img2) => img1 above img2, n => Barbell(15, 100).toImage, count)
}
