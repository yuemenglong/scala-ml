import breeze.plot.{Figure, plot}

object Main {
  def main(args: Array[String]): Unit = {
    val f = Figure()
    val p = f.subplot(0)

    def isCycle(a: Double, b: Double, r: Double): Boolean = Math.abs(a * a + b * b - r * r) < 0.01

    val a: Array[(Double, Double)] = (-1.0).to(1.0, 0.01).flatMap(i => {
      (-1.0).to(1.0, 0.01).map(j => {
        (i, j)
      })
    }).toArray
    val (x, y) = a.filter(p => isCycle(p._1, p._2, 1)).unzip
    p += plot(x, y, '.')
  }
}
