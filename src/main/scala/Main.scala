import breeze.linalg.{DenseMatrix, svd}
import breeze.plot.{Figure, plot, scatter}
import breeze.linalg.svd.SVD
import breeze.linalg._
import breeze.numerics._
import breeze.stats._

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //    drawCircle()
    //    pca()
    //    test()
    //    身高体重()
    val f1 = (x: Int) => x
    val f2 = (x: Int) => x
    val f3 = f1 compose f2
  }

  def drawCircle(): Unit = {
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
    //    p += scatter(x, y, _ => 0.01)
  }

  def pca(): Unit = {
    val sourceData = List(1 to 100: _*).map(i => List(i.toDouble, Random.nextInt(i * 2).toDouble))
    val data = DenseMatrix(sourceData: _*)
    val m = data.rows
    val r = (1.0 / m) * (data.t * data)
    val SVD(u, s, v) = svd(r)
    val Ureduce = u(::, 0 to 0) //底层使用的是数组，索引从0开始，而不是数学概念里的1开始
    val x = Ureduce(0, 0)
    val y = Ureduce(1, 0)
    val f = Figure()
    val p = f.subplot(0)
    p += scatter(sourceData.map(i => i.head), sourceData.map(_.tail.head), size = _ => 1)
    val l = linspace(0, 100)
    println(x, y)
    p += plot(l, l / x * y)
    //    p += plot(-150.0 to 150 by 1.0 map (i => (i * x, i * y)._1), -150.0 to 150 by 1.0 map (i => (i * x, i * y)._2))
    f.saveas("ha.png")
  }

  def a() {

    // 随机产生数据
    //    val featuresMatrix = DenseMatrix.rand[Double](3, 3)
    //    val labelMatrix = DenseMatrix.rand[Double](3, 1)

    // 测试数据
    val featuresMatrix = DenseMatrix(
      (1.0, 2.0, 3.0),
      (4.0, 5.0, 6.0),
      (7.0, 8.0, 9.0)
    )

    val labelMatrix = DenseMatrix(
      1.0,
      1.0,
      0.0
    )

    // 均值
    // DenseVector(4.0, 5.0, 6.0)
    val featuresMean = mean(featuresMatrix(::, *)).inner
    println("均值：")
    println(featuresMean)

    // 标准差
    // DenseVector(3.0, 3.0, 3.0)
    val featuresStddev = stddev(featuresMatrix(::, *)).inner
    println("\n标准差：")
    println(featuresStddev)

    // 减去均值
    /**
      * -3.0  -3.0  -3.0
      * 0.0   0.0   0.0
      * 3.0   3.0   3.0
      */
    featuresMatrix(*, ::) -= featuresMean
    println("\n减去均值：")
    println(featuresMatrix)

    // 除以标准差
    /**
      * -1.0  -1.0  -1.0
      * 0.0   0.0   0.0
      * 1.0   1.0   1.0
      */
    featuresMatrix(*, ::) /= featuresStddev
    println("\n除以标准差：")
    println(featuresMatrix)

    // 生成截距
    /**
      * 1.0
      * 1.0
      * 1.0
      */
    val intercept = DenseMatrix.ones[Double](featuresMatrix.rows, 1)
    println("\n截距：")
    println(intercept)

    // 拼接成为最终的训练集
    /**
      * 1.0  -1.0  -1.0  -1.0
      * 1.0  0.0   0.0   0.0
      * 1.0  1.0   1.0   1.0
      */
    val train = DenseMatrix.horzcat(intercept, featuresMatrix)
    println("\n训练集：")
    println(train)

    // 参数
    // 为方便检查结果，这里全部设置为1
    /**
      * 1.0
      * 1.0
      * 1.0
      * 1.0
      */
    val w = new DenseMatrix(4, 1, Array(1.0, 1.0, 1.0, 1.0))
    //    val w = DenseMatrix.rand[Double](4, 1) // 随机生成, 一定要指定类型
    println("\n参数：")
    println(w)

    /**
      * -2.0
      * 1.0
      * 4.0
      */
    // 随机生成w时，如果没有指定类型，A的计算结果虽然不会有错，但是后面将无法计算，除非通过asInstanceOf进行类型转换
    // 如果w指定了类型，那么在idea中，转换语句会是灰色的，意思是这句话没有作用，可以不写
    val A = (train * w).asInstanceOf[DenseMatrix[Double]]
    println("\nA：")
    println(A)

    /**
      * 0.11920292202211755
      * 0.7310585786300049
      * 0.9820137900379085
      */
    // Sigmoid函数
    val probability = 1.0 / (exp(A * -1.0) + 1.0)
    println("\nprobability：")
    println(probability)

    /**
      * MSE : 0.6041613548425021
      */
    val MSE = mean(pow(probability - labelMatrix, 2))
    println("\nMSE：")
    println(MSE)

    /**
      * RMSE : 0.777278170825929
      */
    val RMSE = sqrt(MSE)
    println("\nRMSE：")
    println(RMSE)
  }

  def 身高体重(): Unit = {
    val data = Array(
      Array("男", 6.0, 180, 12),
      Array("男", 5.92, 190, 11),
      Array("男", 5.58, 170, 12),
      Array("男", 5.92, 165, 10),
      Array("女", 5.0, 100, 6),
      Array("女", 5.5, 150, 8),
      Array("女", 5.42, 130, 7),
      Array("女", 5.75, 130, 9)
    )
    // 通过一组数据计算正态分布
    // 画点
    val (x, y) = data.map(arr => {
      (arr(1).asInstanceOf[Double], arr(2).asInstanceOf[Integer].toDouble)
    }).unzip
    val f = Figure()
    val p = f.subplot(0)
    p += scatter(x, y, _ => 0.005)

  }
}
