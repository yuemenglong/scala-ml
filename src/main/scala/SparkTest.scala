import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by <yuemenglong@126.com> on 2018/3/9.
  */
object SparkTest {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("SparkTest").setMaster("local[*]")
    val sc = new SparkContext(conf)
    val lines = sc.textFile("file:///D:/spark.txt")
    val res = lines.flatMap(_.split(" ")).map((_, 1)).reduceByKey(_ + _).sortBy(_._2, false).collect()
    res.foreach { case (n, v) => println(n, v) }
  }
}
