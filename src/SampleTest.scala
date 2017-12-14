import java.io.{File, FileWriter}
import java.util

import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.stat.inference.TTest

import scala.collection.mutable.ArrayBuffer

/**
  * Created by SANGJIN-NAM on 2017-12-12.
  */
class SampleTest {

  def test_equal_Vari(set: (Array[Int], Array[Int])): Boolean = {
    var req_arr = new ArrayBuffer[Double]
    var base_arr = new ArrayBuffer[Double]
    set._1.foreach(req_arr += _.toDouble)
    set._2.foreach(base_arr += _.toDouble)

    var vari_req = new Statistics(req_arr.toArray).getVariance
    var vari_base = new Statistics(base_arr.toArray).getVariance

    if ((vari_base / vari_req) >= 3) return false
    if ((vari_req / vari_base) >= 3) return false

    return true
  }

  def t_test_CDR(req_map: util.HashMap[String, Int], base_map: util.HashMap[String, Int]): Unit = {
    var req_arr = new ArrayBuffer[Int]
    var base_arr = new ArrayBuffer[Int]
    var target_list = new ArrayBuffer[String]

    target_list.foreach(target => {
      req_arr += req_map.get(target)
      base_arr += base_map.get(target)
    })
  }

  def changeMapToArraySet(req_map: util.HashMap[String, Int], base_map: util.HashMap[String, Int]): (Array[Int], Array[Int]) = {
    var req_arr = new ArrayBuffer[Int]
    var base_arr = new ArrayBuffer[Int]
    var target_list = new ArrayBuffer[String]

    req_map.forEach((key, value) => {
      target_list += key
    })

    target_list.foreach(target => {
      req_arr += req_map.get(target)
      base_arr += base_map.get(target)
    })

    return (req_arr.toArray, base_arr.toArray)
  }

  def writeTwoArray(name: String, arrSet: (Array[Int], Array[Int])): Unit = {
    var path = "D:/ParseDataForTest/Test"
    var file = new File(path)
    if (!file.exists) {
      file.mkdirs()
    }
    var sb = new StringBuilder
    var writer = new FileWriter(path + "/" + name)

    for (i <- 0 to arrSet._1.length - 1) {
      sb.append(arrSet._1(i))
      sb.append(",")
      sb.append(arrSet._2(i))
      sb.append("\n")
    }

    writer.write(sb.mkString)
    writer.close()
  }

  def pairedTTest(arrSet: (Array[Int], Array[Int])): Double = {
    var reqArr = new ArrayBuffer[Double]
    var baseArr = new ArrayBuffer[Double]

    arrSet._1.foreach(reqArr += _.toDouble)
    arrSet._2.foreach(baseArr += _.toDouble)


    val studentTest = new TTest
    var testResult = false
    var error: Double = 0
    var tValue: Double = 0

    val meanA = StatUtils.mean(reqArr.toArray)
    val meanB = StatUtils.mean(baseArr.toArray)

    try {
      testResult = studentTest.tTest(reqArr.toArray, baseArr.toArray, 0.05)
      error = studentTest.tTest(reqArr.toArray, baseArr.toArray)
      tValue = studentTest.t(reqArr.toArray, baseArr.toArray)
      System.out.println("result --> MA = " + meanA + " -- MB = " + meanB)
      System.out.println("result --> [" + testResult + "] with p-value = " + error + " and tValue = " + tValue + "\n")
    } catch {
      case ex: Exception =>
        System.out.println("There was an error when trying to calculate Student's t test!")
        ex.printStackTrace()
    }
    return error
  }

}
