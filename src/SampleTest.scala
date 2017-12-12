import java.io.{File, FileWriter}
import java.util

import scala.collection.mutable.ArrayBuffer

/**
  * Created by SANGJIN-NAM on 2017-12-12.
  */
class SampleTest {

  def t_test_CDR(req_map:util.HashMap[String,Int], base_map:util.HashMap[String,Int]):Unit={
    var req_arr = new ArrayBuffer[Int]
    var base_arr = new ArrayBuffer[Int]
    var target_list = new ArrayBuffer[String]

    target_list.foreach( target => {
      req_arr += req_map.get(target)
      base_arr += base_map.get(target)
    })
  }

  def changeMapToArraySet(req_map:util.HashMap[String,Int], base_map:util.HashMap[String,Int]):(Array[Int],Array[Int])={
    var req_arr = new ArrayBuffer[Int]
    var base_arr = new ArrayBuffer[Int]
    var target_list = new ArrayBuffer[String]

    req_map.forEach((key,value)=>{
      target_list+=key
    })

    target_list.foreach( target => {
      req_arr += req_map.get(target)
      base_arr += base_map.get(target)
    })

    return (req_arr.toArray,base_arr.toArray)
  }

  def writeTwoArray(name:String, arrSet:(Array[Int],Array[Int])):Unit={
    var path = "D:/Context_Aware_Authentication/ParseDataForTest/Test"
    var file = new File(path)
    if (!file.exists) {
      file.mkdirs()
    }
    var sb = new StringBuilder
    var writer = new FileWriter(path+"/"+name)

    for(i <- 0 to arrSet._1.length-1){
      sb.append(arrSet._1(i))
      sb.append(",")
      sb.append(arrSet._2(i))
      sb.append("\n")
    }

    writer.write(sb.mkString)
    writer.close()
  }

}
