import java.io.{BufferedReader, File, FileReader, FileWriter}

import org.apache.commons.io.LineIterator

import scala.collection.mutable.ArrayBuffer

/**
  * Created by SANGJIN-NAM on 2017-12-12.
  */
object Start {
  var path: String = "D:/ParseDataForTest/"

  def makeInitFiles: Unit = {
    var parseData = new ParseData
    parseData.saveToPersonal_CSV(path + "CallLog.csv")
    parseData.saveToPersonal_CSV(path + "Location.csv")
  }

  def evaluactionAll_1(user: String): Unit = {
    var cdrpath = path + "LOCATION/"
    var parseData = new ParseData
    var st = new SampleTest
    parseData.user_list.foreach(id => {
      println(id + " 1 day ago----------------------------------------------------------------------------")

      var p: Double = 0
      var result: Boolean = false
      var temp = parseData.getData(cdrpath + id + ".csv")
      var userpath = parseData.getData(cdrpath + user + ".csv")

      var time: Int = 1286681280
      val DAY: Int = 86400

      var req = parseData.getCDRFrequencyOnTime(parseData.toCDR(userpath), time, DAY / 24)
      var base = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), time - DAY, DAY / 24)
      var dataSet = st.changeMapToArraySet(req, base)
      if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
      else println("F-test is False\n")

      if (p > 0.90) result = true
      else result = false

      saveResult("1h_yesterday_1h_", user, id, result)


      st.writeTwoArray("Location_" + id + "_yesterday.csv", dataSet)

    })


  }

  def evaluactionAll_2(user: String): Unit = {
    var cdrpath = path + "LOCATION/"
    var parseData = new ParseData
    var st = new SampleTest

    parseData.user_list.foreach(id => {
      println(id + " 1 week ago----------------------------------------------------------------------------")

      var p: Double = 0
      var result: Boolean = false
      var temp = parseData.getData(cdrpath + id + ".csv")
      var userpath = parseData.getData(cdrpath + user + ".csv")

      var time: Int = 1286681280
      val DAY: Int = 86400

      var req = parseData.getCDRFrequencyOnTime(parseData.toCDR(userpath), time, DAY / 24)
      var base = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), time - (DAY * 7), DAY / 24)
      var dataSet = st.changeMapToArraySet(req, base)
      if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
      else println("F-test is False")

      if (p > 0.90) result = true
      else result = false

      saveResult("1h_lastweek_1h_", user, id, result)


      st.writeTwoArray("Location_" + id + "_week.csv", dataSet)
    })

  }

  def savePR(path:String):Unit={
    var bufReader: BufferedReader = null
    bufReader = new BufferedReader(new FileReader(path))
    var LineList = new ArrayBuffer[Array[String]]
//    var sb = new StringBuilder
    val iter = new LineIterator(bufReader)
    while (iter.hasNext) {
      var temp = iter.nextLine.split(",")
//      sb.append(temp(0))
//      sb.append(",")
//      sb.append(temp(1))
//      sb.append(",")
//      sb.append(temp(2))
//      sb.append(",")
//      sb.append(temp(3))
//      sb.append("\n")
      LineList += temp
    }

    var predict_list = new ArrayBuffer[Boolean]
    var actual_list = new ArrayBuffer[Boolean]
    var tp_list = new ArrayBuffer[Boolean]
    var fn_list = new ArrayBuffer[Boolean]
    var fp_list = new ArrayBuffer[Boolean]
    var tn_list = new ArrayBuffer[Boolean]

    LineList.foreach(line => {
      var user1 = line(0)
      var user2 = line(1)
      var predict = line(2).toBoolean
      var actual = user1.equals(user2)

      predict_list += predict
      actual_list += actual
      fn_list += getFalseNegative(predict, actual)
      fp_list += getFalsePositive(predict, actual)
      tn_list += getTrueNegative(predict, actual)
      tp_list += getTruePositive(predict, actual)

    })

    var fn_list_count: Double = getTrueCount(fn_list)
    var fp_list_count: Double = getTrueCount(fp_list)
    var tn_list_count: Double = getTrueCount(tn_list)
    var tp_list_count: Double = getTrueCount(tp_list)

//    sb.append("\n")
//    sb.append("Precision")
//    sb.append(",")
//    sb.append("Recall")
//    sb.append(",")
//    sb.append("Accuracy")
//    sb.append(",")
//    sb.append("F-measure")
//    sb.append("\n")

    var Precision = tp_list_count / (tp_list_count + fp_list_count)
    var Recall = tp_list_count / (tp_list_count + fn_list_count)
    var Accuracy = (tp_list_count + tn_list_count) / LineList.size
    var F_Measure = (2 * Precision * Recall) / (Precision + Recall)
//    sb.append(Precision)
//    sb.append(",")
//    sb.append(Recall)
//    sb.append(",")
//    sb.append(Accuracy)
//    sb.append(",")
//    sb.append(F_Measure)
//    sb.append("\n")
//
//    var writer = new FileWriter(path)
//    writer.write(sb.mkString)
//    writer.close()

    println("------------------"+path+"-----------------")
    println("Precision : "+Precision)
    println("Recall : "+Recall)
    println("Accuracy : "+Accuracy)
    println("F_Measure : "+F_Measure)


  }

  def saveResult(dir:String ,user: String, other: String, predict: Boolean): Unit = {
    var actual = user.equals(other)
    var path = "D:/ParseDataForTest/Precision&Recall"
    var filePath = path + "/"+dir+ user + ".csv"
    var file = new File(path)
    if (!file.exists) {
      file.mkdirs
    }
    var legacy = new File(filePath)
    if(!legacy.exists){
      var writer = new FileWriter(filePath)
      writer.write("")
      writer.close()
    }

    var bufReader: BufferedReader = null
    bufReader = new BufferedReader(new FileReader(filePath))
    var sb = new StringBuilder

    val iter = new LineIterator(bufReader)

    while (iter.hasNext) {
      var temp = iter.nextLine.split(",")
      sb.append(temp(0))
      sb.append(",")
      sb.append(temp(1))
      sb.append(",")
      sb.append(temp(2))
      sb.append(",")
      sb.append(temp(3))
      sb.append("\n")
    }
    sb.append(user)
    sb.append(",")
    sb.append(other)
    sb.append(",")
    sb.append(predict)
    sb.append(",")
    sb.append(actual)
    sb.append("\n")

    var writer = new FileWriter(filePath)
    writer.write(sb.mkString)
    writer.close()

  }

  def getTruePositive(pred: Boolean, actu: Boolean): Boolean = {
    if (pred.equals(true)) {
      if (actu.equals(true)) {
        return true
      } else return false
    } else return false
  }

  def getFalseNegative(pred: Boolean, actu: Boolean): Boolean = {
    if (pred.equals(false)) {
      if (actu.equals(true)) {
        return true
      } else return false
    } else return false
  }

  def getFalsePositive(pred: Boolean, actu: Boolean): Boolean = {
    if (pred.equals(true)) {
      if (actu.equals(false)) {
        return true
      } else return false
    } else return false
  }

  def getTrueNegative(pred: Boolean, actu: Boolean): Boolean = {
    if (pred.equals(false)) {
      if (actu.equals(false)) {
        return true
      } else return false
    } else return false
  }

  def getTrueCount(column: ArrayBuffer[Boolean]): Double = {
    var count: Int = 0
    column.foreach(x => {
      if (x.equals(true)) count += 1
    })
    return count.toDouble
  }


  def main(args: Array[String]): Unit = {

    var rule = new Rule("fa10-01-10",1305324180)
    println(rule.Rule01)
    println(rule.Rule02)
    println(rule.Rule03)
    println(rule.Rule04)
    println(rule.Rule05)
    println(rule.Rule06)
    println(rule.Rule07)
    println(rule.Rule08)
    println(rule.Rule09)
    println(rule.Rule10)


  }
}