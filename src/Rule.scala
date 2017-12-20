import java.io.{BufferedReader, FileReader}
import java.util

import org.apache.commons.io.LineIterator

import scala.collection.mutable.ArrayBuffer

/**
  * Created by SANGJIN-NAM on 2017-12-20.
  */
class Rule(user: String, isUser:Boolean) {
  var parseData = new ParseData
  var st = new SampleTest
  var cdrPath = ""
  var locationPath = ""
  if(isUser){
    cdrPath = "D:/ParseDataForTest/Test/CDR/" + user
    locationPath = "D:/ParseDataForTest/Test/Location/" + user
  }
  else{
    cdrPath = "D:/ParseDataForTest/Test/StolenCDR/" + user
    locationPath = "D:/ParseDataForTest/Test/StolenLocation/" + user
  }




  def Rule01: Int = {
    var reqTime = 3600
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")

    var crnTime = temp.last(2).toInt

    var req = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime, reqTime)
    var base = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - oneWeek, reqTime)

    var dataSet = st.changeMapToArraySet(req, base)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0

  }

  def Rule02: Int = {
    var reqTime = 3600
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")
    var crnTime = temp.last(2).toInt

    var req = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime, reqTime)
    var base1 = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - oneWeek, reqTime)
    var base2 = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - (oneWeek * 2), reqTime)
    var base3 = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - (oneWeek * 3), reqTime)
    var base4 = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - (oneWeek * 4), reqTime)

    var MeanMap = new util.HashMap[String, Int]
    base1.forEach((key, value) => {
      var sum = 0
      sum += value
      sum += base2.get(key)
      sum += base3.get(key)
      sum += base4.get(key)
      var mean = sum / 4
      MeanMap.put(key, mean)
    })

    var dataSet = st.changeMapToArraySet(req, MeanMap)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0
  }

  def Rule03: Int = {
    var reqTime = 86400
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")
    var crnTime = temp.last(2).toInt

    var req = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime, reqTime)
    var base1 = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - oneWeek, reqTime)
    var base2 = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - (oneWeek * 2), reqTime)
    var base3 = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - (oneWeek * 3), reqTime)
    var base4 = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - (oneWeek * 4), reqTime)

    var MeanMap = new util.HashMap[String, Int]
    base1.forEach((key, value) => {
      var sum = 0
      sum += value
      sum += base2.get(key)
      sum += base3.get(key)
      sum += base4.get(key)
      var mean = sum / 4
      MeanMap.put(key, mean)
    })

    var dataSet = st.changeMapToArraySet(req, MeanMap)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0
  }

  def Rule04: Int = {
    var reqTime = 86400
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")
    var crnTime = temp.last(2).toInt

    var req = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime, reqTime)

    var baselist = new ArrayBuffer[util.HashMap[String, Int]]
    for (i <- 1 to 24) {
      baselist += parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - (oneWeek * i), reqTime)
    }
    var MeanMap = new util.HashMap[String, Int]
    baselist(0).forEach((key, value) => {
      var sum = 0
      baselist.foreach(map => {
        sum += map.get(key)
      })
      var mean = sum / 24
      MeanMap.put(key, mean)
    })

    var dataSet = st.changeMapToArraySet(req, MeanMap)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0
  }

  def Rule05: Int = {
    var reqTime = 86400 * 30
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")
    var crnTime = temp.last(2).toInt

    var req = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime, reqTime)
    var base = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime-reqTime, reqTime*12)

    var MeanMap = new util.HashMap[String, Int]
    base.forEach((key,value)=>{
      MeanMap.put(key, value/12)
    })

    var dataSet = st.changeMapToArraySet(req, MeanMap)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0
  }

  //////////////////////////////////////////////////////////////////////////Location/////////////////////////////////////////////////////////////////////////////
  def Rule06: Int = {
    var reqTime = 3600
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")
    var crnTime = temp.last(2).toInt

    var req = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime, reqTime)
    var base = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - oneWeek, reqTime)

    var dataSet = st.changeMapToArraySet_Location(req, base)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0
  }

  def Rule07: Int = {
    var reqTime = 3600
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")
    var crnTime = temp.last(2).toInt

    var req = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime, reqTime)
    var base1 = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - oneWeek, reqTime)
    var base2 = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - (oneWeek * 2), reqTime)
    var base3 = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - (oneWeek * 3), reqTime)
    var base4 = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - (oneWeek * 4), reqTime)

    var MeanMap = new util.HashMap[(Double,Double), Int]
    base1.forEach((key, value) => {
      var sum = 0
      sum += value
      sum += base2.get(key)
      sum += base3.get(key)
      sum += base4.get(key)
      var mean = sum / 4
      MeanMap.put(key, mean)
    })

    var dataSet = st.changeMapToArraySet_Location(req, MeanMap)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0
  }

  def Rule08: Int = {
    var reqTime = 86400
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")
    var crnTime = temp.last(2).toInt

    var req = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime, reqTime)
    var base1 = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - oneWeek, reqTime)
    var base2 = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - (oneWeek * 2), reqTime)
    var base3 = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - (oneWeek * 3), reqTime)
    var base4 = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - (oneWeek * 4), reqTime)

    var MeanMap = new util.HashMap[(Double,Double), Int]
    base1.forEach((key, value) => {
      var sum = 0
      sum += value
      sum += base2.get(key)
      sum += base3.get(key)
      sum += base4.get(key)
      var mean = sum / 4
      MeanMap.put(key, mean)
    })

    var dataSet = st.changeMapToArraySet_Location(req, MeanMap)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0
  }

  def Rule09: Int = {
    var reqTime = 86400
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")
    var crnTime = temp.last(2).toInt

    var req = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime, reqTime)

    var baselist = new ArrayBuffer[util.HashMap[(Double,Double), Int]]
    for (i <- 1 to 24) {
      baselist += parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - (oneWeek * i), reqTime)
    }
    var MeanMap = new util.HashMap[(Double,Double), Int]
    baselist(0).forEach((key, value) => {
      var sum = 0
      baselist.foreach(map => {
        sum += map.get(key)
      })
      var mean = sum / 24
      MeanMap.put(key, mean)
    })

    var dataSet = st.changeMapToArraySet_Location(req, MeanMap)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0
  }

  def Rule10: Int = {
    var reqTime = 86400 * 30
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")
    var crnTime = temp.last(2).toInt

    var req = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime, reqTime)
    var base = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime-reqTime, reqTime*12)

    var MeanMap = new util.HashMap[(Double,Double), Int]
    base.forEach((key,value)=>{
      MeanMap.put(key, value/12)
    })

    var dataSet = st.changeMapToArraySet_Location(req, MeanMap)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    if(result) return 1
    else return 0
  }
}
