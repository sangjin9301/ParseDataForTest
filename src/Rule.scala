import java.util

import scala.collection.mutable.ArrayBuffer

/**
  * Created by SANGJIN-NAM on 2017-12-20.
  */
class Rule(user: String, crnTime: Int) {
  var parseData = new ParseData
  var st = new SampleTest
  val cdrPath = "D:/ParseDataForTest/" + "CDR/" + user
  val locationPath = "D:/ParseDataForTest/" + "LOCATION/" + user

  def Rule01: Boolean = {
    var reqTime = 3600
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")

    var req = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime, reqTime)
    var base = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), crnTime - oneWeek, reqTime)

    var dataSet = st.changeMapToArraySet(req, base)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    return result
  }

  def Rule02: Boolean = {
    var reqTime = 3600
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")

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

    return result
  }

  def Rule03: Boolean = {
    var reqTime = 86400
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")

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

    return result
  }

  def Rule04: Boolean = {
    var reqTime = 86400
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")

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

    return result
  }

  def Rule05: Boolean = {
    var reqTime = 86400 * 30
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(cdrPath + ".csv")

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

    return result
  }

  //////////////////////////////////////////////////////////////////////////Location/////////////////////////////////////////////////////////////////////////////
  def Rule06: Boolean = {
    var reqTime = 3600
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")

    var req = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime, reqTime)
    var base = parseData.getLocationFrequencyOnTime(parseData.toLocation(temp), crnTime - oneWeek, reqTime)

    var dataSet = st.changeMapToArraySet_Location(req, base)
    if (st.test_equal_Vari(dataSet).equals(true)) p = st.pairedTTest(dataSet)
    else result = false

    if (p > 0.90) result = true
    else result = false

    return result
  }

  def Rule07: Boolean = {
    var reqTime = 3600
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")

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

    return result
  }

  def Rule08: Boolean = {
    var reqTime = 86400
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")

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

    return result
  }

  def Rule09: Boolean = {
    var reqTime = 86400
    var oneWeek = 86400 * 7
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")

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

    return result
  }

  def Rule10: Boolean = {
    var reqTime = 86400 * 30
    var p: Double = 0
    var result: Boolean = false

    var temp = parseData.getData(locationPath + ".csv")

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

    return result
  }
}