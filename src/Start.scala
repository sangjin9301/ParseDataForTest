/**
  * Created by SANGJIN-NAM on 2017-12-12.
  */
object Start {
  var path: String = "D:/Context_Aware_Authentication/ParseDataForTest/"

  def makeInitFiles:Unit={
    var parseData = new ParseData
    parseData.saveToPersonal_CSV(path + "CallLog.csv")
    parseData.saveToPersonal_CSV(path + "Location.csv")
  }

  def main(args: Array[String]): Unit = {
    var parseData = new ParseData
    var st = new SampleTest
    var temp = parseData.getData("D:/Context_Aware_Authentication/ParseDataForTest/CDR/fa10-01-01.csv")

    var time:Int = 1310714640
    val DAY:Int = 86400

    var req = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), time, DAY/24)
    var base = parseData.getCDRFrequencyOnTime(parseData.toCDR(temp), time-DAY, DAY/24)
    st.writeTwoArray("test1.csv",st.changeMapToArraySet(req,base))
  }
}