import java.io._
import java.text.SimpleDateFormat
import java.util
import java.util.Calendar

import org.apache.commons.io.LineIterator

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

/**
  * Created by SANGJIN-NAM on 2017-12-12.
  */
class ParseData {

  val user_list_path = "D:/ParseDataForTest/UserList.csv"
  var locationPath: String = "D:/ParseDataForTest/LOCATION"
  var callLogPath: String = "D:/ParseDataForTest/CDR"

  val splitBy = ","
  val user_list = setUserList(user_list_path)


  /**
    * 사용자 ID를 배열로 얻는다.
    */
  def setUserList(path: String): Array[String] = {
    var bufReader: BufferedReader = null;
    var line = ""
    var csvSplitBy = ","
    var user_list = new ArrayBuffer[String]

    try {
      bufReader = new BufferedReader(new FileReader(path))
      val iter = new LineIterator(bufReader)
      while (iter.hasNext) {
        line = iter.nextLine
        var id = line.split(csvSplitBy)
        user_list += id(0)
      }
    } catch {
      case e: IOException => {
        e.printStackTrace()
      }
    }

    return user_list.toArray
  }

  /**
    * 전체 사용자의 데이터를 각자의 데이터로 나누어 모두 csv로 저장
    */
  def saveToPersonal_CSV(path: String): Unit = {
    var bufReader: BufferedReader = null
    bufReader = new BufferedReader(new FileReader(path))
    val iter = new LineIterator(bufReader)
    var line_list = new ArrayBuffer[String]
    try {
      while (iter.hasNext) {
        line_list += iter.nextLine
      }

      this.user_list.foreach(id => {
        if (path.contains("Location")) writeLocationByUser(id, line_list.toArray)
        if (path.contains("CallLog")) writeCDRByUser(id, line_list.toArray)
      })

      bufReader.close
    } catch {
      case e: FileNotFoundException => {
        e.printStackTrace
      }
      case e: IOException => {
        e.printStackTrace
      }
      case e: ArrayIndexOutOfBoundsException => {
        e.printStackTrace
      }
    } finally {
      if (bufReader != null) {
        try {
          bufReader.close
        } catch {
          case e: IOException => {
            e.printStackTrace
          }
        }
      }
    }
  }

  /**
    * 단일 사용자에 대한 .csv에서 정보를 읽어 2차원 배열로 반환
    * 1차원 : line index
    * 2차원 : column
    */
  def getData(path: String): Array[Array[String]] = {
    var arr = new ArrayBuffer[Array[String]]
    var bufReader: BufferedReader = null
    bufReader = new BufferedReader(new FileReader(path))
    val iter = new LineIterator(bufReader)

    while (iter.hasNext) {
      arr += iter.nextLine().split(splitBy)
    }
    arr.remove(0)
    return arr.toArray
  }

  /**
    * 2차원 배열의 단일 사용자 정보를 CDR객체로 만들어 1차월 배열로 반환
    */
  def toCDR(arr: Array[Array[String]]): Array[CDR] = {
    var cdr_list = new ArrayBuffer[CDR]

    arr.foreach(line => {
      var cdr = new CDR
      cdr.setUser(line(0))
      cdr.setDate(line(1))
      cdr.setTimestamp(line(2).toDouble)
      cdr.setTime(line(3).toInt)
      cdr.setWeek(line(4).toInt)
      cdr.setType(line(5))
      cdr.setTarget(line(6))
      cdr_list += cdr
    })
    return cdr_list.toArray
  }

  /**
    * 2차원 배열의 단일 사용자 정보를 Location객체로 만들어 1차월 배열로 반환
    */
  def toLocation(arr: Array[Array[String]]): Array[Location] = {
    var loc_list = new ArrayBuffer[Location]

    arr.foreach(line => {

      var loc = new Location
      loc.setUser(line(0))
      loc.setDate(line(1))
      loc.setTimestamp(line(2).toDouble)
      loc.setTime(line(3).toInt)
      loc.setWeek(line(4).toInt)
      loc.setX(line(5).toDouble)
      loc.setY(line(6).toDouble)
      loc_list += loc

    })

    return loc_list.toArray
  }

  def getCDRFrequencyOnTime(cdr_arr: Array[CDR], timestamp: Int, reqTime: Int): util.HashMap[String, Int] = {
    val term = timestamp - reqTime
    var target_freq_Map = new util.HashMap[String, Int]

    cdr_arr.foreach(cdr => {
      var target = cdr.getTarget
      if (!target_freq_Map.containsKey(target)) target_freq_Map.put(target, 0)
    })

    cdr_arr.foreach(cdr => {
      if (cdr.getTimestamp <= timestamp) {
        if (cdr.getTimestamp > term) {
          var freq = target_freq_Map.get(cdr.getTarget)
          target_freq_Map.put(cdr.getTarget, freq + 1)
        }
      }
    })

    return target_freq_Map
  }

  def getLocationFrequencyOnTime(loc_arr: Array[Location], timestamp: Int, reqTime: Int): util.HashMap[(Double, Double), Int] = {
    val term = timestamp - reqTime
    var target_freq_Map = new util.HashMap[(Double, Double), Int]

    loc_arr.foreach(loc => {
      var location = (loc.getX, loc.getY)
      if (!target_freq_Map.containsKey(location)) target_freq_Map.put(location, 0)
    })

    loc_arr.foreach(loc => {
      if (loc.getTimestamp <= timestamp) {
        if (loc.getTimestamp > term) {
          var location = (loc.getX, loc.getY)
          var freq = target_freq_Map.get(location)
          target_freq_Map.put(location, freq + 1)
        }
      }
    })

    return target_freq_Map
  }

  /**
    * 전체 Location csv파일에서 사용자별 csv로 나눠 저장
    */
  def writeLocationByUser(user: String, data: Array[String]): Unit = {
    var fileName = locationPath + "/" + user + ".csv"
    var file = new File(locationPath)
    if (!file.exists) {
      file.mkdirs()
    }
    var writer = new FileWriter(fileName)
    var line = ""
    var csvSplitBy = ","

    var sb = new StringBuilder
    sb.append("ID")
    sb.append(",")
    sb.append("DATE")
    sb.append(",")
    sb.append("Timestamp")
    sb.append(",")
    sb.append("TIME")
    sb.append(",")
    sb.append("WEEK")
    sb.append(",")
    sb.append("ACCURACY")
    sb.append(",")
    sb.append("X")
    sb.append(",")
    sb.append("Y")
    sb.append("\n")

    println("save :" + user + "'s Location")


    for (i <- 1 to data.length - 1) {
      breakable {
        line = data(i)
        if (line.contains("Inf")) break

        //use comma as separator
        var location = line.split(csvSplitBy)
        var time = location(1).split(" ")(1).split(":")(0)
        var ts = date2Timestamp(location(1), "yyyy-MM-dd HH:mm")
        var week = getWeek(location(1))


        if (location(0).equals(user)) {
          sb.append(user)
          sb.append(",")
          sb.append(location(1))
          sb.append(",")
          sb.append(ts)
          sb.append(",")
          sb.append(time)
          sb.append(",")
          sb.append(week)
          sb.append(",")
          sb.append(location(2))
          sb.append(",")
          sb.append(location(3))
          sb.append(",")
          sb.append(location(4))
          sb.append("\n")
        }
      }
    }
    writer.write(sb.mkString)
    writer.close()

  }


  /**
    * 전체 CDR csv파일에서 사용자별 csv로 나눠 저장
    */
  def writeCDRByUser(user: String, data: Array[String]): Unit = {
    var fileName = callLogPath + "/" + user + ".csv"
    var file = new File(callLogPath)
    if (!file.exists) {
      file.mkdirs();
    }
    var writer = new FileWriter(fileName)
    var line = ""
    var csvSplitBy = ","

    var sb = new StringBuilder
    sb.append("ID")
    sb.append(",")
    sb.append("DATE")
    sb.append(",")
    sb.append("Timestamp")
    sb.append(",")
    sb.append("TIME")
    sb.append(",")
    sb.append("WEEK")
    sb.append(",")
    sb.append("TYPE")
    sb.append(",")
    sb.append("TARGET")
    sb.append("\n")

    println("save :" + user + "'s CDR")

    for (i <- 1 to data.length - 1) {
      breakable {
        line = data(i)
        if (line.contains("Inf")) break

        //use comma as separator
        var log = line.split(csvSplitBy)
        var id = log(0).replace("\"", "")
        var time = log(2).split(" ")(1).split(":")(0)
        var ts = date2Timestamp(log(2), "yyyy-MM-dd HH:mm")
        var week = getWeek(log(2))


        if (user.equals(id)) {
          sb.append(user)
          sb.append(",")
          sb.append(log(2))
          sb.append(",")
          sb.append(ts)
          sb.append(",")
          sb.append(time)
          sb.append(",")
          sb.append(week)
          sb.append(",")
          sb.append(log(3))
          sb.append(",")
          sb.append(log(5))
          sb.append("\n")
        }
      }
    }
    writer.write(sb.mkString)
    writer.close()

  }


  def date2Timestamp(date_str: String, format: String): String = {
    var sdf: SimpleDateFormat = new SimpleDateFormat(format)
    return String.valueOf(sdf.parse(date_str).getTime / 1000)
  }

  def timestamp: String = {
    var time: Long = System.currentTimeMillis()
    var t: String = String.valueOf(time / 1000)
    return t
  }

  def getWeek(dateString: String): Int = {
    var format = "yyyy-MM-dd HH:mm"

    var df = new SimpleDateFormat(format)
    var date = df.parse(dateString)

    var cal = Calendar.getInstance()
    cal.setTime(date)
    var week = cal.get(Calendar.DAY_OF_WEEK)
    var weekString = ""

    return week
  }

}
