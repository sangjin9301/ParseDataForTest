import java.util

class Statistics(var data: Array[Double]) {
  var size = 0
  size = data.length

  def getMean: Double = {
    var sum = 0.0
    for (a <- data) {
      sum += a
    }
    return sum / size
  }

  def getVariance: Double = {
    val mean = getMean
    var temp:Double = 0
    for (a <- data) {
      temp += (a - mean) * (a - mean)
    }
    return temp / (size - 1)
  }

  def getStdDev: Double = return Math.sqrt(getVariance)

  def median: Double = {
    util.Arrays.sort(data)
    if (data.length % 2 == 0) return (data((data.length / 2) - 1) + data(data.length / 2)) / 2.0
    return data(data.length / 2)
  }
}