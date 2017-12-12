class CDR {
  //ID DATE TIME WEEK TYPE TARGET
  var user: String = ""
  var date: String = ""
  var timestamp: Double = 0
  var time: Int = 0
  var week: Int = 0
  var target: String = ""
  var call_type: String = "" //outgoing+, incoming+

  def setUser(user: String): Unit = {
    this.user = user
  }

  def setDate(date: String): Unit = {
    this.date = date
  }

  def setTimestamp(ts:Double):Unit={
    this.timestamp = ts
  }

  def setTime(time: Int): Unit = {
    this.time = time
  }

  def setWeek(week: Int): Unit = {
    this.week = week
  }

  def setTarget(target: String): Unit = {
    this.target = target
  }

  def setType(ty: String): Unit = {
    this.call_type = call_type
  }


  def getUser: String = {
    return this.user
  }

  def getDate: String = {
    return this.date
  }

  def getTimestamp:Double={
    return this.timestamp
  }

  def getTime: Int = {
    return this.time
  }

  def getWeek:Int={
    return this.week
  }

  def getTarget:String={
    return this.target
  }

  def getType:String={
    return this.call_type
  }


}