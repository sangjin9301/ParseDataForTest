class Location {
  var user:String = ""
  var date:String = ""
  var timestamp: Double = 0
  var time:Int = 0
  var week:Int = 0
  var x:Double = 0
  var y:Double = 0
  
  def setUser(user:String):Unit=
  {
    this.user = user
  }
  
  def setDate(date:String):Unit=
  {
    this.date = date
  }

  def setTimestamp(ts:Double):Unit={
    this.timestamp = ts
  }

  def setTime(time:Int):Unit=
  {
    this.time = time
  }

  def setWeek(week:Int):Unit=
  {
    this.week = week
  }
  
  def setX(x:Double):Unit=
  {
    this.x = x
  }
  
  def setY(y:Double):Unit=
  {
    this.y = y
  }



  //get method
  def getUser:String=
  {
    return this.user
  }
  
  def getDate:String=
  {
    return this.date
  }

  def getTimestamp:Double={
    return this.timestamp
  }

  def getTime:Int=
  {
    return this.time
  }

  def getWeek:Int=
  {
    return this.week
  }
  
  def getX:Double=
  {
    return this.x
  }
  
  def getY:Double=
  {
    return this.y
  }
}