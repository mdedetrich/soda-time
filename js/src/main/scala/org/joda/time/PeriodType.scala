package org.joda.time

import java.io.Serializable
import org.joda.time.field.FieldUtils
import scalajs.js
import js.JSConverters._

object PeriodType {

  private val cTypes = new collection.mutable.HashMap[PeriodType, Any]()

  var YEAR_INDEX: Int = 0
  var MONTH_INDEX: Int = 1
  var WEEK_INDEX: Int = 2
  var DAY_INDEX: Int = 3
  var HOUR_INDEX: Int = 4
  var MINUTE_INDEX: Int = 5
  var SECOND_INDEX: Int = 6
  var MILLI_INDEX: Int = 7

  private var cStandard: PeriodType = _
  private var cYMDTime: PeriodType = _
  private var cYMD: PeriodType = _
  private var cYWDTime: PeriodType = _
  private var cYWD: PeriodType = _
  private var cYDTime: PeriodType = _
  private var cYD: PeriodType = _
  private var cDTime: PeriodType = _
  private var cTime: PeriodType = _
  private var cYears: PeriodType = _
  private var cMonths: PeriodType = _
  private var cWeeks: PeriodType = _
  private var cDays: PeriodType = _
  private var cHours: PeriodType = _
  private var cMinutes: PeriodType = _
  private var cSeconds: PeriodType = _
  private var cMillis: PeriodType = _

  def standard(): PeriodType = {
    var `type` = cStandard
    if (`type` == null) {
      `type` = new PeriodType("Standard", js.Array(DurationFieldType.years(), DurationFieldType.months(), DurationFieldType.weeks(), DurationFieldType.days(), DurationFieldType.hours(), DurationFieldType.minutes(), DurationFieldType.seconds(), DurationFieldType.millis()),
        js.Array(0, 1, 2, 3, 4, 5, 6, 7))
      cStandard = `type`
    }
    `type`
  }

  def yearMonthDayTime(): PeriodType = {
    var `type` = cYMDTime
    if (`type` == null) {
      `type` = new PeriodType("YearMonthDayTime", js.Array(DurationFieldType.years(), DurationFieldType.months(), DurationFieldType.days(), DurationFieldType.hours(), DurationFieldType.minutes(), DurationFieldType.seconds(), DurationFieldType.millis()),
        js.Array(0, 1, -1, 2, 3, 4, 5, 6))
      cYMDTime = `type`
    }
    `type`
  }

  def yearMonthDay(): PeriodType = {
    var `type` = cYMD
    if (`type` == null) {
      `type` = new PeriodType("YearMonthDay", js.Array(DurationFieldType.years(), DurationFieldType.months(), DurationFieldType.days()),
        js.Array(0, 1, -1, 2, -1, -1, -1, -1))
      cYMD = `type`
    }
    `type`
  }

  def yearWeekDayTime(): PeriodType = {
    var `type` = cYWDTime
    if (`type` == null) {
      `type` = new PeriodType("YearWeekDayTime", js.Array(DurationFieldType.years(), DurationFieldType.weeks(), DurationFieldType.days(), DurationFieldType.hours(), DurationFieldType.minutes(), DurationFieldType.seconds(), DurationFieldType.millis()),
        js.Array(0, -1, 1, 2, 3, 4, 5, 6))
      cYWDTime = `type`
    }
    `type`
  }

  def yearWeekDay(): PeriodType = {
    var `type` = cYWD
    if (`type` == null) {
      `type` = new PeriodType("YearWeekDay", js.Array(DurationFieldType.years(), DurationFieldType.weeks(), DurationFieldType.days()),
        js.Array(0, -1, 1, 2, -1, -1, -1, -1))
      cYWD = `type`
    }
    `type`
  }

  def yearDayTime(): PeriodType = {
    var `type` = cYDTime
    if (`type` == null) {
      `type` = new PeriodType("YearDayTime", js.Array(DurationFieldType.years(), DurationFieldType.days(), DurationFieldType.hours(), DurationFieldType.minutes(), DurationFieldType.seconds(), DurationFieldType.millis()),
        js.Array(0, -1, -1, 1, 2, 3, 4, 5))
      cYDTime = `type`
    }
    `type`
  }

  def yearDay(): PeriodType = {
    var `type` = cYD
    if (`type` == null) {
      `type` = new PeriodType("YearDay", js.Array(DurationFieldType.years(), DurationFieldType.days()),
        js.Array(0, -1, -1, 1, -1, -1, -1, -1))
      cYD = `type`
    }
    `type`
  }

  def dayTime(): PeriodType = {
    var `type` = cDTime
    if (`type` == null) {
      `type` = new PeriodType("DayTime", js.Array(DurationFieldType.days(), DurationFieldType.hours(), DurationFieldType.minutes(), DurationFieldType.seconds(), DurationFieldType.millis()),
        js.Array(-1, -1, -1, 0, 1, 2, 3, 4))
      cDTime = `type`
    }
    `type`
  }

  def time(): PeriodType = {
    var `type` = cTime
    if (`type` == null) {
      `type` = new PeriodType("Time", js.Array(DurationFieldType.hours(), DurationFieldType.minutes(), DurationFieldType.seconds(), DurationFieldType.millis()),
        js.Array(-1, -1, -1, -1, 0, 1, 2, 3))
      cTime = `type`
    }
    `type`
  }

  def years(): PeriodType = {
    var `type` = cYears
    if (`type` == null) {
      `type` = new PeriodType("Years", js.Array(DurationFieldType.years()), js.Array(0, -1, -1, -1, -1, -1, -1, -1))
      cYears = `type`
    }
    `type`
  }

  def months(): PeriodType = {
    var `type` = cMonths
    if (`type` == null) {
      `type` = new PeriodType("Months", js.Array(DurationFieldType.months()), js.Array(-1, 0, -1, -1, -1, -1, -1, -1))
      cMonths = `type`
    }
    `type`
  }

  def weeks(): PeriodType = {
    var `type` = cWeeks
    if (`type` == null) {
      `type` = new PeriodType("Weeks", js.Array(DurationFieldType.weeks()), js.Array(-1, -1, 0, -1, -1, -1, -1, -1))
      cWeeks = `type`
    }
    `type`
  }

  def days(): PeriodType = {
    var `type` = cDays
    if (`type` == null) {
      `type` = new PeriodType("Days", js.Array(DurationFieldType.days()), js.Array(-1, -1, -1, 0, -1, -1, -1, -1))
      cDays = `type`
    }
    `type`
  }

  def hours(): PeriodType = {
    var `type` = cHours
    if (`type` == null) {
      `type` = new PeriodType("Hours", js.Array(DurationFieldType.hours()), js.Array(-1, -1, -1, -1, 0, -1, -1, -1))
      cHours = `type`
    }
    `type`
  }

  def minutes(): PeriodType = {
    var `type` = cMinutes
    if (`type` == null) {
      `type` = new PeriodType("Minutes", js.Array(DurationFieldType.minutes()), js.Array(-1, -1, -1, -1, -1, 0, -1, -1))
      cMinutes = `type`
    }
    `type`
  }

  def seconds(): PeriodType = {
    var `type` = cSeconds
    if (`type` == null) {
      `type` = new PeriodType("Seconds", js.Array(DurationFieldType.seconds()), js.Array(-1, -1, -1, -1, -1, -1, 0, -1))
      cSeconds = `type`
    }
    `type`
  }

  def millis(): PeriodType = {
    var `type` = cMillis
    if (`type` == null) {
      `type` = new PeriodType("Millis", js.Array(DurationFieldType.millis()), js.Array(-1, -1, -1, -1, -1, -1, -1, 0))
      cMillis = `type`
    }
    `type`
  }

  def forFields(types: Array[DurationFieldType]): PeriodType = {
    forFields(types.toJSArray)
  }

  def forFields(types: js.Array[DurationFieldType]): PeriodType = {
    synchronized {
      if (types == null || types.length == 0) {
        throw new IllegalArgumentException("Types array must not be null or empty")
      }
      for (i <- types.indices if types(i) == null) {
        throw new IllegalArgumentException("Types array must not contain null")
      }
      val cache = cTypes
      if (cache.isEmpty) {
        cache.put(standard(), standard())
        cache.put(yearMonthDayTime(), yearMonthDayTime())
        cache.put(yearMonthDay(), yearMonthDay())
        cache.put(yearWeekDayTime(), yearWeekDayTime())
        cache.put(yearWeekDay(), yearWeekDay())
        cache.put(yearDayTime(), yearDayTime())
        cache.put(yearDay(), yearDay())
        cache.put(dayTime(), dayTime())
        cache.put(time(), time())
        cache.put(years(), years())
        cache.put(months(), months())
        cache.put(weeks(), weeks())
        cache.put(days(), days())
        cache.put(hours(), hours())
        cache.put(minutes(), minutes())
        cache.put(seconds(), seconds())
        cache.put(millis(), millis())
      }
      val inPartType = new PeriodType(null, types, null)
      val cached = cache(inPartType)
      if (cached.isInstanceOf[PeriodType]) {
        return cached.asInstanceOf[PeriodType]
      }
      if (cached != null) {
        throw new IllegalArgumentException("PeriodType does not support fields: " + cached)
      }
      var `type` = standard()
      
      val list = types.jsSlice(0)
      
      {
        val index = list.indexOf(DurationFieldType.years())
        if (index > 0) {
          list.remove(index)
        } else {
          `type` = `type`.withYearsRemoved()
        }
      }
      
      {
        val index = list.indexOf(DurationFieldType.months())
        if (index > 0) {
          list.remove(index)
        } else {
          `type` = `type`.withMonthsRemoved()
        }
      }

      {
        val index = list.indexOf(DurationFieldType.weeks())
        if (index > 0) {
          list.remove(index)
        } else {
          `type` = `type`.withWeeksRemoved()
        }
      }

      {
        val index = list.indexOf(DurationFieldType.days())
        if (index > 0) {
          list.remove(index)
        } else {
          `type` = `type`.withDaysRemoved()
        }
      }

      {
        val index = list.indexOf(DurationFieldType.hours())
        if (index > 0) {
          list.remove(index)
        } else {
          `type` = `type`.withHoursRemoved()
        }
      }

      {
        val index = list.indexOf(DurationFieldType.minutes())
        if (index > 0) {
          list.remove(index)
        } else {
          `type` = `type`.withMinutesRemoved()
        }
      }

      {
        val index = list.indexOf(DurationFieldType.seconds())
        if (index > 0) {
          list.remove(index)
        } else {
          `type` = `type`.withSecondsRemoved()
        }
      }

      {
        val index = list.indexOf(DurationFieldType.millis())
        if (index > 0) {
          list.remove(index)
        } else {
          `type` = `type`.withMillisRemoved()
        }
      }

      if (list.nonEmpty) {
        cache.put(inPartType, list)
        throw new IllegalArgumentException("PeriodType does not support fields: " + list)
      }
      val checkPartType = new PeriodType(null, `type`.iTypes, null)
      val checkedType = cache.get(checkPartType).asInstanceOf[PeriodType]
      if (checkedType != null) {
        cache.put(checkPartType, checkedType)
        return checkedType
      }
      cache.put(checkPartType, `type`)
      `type`
    }
  }
}

@SerialVersionUID(2274324892792009998L)
class PeriodType extends Serializable() {

  private var iName: String = _
  private var iTypes: js.Array[DurationFieldType] = _
  private var iIndices: js.Array[Int] = _
  
  protected def this(name: String, types: Array[DurationFieldType], indices: Array[Int]) {
    this()
    iName = name
    iTypes = types.toJSArray
    iIndices = indices.toJSArray
  }


  protected def this(name: String, types: js.Array[DurationFieldType], indices: js.Array[Int]) {
    this()
    iName = name
    iTypes = types
    iIndices = indices
  }

  def getName(): String = iName

  def size(): Int = iTypes.length

  def getFieldType(index: Int): DurationFieldType = iTypes(index)

  def isSupported(`type`: DurationFieldType): Boolean = indexOf(`type`) >= 0

  def indexOf(`type`: DurationFieldType): Int = {
    (0 until this.size()).find(iTypes(_) == `type`).getOrElse(-1)
  }

  override def toString(): String = "PeriodType[" + getName + "]"

  def getIndexedField(period: ReadablePeriod, index: Int): Int = {
    val realIndex = iIndices(index)
    if (realIndex == -1) 0 else period.getValue(realIndex)
  }

  def setIndexedField(period: ReadablePeriod,
                      index: Int,
                      values: js.Array[Int],
                      newValue: Int): Boolean = {
    val realIndex = iIndices(index)
    if (realIndex == -1) {
      throw new UnsupportedOperationException("Field is not supported")
    }
    values(realIndex) = newValue
    true
  }

  def setIndexedField(period: ReadablePeriod,
                      index: Int,
                      values: Array[Int],
                      newValue: Int): Boolean = setIndexedField(period,index,values.toJSArray,newValue)

  def addIndexedField(period: ReadablePeriod,
                      index: Int,
                      values: js.Array[Int],
                      valueToAdd: Int): Boolean = {
    if (valueToAdd == 0) {
      return false
    }
    val realIndex = iIndices(index)
    if (realIndex == -1) {
      throw new UnsupportedOperationException("Field is not supported")
    }
    values(realIndex) = FieldUtils.safeAdd(values(realIndex), valueToAdd)
    true
  }

  def addIndexedField(period: ReadablePeriod,
                      index: Int,
                      values: Array[Int],
                      valueToAdd: Int): Boolean = addIndexedField(period,index,values.toJSArray,valueToAdd)

  def withYearsRemoved(): PeriodType = withFieldRemoved(0, "NoYears")

  def withMonthsRemoved(): PeriodType = withFieldRemoved(1, "NoMonths")

  def withWeeksRemoved(): PeriodType = withFieldRemoved(2, "NoWeeks")

  def withDaysRemoved(): PeriodType = withFieldRemoved(3, "NoDays")

  def withHoursRemoved(): PeriodType = withFieldRemoved(4, "NoHours")

  def withMinutesRemoved(): PeriodType = withFieldRemoved(5, "NoMinutes")

  def withSecondsRemoved(): PeriodType = withFieldRemoved(6, "NoSeconds")

  def withMillisRemoved(): PeriodType = withFieldRemoved(7, "NoMillis")

  private def withFieldRemoved(indicesIndex: Int, name: String): PeriodType = {
    val fieldIndex = iIndices(indicesIndex)
    if (fieldIndex == -1) {
      return this
    }
    val types = js.Array[DurationFieldType]()
    for (i <- iTypes.indices) {
      if (i < fieldIndex) {
        types(i) = iTypes(i)
      } else if (i > fieldIndex) {
        types(i - 1) = iTypes(i)
      }
    }
    val indices = js.Array[Int]()
    for (i <- indices.indices) {
      indices(i) = if (i < indicesIndex) iIndices(i) else if (i > indicesIndex) if (iIndices(i) == -1) -1 else iIndices(i) - 1 else -1
    }
    new PeriodType(getName + name, types, indices)
  }

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    }
    if (obj.isInstanceOf[PeriodType] == false) {
      return false
    }
    val other = obj.asInstanceOf[PeriodType]
    iTypes.sameElements(iTypes) // Need to have a look at this
  }

  override def hashCode(): Int = {
    var hash = 0
    for (i <- iTypes.indices) {
      hash += iTypes(i).hashCode
    }
    hash
  }
}
