package org.joda.time.base

import java.io.Serializable
import org.joda.time.Chronology
import org.joda.time.DateTimeUtils
import org.joda.time.Duration
import org.joda.time.DurationFieldType
import org.joda.time.MutablePeriod
import org.joda.time.PeriodType
import org.joda.time.ReadWritablePeriod
import org.joda.time.ReadableDuration
import org.joda.time.ReadableInstant
import org.joda.time.ReadablePartial
import org.joda.time.ReadablePeriod
import org.joda.time.chrono.ISOChronology
import org.joda.time.convert.ConverterManager
import org.joda.time.field.FieldUtils
import BasePeriod._

object BasePeriod {

  private val DUMMY_PERIOD = new AbstractPeriod() {

    def getValue(index: Int): Int = 0

    def getPeriodType(): PeriodType = PeriodType.time()
  }
}

@SerialVersionUID(-2110953284060001145L)
abstract class BasePeriod extends AbstractPeriod() with ReadablePeriod with Serializable {

  
  private var iValues: Array[Int] = null
  private var iType: PeriodType = null
  
  def this(years: Int,
           months: Int,
           weeks: Int,
           days: Int,
           hours: Int,
           minutes: Int,
           seconds: Int,
           millis: Int,
           `type`: PeriodType) {
    this()
    iValues = setPeriodInternal(years, months, weeks, days, hours, minutes, seconds, millis)
    iType = checkPeriodType(`type`)
  }

  protected def this(startInstant: Long,
                     endInstant: Long,
                     `type`: PeriodType,
                     chrono: Chronology) {
    this()
    var _type: PeriodType = `type`
    var _chrono: Chronology = chrono
    _type = checkPeriodType(_type)
    _chrono = DateTimeUtils.getChronology(_chrono)
    iType = _type
    iValues = _chrono.get(this, startInstant, endInstant)
  }

  protected def this(startInstant: ReadableInstant, endInstant: ReadableInstant, `type`: PeriodType) {
    this()
    var _type:PeriodType = `type`
    _type = checkPeriodType(_type)
    if (startInstant == null && endInstant == null) {
      iType = _type
      iValues = Array.ofDim[Int](size)
    } else {
      val startMillis = DateTimeUtils.getInstantMillis(startInstant)
      val endMillis = DateTimeUtils.getInstantMillis(endInstant)
      val chrono = DateTimeUtils.getIntervalChronology(startInstant, endInstant)
      iType = _type
      iValues = chrono.get(this, startMillis, endMillis)
    }
  }

  protected def this(start: ReadablePartial, end: ReadablePartial, `type`: PeriodType) {
    this()
    var _type: PeriodType = `type`
    if (start == null || end == null) {
      throw new IllegalArgumentException("ReadablePartial objects must not be null")
    }
    if (start.isInstanceOf[BaseLocal] && end.isInstanceOf[BaseLocal] &&
      start.getClass == end.getClass) {
      _type = checkPeriodType(_type)
      val startMillis = start.asInstanceOf[BaseLocal].getLocalMillis()
      val endMillis = end.asInstanceOf[BaseLocal].getLocalMillis()
      var chrono = start.getChronology
      chrono = DateTimeUtils.getChronology(chrono)
      iType = _type
      iValues = chrono.get(this, startMillis, endMillis)
    } else {
      if (start.size != end.size) {
        throw new IllegalArgumentException("ReadablePartial objects must have the same set of fields")
      }
      for (i <- 0 until start.size() if start.getFieldType(i) != end.getFieldType(i)) {
        throw new IllegalArgumentException("ReadablePartial objects must have the same set of fields")
      }
      if (DateTimeUtils.isContiguous(start) == false) {
        throw new IllegalArgumentException("ReadablePartial objects must be contiguous")
      }
      iType = checkPeriodType(_type)
      val chrono = DateTimeUtils.getChronology(start.getChronology).withUTC()
      iValues = chrono.get(this, chrono.set(start, 0L), chrono.set(end, 0L))
    }
  }

  protected def this(startInstant: ReadableInstant, duration: ReadableDuration, `type`: PeriodType) {
    this()
    var _type: PeriodType = `type`
    _type = checkPeriodType(_type)
    val startMillis = DateTimeUtils.getInstantMillis(startInstant)
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    val endMillis = FieldUtils.safeAdd(startMillis, durationMillis)
    val chrono = DateTimeUtils.getInstantChronology(startInstant)
    iType = _type
    iValues = chrono.get(this, startMillis, endMillis)
  }

  protected def this(duration: ReadableDuration, endInstant: ReadableInstant, `type`: PeriodType) {
    this()
    var _type: PeriodType = `type`
    _type = checkPeriodType(_type)
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    val endMillis = DateTimeUtils.getInstantMillis(endInstant)
    val startMillis = FieldUtils.safeSubtract(endMillis, durationMillis)
    val chrono = DateTimeUtils.getInstantChronology(endInstant)
    iType = _type
    iValues = chrono.get(this, startMillis, endMillis)
  }

  protected def this(duration: Long) {
    this()
    iType = PeriodType.standard()
    val values = ISOChronology.getInstanceUTC.get(DUMMY_PERIOD, duration)
    iValues = Array.ofDim[Int](8)
    System.arraycopy(values, 0, iValues, 4, 4)
  }

  protected def this(duration: Long, `type`: PeriodType, chrono: Chronology) {
    this()
    var _type = `type`
    var _chrono = chrono
    _type = checkPeriodType(_type)
    _chrono = DateTimeUtils.getChronology(_chrono)
    iType = _type
    iValues = _chrono.get(this, duration)
  }

  protected def this(period: AnyRef, `type`: PeriodType, chrono: Chronology) {
    this()
    var _type = `type`
    var _chrono = chrono
    val converter = ConverterManager.getInstance.getPeriodConverter(period)
    _type = if (_type == null) converter.getPeriodType(period) else _type
    _type = checkPeriodType(_type)
    iType = _type
    if (this.isInstanceOf[ReadWritablePeriod]) {
      iValues = Array.ofDim[Int](size)
      _chrono = DateTimeUtils.getChronology(_chrono)
      converter.setInto(this.asInstanceOf[ReadWritablePeriod], period, _chrono)
    } else {
      iValues = new MutablePeriod(period, _type, _chrono).getValues
    }
  }

  protected def this(values: Array[Int], `type`: PeriodType) {
    this()
    iType = `type`
    iValues = values
  }
  
  protected def auxConstructor(duration: Long, `type`: PeriodType, chrono: Chronology): Unit = {
    var _type = `type`
    var _chrono = chrono
    _type = checkPeriodType(_type)
    _chrono = DateTimeUtils.getChronology(_chrono)
    iType = _type
    iValues = _chrono.get(this, duration)
  }
  
  protected def auxConstructor(years: Int,
                               months: Int,
                               weeks: Int,
                               days: Int,
                               hours: Int,
                               minutes: Int,
                               seconds: Int,
                               millis: Int,
                               `type`: PeriodType): Unit = {
    iValues = setPeriodInternal(years, months, weeks, days, hours, minutes, seconds, millis)
    iType = checkPeriodType(`type`)
  }
  
  protected def auxConstructor(duration: Long): Unit = {
    iType = PeriodType.standard()
    val values = ISOChronology.getInstanceUTC.get(DUMMY_PERIOD, duration)
    iValues = Array.ofDim[Int](8)
    System.arraycopy(values, 0, iValues, 4, 4)
  }
  
  protected def auxConstructor(startInstant: Long,
                               endInstant: Long,
                               `type`: PeriodType,
                               chrono: Chronology): Unit = {
    var _type: PeriodType = `type`
    var _chrono: Chronology = chrono
    _type = checkPeriodType(_type)
    _chrono = DateTimeUtils.getChronology(_chrono)
    iType = _type
    iValues = _chrono.get(this, startInstant, endInstant)
  }
  
  protected def auxConstructor(startInstant: ReadableInstant, endInstant: ReadableInstant, `type`: PeriodType): Unit = {
    var _type:PeriodType = `type`
    _type = checkPeriodType(_type)
    if (startInstant == null && endInstant == null) {
      iType = _type
      iValues = Array.ofDim[Int](size)
    } else {
      val startMillis = DateTimeUtils.getInstantMillis(startInstant)
      val endMillis = DateTimeUtils.getInstantMillis(endInstant)
      val chrono = DateTimeUtils.getIntervalChronology(startInstant, endInstant)
      iType = _type
      iValues = chrono.get(this, startMillis, endMillis)
    }
  }
  
  protected def auxConstructor(startInstant: ReadableInstant, duration: ReadableDuration, `type`: PeriodType): Unit = {
    var _type: PeriodType = `type`
    _type = checkPeriodType(_type)
    val startMillis = DateTimeUtils.getInstantMillis(startInstant)
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    val endMillis = FieldUtils.safeAdd(startMillis, durationMillis)
    val chrono = DateTimeUtils.getInstantChronology(startInstant)
    iType = _type
    iValues = chrono.get(this, startMillis, endMillis)
  }
  
  protected def auxConstructor(duration: ReadableDuration, endInstant: ReadableInstant, `type`: PeriodType): Unit = {
    var _type: PeriodType = `type`
    _type = checkPeriodType(_type)
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    val endMillis = DateTimeUtils.getInstantMillis(endInstant)
    val startMillis = FieldUtils.safeSubtract(endMillis, durationMillis)
    val chrono = DateTimeUtils.getInstantChronology(endInstant)
    iType = _type
    iValues = chrono.get(this, startMillis, endMillis)
  }
  
  protected def auxConstructor(period: AnyRef, `type`: PeriodType, chrono: Chronology): Unit = {
    var _type = `type`
    var _chrono = chrono
    val converter = ConverterManager.getInstance.getPeriodConverter(period)
    _type = (if (_type == null) converter.getPeriodType(period) else _type)
    _type = checkPeriodType(_type)
    iType = _type
    if (this.isInstanceOf[ReadWritablePeriod]) {
      iValues = Array.ofDim[Int](size)
      _chrono = DateTimeUtils.getChronology(_chrono)
      converter.setInto(this.asInstanceOf[ReadWritablePeriod], period, _chrono)
    } else {
      iValues = new MutablePeriod(period, _type, _chrono).getValues
    }
  }
  
  protected def auxConstructor(start: ReadablePartial, end: ReadablePartial, `type`: PeriodType): Unit = {
    var _type: PeriodType = `type`
    if (start == null || end == null) {
      throw new IllegalArgumentException("ReadablePartial objects must not be null")
    }
    if (start.isInstanceOf[BaseLocal] && end.isInstanceOf[BaseLocal] &&
      start.getClass == end.getClass) {
      _type = checkPeriodType(_type)
      val startMillis = start.asInstanceOf[BaseLocal].getLocalMillis()
      val endMillis = end.asInstanceOf[BaseLocal].getLocalMillis()
      var chrono = start.getChronology
      chrono = DateTimeUtils.getChronology(chrono)
      iType = _type
      iValues = chrono.get(this, startMillis, endMillis)
    } else {
      if (start.size != end.size) {
        throw new IllegalArgumentException("ReadablePartial objects must have the same set of fields")
      }
      for (i <- 0 until start.size() if start.getFieldType(i) != end.getFieldType(i)) {
        throw new IllegalArgumentException("ReadablePartial objects must have the same set of fields")
      }
      if (DateTimeUtils.isContiguous(start) == false) {
        throw new IllegalArgumentException("ReadablePartial objects must be contiguous")
      }
      iType = checkPeriodType(_type)
      val chrono = DateTimeUtils.getChronology(start.getChronology).withUTC()
      iValues = chrono.get(this, chrono.set(start, 0L), chrono.set(end, 0L))
    }
  }
  
  protected def auxConstructor(values: Array[Int], `type`: PeriodType): Unit = {
    iType = `type`
    iValues = values
  }

  protected def checkPeriodType(`type`: PeriodType): PeriodType = DateTimeUtils.getPeriodType(`type`)

  def getPeriodType(): PeriodType = iType

  def getValue(index: Int): Int = iValues(index)

  def toDurationFrom(startInstant: ReadableInstant): Duration = {
    val startMillis = DateTimeUtils.getInstantMillis(startInstant)
    val chrono = DateTimeUtils.getInstantChronology(startInstant)
    val endMillis = chrono.add(this, startMillis, 1)
    new Duration(startMillis, endMillis)
  }

  def toDurationTo(endInstant: ReadableInstant): Duration = {
    val endMillis = DateTimeUtils.getInstantMillis(endInstant)
    val chrono = DateTimeUtils.getInstantChronology(endInstant)
    val startMillis = chrono.add(this, endMillis, -1)
    new Duration(startMillis, endMillis)
  }

  private def checkAndUpdate(`type`: DurationFieldType, values: Array[Int], newValue: Int) {
    val index = indexOf(`type`)
    if (index == -1) {
      if (newValue != 0) {
        throw new IllegalArgumentException("Period does not support field '" + `type`.getName + "'")
      }
    } else {
      values(index) = newValue
    }
  }

  protected def setPeriod(period: ReadablePeriod) {
    if (period == null) {
      setValues(Array.ofDim[Int](size))
    } else {
      setPeriodInternal(period)
    }
  }

  private def setPeriodInternal(period: ReadablePeriod) {
    val newValues = Array.ofDim[Int](size)
    for (i <- 0 until period.size()) {
      val `type` = period.getFieldType(i)
      val value = period.getValue(i)
      checkAndUpdate(`type`, newValues, value)
    }
    setValues(newValues)
  }

  protected def setPeriod(years: Int,
                          months: Int,
                          weeks: Int,
                          days: Int,
                          hours: Int,
                          minutes: Int,
                          seconds: Int,
                          millis: Int) {
    val newValues = setPeriodInternal(years, months, weeks, days, hours, minutes, seconds, millis)
    setValues(newValues)
  }

  private def setPeriodInternal(years: Int,
                                months: Int,
                                weeks: Int,
                                days: Int,
                                hours: Int,
                                minutes: Int,
                                seconds: Int,
                                millis: Int): Array[Int] = {
    val newValues = Array.ofDim[Int](size)
    checkAndUpdate(DurationFieldType.years(), newValues, years)
    checkAndUpdate(DurationFieldType.months(), newValues, months)
    checkAndUpdate(DurationFieldType.weeks(), newValues, weeks)
    checkAndUpdate(DurationFieldType.days(), newValues, days)
    checkAndUpdate(DurationFieldType.hours(), newValues, hours)
    checkAndUpdate(DurationFieldType.minutes(), newValues, minutes)
    checkAndUpdate(DurationFieldType.seconds(), newValues, seconds)
    checkAndUpdate(DurationFieldType.millis(), newValues, millis)
    newValues
  }

  protected def setField(field: DurationFieldType, value: Int) {
    setFieldInto(iValues, field, value)
  }

  protected def setFieldInto(values: Array[Int], field: DurationFieldType, value: Int) {
    val index = indexOf(field)
    if (index == -1) {
      if (value != 0 || field == null) {
        throw new IllegalArgumentException("Period does not support field '" + field + "'")
      }
    } else {
      values(index) = value
    }
  }

  protected def addField(field: DurationFieldType, value: Int) {
    addFieldInto(iValues, field, value)
  }

  protected def addFieldInto(values: Array[Int], field: DurationFieldType, value: Int) {
    val index = indexOf(field)
    if (index == -1) {
      if (value != 0 || field == null) {
        throw new IllegalArgumentException("Period does not support field '" + field + "'")
      }
    } else {
      values(index) = FieldUtils.safeAdd(values(index), value)
    }
  }

  protected def mergePeriod(period: ReadablePeriod) {
    if (period != null) {
      setValues(mergePeriodInto(getValues, period))
    }
  }

  protected def mergePeriodInto(values: Array[Int], period: ReadablePeriod): Array[Int] = {
    for (i <- 0 until period.size()) {
      val `type` = period.getFieldType(i)
      val value = period.getValue(i)
      checkAndUpdate(`type`, values, value)
    }
    values
  }

  protected def addPeriod(period: ReadablePeriod) {
    if (period != null) {
      setValues(addPeriodInto(getValues, period))
    }
  }

  protected def addPeriodInto(values: Array[Int], period: ReadablePeriod): Array[Int] = {
    for (i <- 0 until period.size()) {
      val `type` = period.getFieldType(i)
      val value = period.getValue(i)
      if (value != 0) {
        val index = indexOf(`type`)
        if (index == -1) {
          throw new IllegalArgumentException("Period does not support field '" + `type`.getName + "'")
        } else {
          values(index) = FieldUtils.safeAdd(getValue(index), value)
        }
      }
    }
    values
  }

  protected def setValue(index: Int, value: Int) {
    iValues(index) = value
  }

  protected def setValues(values: Array[Int]) {
    System.arraycopy(values, 0, iValues, 0, iValues.length)
  }
}
