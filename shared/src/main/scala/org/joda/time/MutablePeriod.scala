package org.joda.time

import java.io.Serializable
import org.joda.convert.FromString
import org.joda.time.base.BasePeriod
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISOPeriodFormat
import org.joda.time.format.PeriodFormatter

object MutablePeriod {

  @FromString
  def parse(str: String): MutablePeriod = parse(str, ISOPeriodFormat.standard())

  def parse(str: String, formatter: PeriodFormatter): MutablePeriod = {
    formatter.parsePeriod(str).toMutablePeriod()
  }
}

@SerialVersionUID(3436451121567212165L)
class MutablePeriod extends BasePeriod(0L, null, null) with ReadWritablePeriod with Cloneable with Serializable {

  def this(`type`: PeriodType) {
    this()
    super.auxConstructor(0L, `type`, null)
  }

  def this(hours: Int,
           minutes: Int,
           seconds: Int,
           millis: Int) {
    this()
    super.auxConstructor(0, 0, 0, 0, hours, minutes, seconds, millis, PeriodType.standard())
  }

  def this(years: Int,
           months: Int,
           weeks: Int,
           days: Int,
           hours: Int,
           minutes: Int,
           seconds: Int,
           millis: Int) {
    this()
    super.auxConstructor(years, months, weeks, days, hours, minutes, seconds, millis, PeriodType.standard())
  }

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
    super.auxConstructor(years, months, weeks, days, hours, minutes, seconds, millis, `type`)
  }

  def this(duration: Long) {
    this()
    super.auxConstructor(duration)
  }

  def this(duration: Long, `type`: PeriodType) {
    this()
    super.auxConstructor(duration, `type`, null)
  }

  def this(duration: Long, chronology: Chronology) {
    this()
    super.auxConstructor(duration,null,chronology)
  }

  def this(duration: Long, `type`: PeriodType, chronology: Chronology) {
    this()
    super.auxConstructor(duration,`type`,chronology)
  }

  def this(startInstant: Long, endInstant: Long) {
    this()
    super.auxConstructor(startInstant,endInstant,null,null)
  }

  def this(startInstant: Long, endInstant: Long, `type`: PeriodType) {
    this()
    super.auxConstructor(startInstant, endInstant, `type`, null)
  }

  def this(startInstant: Long, endInstant: Long, chrono: Chronology) {
    this()
    super.auxConstructor(startInstant, endInstant, null, chrono)
  }

  def this(startInstant: Long,
           endInstant: Long,
           `type`: PeriodType,
           chrono: Chronology) {
    this()
    super.auxConstructor(startInstant, endInstant, `type`, chrono)
  }

  def this(startInstant: ReadableInstant, endInstant: ReadableInstant) {
    this()
    super.auxConstructor(startInstant, endInstant, null)
  }

  def this(startInstant: ReadableInstant, endInstant: ReadableInstant, `type`: PeriodType) {
    this()
    super.auxConstructor(startInstant, endInstant, `type`)
  }

  def this(startInstant: ReadableInstant, duration: ReadableDuration) {
    this()
    super.auxConstructor(startInstant, duration, null)
  }

  def this(startInstant: ReadableInstant, duration: ReadableDuration, `type`: PeriodType) {
    this()
    super.auxConstructor(startInstant, duration, `type`)
  }

  def this(duration: ReadableDuration, endInstant: ReadableInstant) {
    this()
    super.auxConstructor(duration, endInstant, null)
  }

  def this(duration: ReadableDuration, endInstant: ReadableInstant, `type`: PeriodType) {
    this()
    super.auxConstructor(duration, endInstant, `type`)
  }

  def this(period: AnyRef) {
    this()
    super.auxConstructor(period, null, null)
  }

  def this(period: AnyRef, `type`: PeriodType) {
    this()
    super.auxConstructor(period, `type`, null)
  }

  def this(period: AnyRef, chrono: Chronology) {
    this()
    super.auxConstructor(period, null, chrono)
  }

  def this(period: AnyRef, `type`: PeriodType, chrono: Chronology) {
    this()
    super.auxConstructor(period, `type`, chrono)
  }

  def clear() {
    super.setValues(Array.ofDim[Int](size))
  }

  override def setValue(index: Int, value: Int) {
    super.setValue(index, value)
  }

  def set(field: DurationFieldType, value: Int) {
    super.setField(field, value)
  }

  override def setPeriod(period: ReadablePeriod) {
    super.setPeriod(period)
  }

  override def setPeriod(years: Int,
                months: Int,
                weeks: Int,
                days: Int,
                hours: Int,
                minutes: Int,
                seconds: Int,
                millis: Int) {
    super.setPeriod(years, months, weeks, days, hours, minutes, seconds, millis)
  }

  def setPeriod(interval: ReadableInterval) {
    if (interval == null) {
      setPeriod(0L)
    } else {
      val chrono = DateTimeUtils.getChronology(interval.getChronology)
      setPeriod(interval.getStartMillis, interval.getEndMillis, chrono)
    }
  }

  def setPeriod(start: ReadableInstant, end: ReadableInstant) {
    if (start == end) {
      setPeriod(0L)
    } else {
      val startMillis = DateTimeUtils.getInstantMillis(start)
      val endMillis = DateTimeUtils.getInstantMillis(end)
      val chrono = DateTimeUtils.getIntervalChronology(start, end)
      setPeriod(startMillis, endMillis, chrono)
    }
  }

  def setPeriod(startInstant: Long, endInstant: Long) {
    setPeriod(startInstant, endInstant, null)
  }

  def setPeriod(startInstant: Long, endInstant: Long, chrono: Chronology) {
    var _chrono = chrono
    _chrono = DateTimeUtils.getChronology(_chrono)
    setValues(_chrono.get(this, startInstant, endInstant))
  }

  def setPeriod(duration: ReadableDuration) {
    setPeriod(duration, null)
  }

  def setPeriod(duration: ReadableDuration, chrono: Chronology) {
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    setPeriod(durationMillis, chrono)
  }

  def setPeriod(duration: Long) {
    setPeriod(duration, null)
  }

  def setPeriod(duration: Long, chrono: Chronology) {
    var _chrono = chrono
    _chrono = DateTimeUtils.getChronology(_chrono)
    setValues(_chrono.get(this, duration))
  }

  def add(field: DurationFieldType, value: Int) {
    super.addField(field, value)
  }

  def add(period: ReadablePeriod) {
    super.addPeriod(period)
  }

  def add(years: Int,
          months: Int,
          weeks: Int,
          days: Int,
          hours: Int,
          minutes: Int,
          seconds: Int,
          millis: Int) {
    setPeriod(FieldUtils.safeAdd(getYears, years), FieldUtils.safeAdd(getMonths, months), FieldUtils.safeAdd(getWeeks,
      weeks), FieldUtils.safeAdd(getDays, days), FieldUtils.safeAdd(getHours, hours), FieldUtils.safeAdd(getMinutes,
      minutes), FieldUtils.safeAdd(getSeconds, seconds), FieldUtils.safeAdd(getMillis, millis))
  }

  def add(interval: ReadableInterval) {
    if (interval != null) {
      add(interval.toPeriod(getPeriodType))
    }
  }

  def add(duration: ReadableDuration) {
    if (duration != null) {
      add(new Period(duration.getMillis, getPeriodType))
    }
  }

  def add(duration: Long) {
    add(new Period(duration, getPeriodType))
  }

  def add(duration: Long, chrono: Chronology) {
    add(new Period(duration, getPeriodType, chrono))
  }

  override def mergePeriod(period: ReadablePeriod) {
    super.mergePeriod(period)
  }

  def getYears(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.YEAR_INDEX)
  }

  def getMonths(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.MONTH_INDEX)
  }

  def getWeeks(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.WEEK_INDEX)
  }

  def getDays(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.DAY_INDEX)
  }

  def getHours(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.HOUR_INDEX)
  }

  def getMinutes(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.MINUTE_INDEX)
  }

  def getSeconds(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.SECOND_INDEX)
  }

  def getMillis(): Int = {
    getPeriodType.getIndexedField(this, PeriodType.MILLI_INDEX)
  }

  def setYears(years: Int) {
    super.setField(DurationFieldType.years(), years)
  }

  def addYears(years: Int) {
    super.addField(DurationFieldType.years(), years)
  }

  def setMonths(months: Int) {
    super.setField(DurationFieldType.months(), months)
  }

  def addMonths(months: Int) {
    super.addField(DurationFieldType.months(), months)
  }

  def setWeeks(weeks: Int) {
    super.setField(DurationFieldType.weeks(), weeks)
  }

  def addWeeks(weeks: Int) {
    super.addField(DurationFieldType.weeks(), weeks)
  }

  def setDays(days: Int) {
    super.setField(DurationFieldType.days(), days)
  }

  def addDays(days: Int) {
    super.addField(DurationFieldType.days(), days)
  }

  def setHours(hours: Int) {
    super.setField(DurationFieldType.hours(), hours)
  }

  def addHours(hours: Int) {
    super.addField(DurationFieldType.hours(), hours)
  }

  def setMinutes(minutes: Int) {
    super.setField(DurationFieldType.minutes(), minutes)
  }

  def addMinutes(minutes: Int) {
    super.addField(DurationFieldType.minutes(), minutes)
  }

  def setSeconds(seconds: Int) {
    super.setField(DurationFieldType.seconds(), seconds)
  }

  def addSeconds(seconds: Int) {
    super.addField(DurationFieldType.seconds(), seconds)
  }

  def setMillis(millis: Int) {
    super.setField(DurationFieldType.millis(), millis)
  }

  def addMillis(millis: Int) {
    super.addField(DurationFieldType.millis(), millis)
  }

  def copy(): MutablePeriod = clone().asInstanceOf[MutablePeriod]

  override def clone(): AnyRef = super.clone()
}
