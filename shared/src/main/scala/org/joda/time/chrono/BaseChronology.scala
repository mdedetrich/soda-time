package org.joda.time.chrono

import java.io.Serializable
import org.joda.time.Chronology
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeZone
import org.joda.time.DurationField
import org.joda.time.DurationFieldType
import org.joda.time.IllegalFieldValueException
import org.joda.time.ReadablePartial
import org.joda.time.ReadablePeriod
import org.joda.time.field.FieldUtils
import org.joda.time.field.UnsupportedDateTimeField
import org.joda.time.field.UnsupportedDurationField

@SerialVersionUID(-7310865996721419676L)
abstract class BaseChronology protected () extends Chronology() with Serializable {

  def getZone(): DateTimeZone

  def withUTC(): Chronology

  def withZone(zone: DateTimeZone): Chronology

  def getDateTimeMillis(year: Int,
                        monthOfYear: Int,
                        dayOfMonth: Int,
                        millisOfDay: Int): Long = {
    var instant = this.year().set(0, year)
    instant = this.monthOfYear().set(instant, monthOfYear)
    instant = this.dayOfMonth().set(instant, dayOfMonth)
    this.millisOfDay().set(instant, millisOfDay)
  }

  def getDateTimeMillis(year: Int,
                        monthOfYear: Int,
                        dayOfMonth: Int,
                        hourOfDay: Int,
                        minuteOfHour: Int,
                        secondOfMinute: Int,
                        millisOfSecond: Int): Long = {
    var instant = this.year().set(0, year)
    instant = this.monthOfYear().set(instant, monthOfYear)
    instant = this.dayOfMonth().set(instant, dayOfMonth)
    instant = this.hourOfDay().set(instant, hourOfDay)
    instant = this.minuteOfHour().set(instant, minuteOfHour)
    instant = this.secondOfMinute().set(instant, secondOfMinute)
    this.millisOfSecond().set(instant, millisOfSecond)
  }

  def getDateTimeMillis(instant: Long,
                        hourOfDay: Int,
                        minuteOfHour: Int,
                        secondOfMinute: Int,
                        millisOfSecond: Int): Long = {
    var _instant = instant
    _instant = this.hourOfDay().set(_instant, hourOfDay)
    _instant = this.minuteOfHour().set(_instant, minuteOfHour)
    _instant = this.secondOfMinute().set(_instant, secondOfMinute)
    this.millisOfSecond().set(_instant, millisOfSecond)
  }

  def validate(partial: ReadablePartial, values: Array[Int]) {
    val size = partial.size
    for (i <- 0 until size) {
      val value = values(i)
      val field = partial.getField(i)
      if (value < field.getMinimumValue) {
        throw IllegalFieldValueException.create(field.getType, Integer.valueOf(value), Integer.valueOf(field.getMinimumValue),
          null)
      }
      if (value > field.getMaximumValue) {
        throw IllegalFieldValueException.create(field.getType, Integer.valueOf(value), null, Integer.valueOf(field.getMaximumValue))
      }
    }
    for (i <- 0 until size) {
      val value = values(i)
      val field = partial.getField(i)
      if (value < field.getMinimumValue(partial, values)) {
        throw IllegalFieldValueException.create(field.getType, Integer.valueOf(value), Integer.valueOf(field.getMinimumValue(partial,
          values)), null)
      }
      if (value > field.getMaximumValue(partial, values)) {
        throw IllegalFieldValueException.create(field.getType, Integer.valueOf(value), null, Integer.valueOf(field.getMaximumValue(partial,
          values)))
      }
    }
  }

  def get(partial: ReadablePartial, instant: Long): Array[Int] = {
    val size = partial.size
    val values = Array.ofDim[Int](size)
    for (i <- 0 until size) {
      values(i) = partial.getFieldType(i).getField(this).get(instant)
    }
    values
  }

  def set(partial: ReadablePartial, instant: Long): Long = {
    var _instant = instant
    for (i <- 0 until partial.size) {
      _instant = partial.getFieldType(i).getField(this).set(_instant, partial.getValue(i))
    }
    _instant
  }

  def get(period: ReadablePeriod, startInstant: Long, endInstant: Long): Array[Int] = {
    var _startInstant:Long = startInstant
    val size = period.size
    val values = Array.ofDim[Int](size)
    if (_startInstant != endInstant) {
      for (i <- 0 until size) {
        val field = period.getFieldType(i).getField(this)
        val value = field.getDifference(endInstant, _startInstant)
        if (value != 0) {
          _startInstant = field.add(_startInstant, value)
        }
        values(i) = value
      }
    }
    values
  }

  def get(period: ReadablePeriod, duration: Long): Array[Int] = {
    val size = period.size
    val values = Array.ofDim[Int](size)
    if (duration != 0) {
      var current:Long = 0
      for (i <- 0 until size) {
        val field = period.getFieldType(i).getField(this)
        if (field.isPrecise) {
          val value = field.getDifference(duration, current)
          current = field.add(current, value)
          values(i) = value
        }
      }
    }
    values
  }

  def add(period: ReadablePeriod, instant: Long, scalar: Int): Long = {
    var _instant:Long = instant
    if (scalar != 0 && period != null) {
      for (i <- 0 until period.size()) {
        val value = period.getValue(i)
        if (value != 0) {
          _instant = period.getFieldType(i).getField(this).add(_instant, value * scalar)
        }
      }
    }
    _instant
  }

  def add(instant: Long, duration: Long, scalar: Int): Long = {
    if (duration == 0 || scalar == 0) {
      return instant
    }
    val add = FieldUtils.safeMultiply(duration, scalar)
    FieldUtils.safeAdd(instant, add)
  }

  def millis(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.millis())
  }

  def millisOfSecond(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.millisOfSecond(), millis())
  }

  def millisOfDay(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.millisOfDay(), millis())
  }

  def seconds(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.seconds())
  }

  def secondOfMinute(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.secondOfMinute(), seconds())
  }

  def secondOfDay(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.secondOfDay(), seconds())
  }

  def minutes(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.minutes())
  }

  def minuteOfHour(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.minuteOfHour(), minutes())
  }

  def minuteOfDay(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.minuteOfDay(), minutes())
  }

  def hours(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.hours())
  }

  def hourOfDay(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.hourOfDay(), hours())
  }

  def clockhourOfDay(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.clockhourOfDay(), hours())
  }

  def halfdays(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.halfdays())
  }

  def hourOfHalfday(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.hourOfHalfday(), hours())
  }

  def clockhourOfHalfday(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.clockhourOfHalfday(), hours())
  }

  def halfdayOfDay(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.halfdayOfDay(), halfdays())
  }

  def days(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.days())
  }

  def dayOfWeek(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.dayOfWeek(), days())
  }

  def dayOfMonth(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.dayOfMonth(), days())
  }

  def dayOfYear(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.dayOfYear(), days())
  }

  def weeks(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.weeks())
  }

  def weekOfWeekyear(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.weekOfWeekyear(), weeks())
  }

  def weekyears(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.weekyears())
  }

  def weekyear(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.weekyear(), weekyears())
  }

  def weekyearOfCentury(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.weekyearOfCentury(), weekyears())
  }

  def months(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.months())
  }

  def monthOfYear(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.monthOfYear(), months())
  }

  def years(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.years())
  }

  def year(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.year(), years())
  }

  def yearOfEra(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.yearOfEra(), years())
  }

  def yearOfCentury(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.yearOfCentury(), years())
  }

  def centuries(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.centuries())
  }

  def centuryOfEra(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.centuryOfEra(), centuries())
  }

  def eras(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.eras())
  }

  def era(): DateTimeField = {
    UnsupportedDateTimeField.getInstance(DateTimeFieldType.era(), eras())
  }

  override def toString(): String
}
