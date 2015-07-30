package org.joda.time.base

import org.joda.time.DateTime
import org.joda.time.DateTimeUtils
import org.joda.time.Duration
import org.joda.time.Interval
import org.joda.time.MutableInterval
import org.joda.time.Period
import org.joda.time.PeriodType
import org.joda.time.ReadableInstant
import org.joda.time.ReadableInterval
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISODateTimeFormat

abstract class AbstractInterval extends ReadableInterval() {

  protected def checkInterval(start: Long, end: Long) {
    if (end < start) {
      throw new IllegalArgumentException("The end instant must be greater or equal to the start")
    }
  }

  def getStart(): DateTime = {
    new DateTime(getStartMillis, getChronology)
  }

  def getEnd(): DateTime = {
    new DateTime(getEndMillis, getChronology)
  }

  def contains(millisInstant: Long): Boolean = {
    val thisStart = getStartMillis
    val thisEnd = getEndMillis
    millisInstant >= thisStart && millisInstant < thisEnd
  }

  def containsNow(): Boolean = {
    contains(DateTimeUtils.currentTimeMillis())
  }

  def contains(instant: ReadableInstant): Boolean = {
    if (instant == null) {
      return containsNow()
    }
    contains(instant.getMillis)
  }

  def contains(interval: ReadableInterval): Boolean = {
    if (interval == null) {
      return containsNow()
    }
    val otherStart = interval.getStartMillis
    val otherEnd = interval.getEndMillis
    val thisStart = getStartMillis
    val thisEnd = getEndMillis
    thisStart <= otherStart && otherStart < thisEnd && otherEnd <= thisEnd
  }

  def overlaps(interval: ReadableInterval): Boolean = {
    val thisStart = getStartMillis
    val thisEnd = getEndMillis
    if (interval == null) {
      val now = DateTimeUtils.currentTimeMillis()
      thisStart < now && now < thisEnd
    } else {
      val otherStart = interval.getStartMillis
      val otherEnd = interval.getEndMillis
      thisStart < otherEnd && otherStart < thisEnd
    }
  }

  def isEqual(other: ReadableInterval): Boolean = {
    getStartMillis == other.getStartMillis && getEndMillis == other.getEndMillis
  }

  def isBefore(millisInstant: Long): Boolean = (getEndMillis <= millisInstant)

  def isBeforeNow(): Boolean = {
    isBefore(DateTimeUtils.currentTimeMillis())
  }

  def isBefore(instant: ReadableInstant): Boolean = {
    if (instant == null) {
      return isBeforeNow
    }
    isBefore(instant.getMillis)
  }

  def isBefore(interval: ReadableInterval): Boolean = {
    if (interval == null) {
      return isBeforeNow
    }
    isBefore(interval.getStartMillis)
  }

  def isAfter(millisInstant: Long): Boolean = (getStartMillis > millisInstant)

  def isAfterNow(): Boolean = {
    isAfter(DateTimeUtils.currentTimeMillis())
  }

  def isAfter(instant: ReadableInstant): Boolean = {
    if (instant == null) {
      return isAfterNow
    }
    isAfter(instant.getMillis)
  }

  def isAfter(interval: ReadableInterval): Boolean = {
    var endMillis: Long = 0l
    endMillis = if (interval == null) DateTimeUtils.currentTimeMillis() else interval.getEndMillis
    getStartMillis >= endMillis
  }

  def toInterval(): Interval = {
    new Interval(getStartMillis, getEndMillis, getChronology)
  }

  def toMutableInterval(): MutableInterval = {
    new MutableInterval(getStartMillis, getEndMillis, getChronology)
  }

  def toDurationMillis(): Long = {
    FieldUtils.safeAdd(getEndMillis, -getStartMillis)
  }

  def toDuration(): Duration = {
    val durMillis = toDurationMillis()
    if (durMillis == 0) {
      Duration.ZERO
    } else {
      new Duration(durMillis)
    }
  }

  def toPeriod(): Period = {
    new Period(getStartMillis, getEndMillis, getChronology)
  }

  def toPeriod(`type`: PeriodType): Period = {
    new Period(getStartMillis, getEndMillis, `type`, getChronology)
  }

  override def equals(readableInterval: Any): Boolean = {
    if (this == readableInterval) {
      return true
    }
    if (readableInterval.isInstanceOf[ReadableInterval] == false) {
      return false
    }
    val other = readableInterval.asInstanceOf[ReadableInterval]
    getStartMillis == other.getStartMillis && getEndMillis == other.getEndMillis &&
      FieldUtils.==(getChronology, other.getChronology)
  }

  override def hashCode(): Int = {
    val start = getStartMillis
    val end = getEndMillis
    var result = 97
    result = 31 * result + (start ^ (start >>> 32)).toInt
    result = 31 * result + (end ^ (end >>> 32)).toInt
    result = 31 * result + getChronology.hashCode
    result
  }

  override def toString(): String = {
    var printer = ISODateTimeFormat.dateTime()
    printer = printer.withChronology(getChronology)
    val buf = new StringBuffer(48)
    printer.printTo(buf, getStartMillis)
    buf.append('/')
    printer.printTo(buf, getEndMillis)
    buf.toString
  }
}
