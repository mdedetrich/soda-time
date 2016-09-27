package org.joda.time.base

import java.util.Date
import org.joda.convert.ToString
import org.joda.time.Chronology
import org.joda.time.DateTime
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeUtils
import org.joda.time.DateTimeZone
import org.joda.time.Instant
import org.joda.time.MutableDateTime
import org.joda.time.ReadableInstant
import org.joda.time.chrono.ISOChronology
import org.joda.time.field.FieldUtils
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat

abstract class AbstractInstant protected () extends ReadableInstant() {

  def getZone(): DateTimeZone = getChronology.getZone

  def get(`type`: DateTimeFieldType): Int = {
    if (`type` == null) {
      throw new IllegalArgumentException(
        "The DateTimeFieldType must not be null")
    }
    `type`.getField(getChronology).get(getMillis)
  }

  def isSupported(`type`: DateTimeFieldType): Boolean = {
    if (`type` == null) {
      return false
    }
    `type`.getField(getChronology).isSupported
  }

  def get(field: DateTimeField): Int = {
    if (field == null) {
      throw new IllegalArgumentException("The DateTimeField must not be null")
    }
    field.get(getMillis)
  }

  def toInstant(): Instant = new Instant(getMillis)

  def toDateTime(): DateTime = new DateTime(getMillis, getZone)

  def toDateTimeISO(): DateTime = {
    new DateTime(getMillis, ISOChronology.getInstance(getZone))
  }

  def toDateTime(zone: DateTimeZone): DateTime = {
    var chrono = DateTimeUtils.getChronology(getChronology)
    chrono = chrono.withZone(zone)
    new DateTime(getMillis, chrono)
  }

  def toDateTime(chronology: Chronology): DateTime =
    new DateTime(getMillis, chronology)

  def toMutableDateTime(): MutableDateTime =
    new MutableDateTime(getMillis, getZone)

  def toMutableDateTimeISO(): MutableDateTime = {
    new MutableDateTime(getMillis, ISOChronology.getInstance(getZone))
  }

  def toMutableDateTime(zone: DateTimeZone): MutableDateTime = {
    var chrono = DateTimeUtils.getChronology(getChronology)
    chrono = chrono.withZone(zone)
    new MutableDateTime(getMillis, chrono)
  }

  def toMutableDateTime(chronology: Chronology): MutableDateTime = {
    new MutableDateTime(getMillis, chronology)
  }

  def toDate(): Date = new Date(getMillis)

  override def equals(readableInstant: Any): Boolean = {
    if (this == readableInstant) {
      return true
    }
    if (readableInstant.isInstanceOf[ReadableInstant] == false) {
      return false
    }
    val otherInstant = readableInstant.asInstanceOf[ReadableInstant]
    getMillis == otherInstant.getMillis &&
    FieldUtils.==(getChronology, otherInstant.getChronology)
  }

  override def hashCode(): Int = {
    (getMillis ^ (getMillis >>> 32)).toInt + (getChronology.hashCode)
  }

  def compareTo(other: ReadableInstant): Int = {
    if (this == other) {
      return 0
    }
    val otherMillis = other.getMillis
    val thisMillis = getMillis
    if (thisMillis == otherMillis) {
      return 0
    }
    if (thisMillis < otherMillis) {
      -1
    } else {
      1
    }
  }

  def isAfter(instant: Long): Boolean = (getMillis > instant)

  def isAfterNow(): Boolean = {
    isAfter(DateTimeUtils.currentTimeMillis())
  }

  def isAfter(instant: ReadableInstant): Boolean = {
    val instantMillis = DateTimeUtils.getInstantMillis(instant)
    isAfter(instantMillis)
  }

  def isBefore(instant: Long): Boolean = (getMillis < instant)

  def isBeforeNow(): Boolean = {
    isBefore(DateTimeUtils.currentTimeMillis())
  }

  def isBefore(instant: ReadableInstant): Boolean = {
    val instantMillis = DateTimeUtils.getInstantMillis(instant)
    isBefore(instantMillis)
  }

  def isEqual(instant: Long): Boolean = (getMillis == instant)

  def isEqualNow(): Boolean = {
    isEqual(DateTimeUtils.currentTimeMillis())
  }

  def isEqual(instant: ReadableInstant): Boolean = {
    val instantMillis = DateTimeUtils.getInstantMillis(instant)
    isEqual(instantMillis)
  }

  @ToString
  override def toString(): String = {
    ISODateTimeFormat.dateTime().print(this)
  }

  def toString(formatter: DateTimeFormatter): String = {
    if (formatter == null) {
      return toString
    }
    formatter.print(this)
  }
}
