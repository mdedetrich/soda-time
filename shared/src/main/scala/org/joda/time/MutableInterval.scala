package org.joda.time

import java.io.Serializable
import org.joda.time.base.BaseInterval
import org.joda.time.field.FieldUtils

object MutableInterval {

  def parse(str: String): MutableInterval = new MutableInterval(str)
}

@SerialVersionUID(-5982824024992428470L)
class MutableInterval
    extends BaseInterval
    with ReadWritableInterval
    with Cloneable
    with Serializable {

  def this(startInstant: Long, endInstant: Long) {
    this()
    super.auxConstructor(startInstant, endInstant, null)
  }

  def this(startInstant: Long, endInstant: Long, chronology: Chronology) {
    this()
    super.auxConstructor(startInstant, endInstant, chronology)
  }

  def this(start: ReadableInstant, end: ReadableInstant) {
    this()
    super.auxConstructor(start, end)
  }

  def this(start: ReadableInstant, duration: ReadableDuration) {
    this()
    super.auxConstructor(start, duration)
  }

  def this(duration: ReadableDuration, end: ReadableInstant) {
    this()
    super.auxConstructor(duration, end)
  }

  def this(start: ReadableInstant, period: ReadablePeriod) {
    this()
    super.auxConstructor(start, period)
  }

  def this(period: ReadablePeriod, end: ReadableInstant) {
    this()
    super.auxConstructor(period, end)
  }

  def this(interval: AnyRef) {
    this()
    super.auxConstructor(interval, null)
  }

  def this(interval: AnyRef, chronology: Chronology) {
    this()
    super.auxConstructor(interval, chronology)
  }

  def setInterval(startInstant: Long, endInstant: Long) {
    super.setInterval(startInstant, endInstant, getChronology)
  }

  def setInterval(interval: ReadableInterval) {
    if (interval == null) {
      throw new IllegalArgumentException("Interval must not be null")
    }
    val startMillis = interval.getStartMillis
    val endMillis = interval.getEndMillis
    val chrono = interval.getChronology
    super.setInterval(startMillis, endMillis, chrono)
  }

  def setInterval(start: ReadableInstant, end: ReadableInstant) {
    if (start == null && end == null) {
      val now = DateTimeUtils.currentTimeMillis()
      setInterval(now, now)
    } else {
      val startMillis = DateTimeUtils.getInstantMillis(start)
      val endMillis = DateTimeUtils.getInstantMillis(end)
      val chrono = DateTimeUtils.getInstantChronology(start)
      super.setInterval(startMillis, endMillis, chrono)
    }
  }

  def setChronology(chrono: Chronology) {
    super.setInterval(getStartMillis, getEndMillis, chrono)
  }

  def setStartMillis(startInstant: Long) {
    super.setInterval(startInstant, getEndMillis, getChronology)
  }

  def setStart(start: ReadableInstant) {
    val startMillis = DateTimeUtils.getInstantMillis(start)
    super.setInterval(startMillis, getEndMillis, getChronology)
  }

  def setEndMillis(endInstant: Long) {
    super.setInterval(getStartMillis, endInstant, getChronology)
  }

  def setEnd(end: ReadableInstant) {
    val endMillis = DateTimeUtils.getInstantMillis(end)
    super.setInterval(getStartMillis, endMillis, getChronology)
  }

  def setDurationAfterStart(duration: Long) {
    setEndMillis(FieldUtils.safeAdd(getStartMillis, duration))
  }

  def setDurationBeforeEnd(duration: Long) {
    setStartMillis(FieldUtils.safeAdd(getEndMillis, -duration))
  }

  def setDurationAfterStart(duration: ReadableDuration) {
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    setEndMillis(FieldUtils.safeAdd(getStartMillis, durationMillis))
  }

  def setDurationBeforeEnd(duration: ReadableDuration) {
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    setStartMillis(FieldUtils.safeAdd(getEndMillis, -durationMillis))
  }

  def setPeriodAfterStart(period: ReadablePeriod) {
    if (period == null) {
      setEndMillis(getStartMillis)
    } else {
      setEndMillis(getChronology.add(period, getStartMillis, 1))
    }
  }

  def setPeriodBeforeEnd(period: ReadablePeriod) {
    if (period == null) {
      setStartMillis(getEndMillis)
    } else {
      setStartMillis(getChronology.add(period, getEndMillis, -1))
    }
  }

  def copy(): MutableInterval = clone().asInstanceOf[MutableInterval]

  override def clone(): AnyRef = super.clone()
}
