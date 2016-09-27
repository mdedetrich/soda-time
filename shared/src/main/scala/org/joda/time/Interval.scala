package org.joda.time

import java.io.Serializable
import org.joda.time.base.BaseInterval
import org.joda.time.chrono.ISOChronology

object Interval {

  def parse(str: String): Interval = new Interval(str)
}

@SerialVersionUID(4922451897541386752L)
class Interval extends BaseInterval with ReadableInterval with Serializable {

  def this(startInstant: Long, endInstant: Long) {
    this()
    super.auxConstructor(startInstant, endInstant, null)
  }

  def this(startInstant: Long, endInstant: Long, zone: DateTimeZone) {
    this()
    super.auxConstructor(startInstant,
                         endInstant,
                         ISOChronology.getInstance(zone))
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

  override def toInterval(): Interval = this

  def overlap(interval: ReadableInterval): Interval = {
    var _interval = interval
    _interval = DateTimeUtils.getReadableInterval(_interval)
    if (overlaps(_interval) == false) {
      return null
    }
    val start = Math.max(getStartMillis, _interval.getStartMillis)
    val end = Math.min(getEndMillis, _interval.getEndMillis)
    new Interval(start, end, getChronology)
  }

  def gap(interval: ReadableInterval): Interval = {
    var _interval = interval
    _interval = DateTimeUtils.getReadableInterval(_interval)
    val otherStart = _interval.getStartMillis
    val otherEnd = _interval.getEndMillis
    val thisStart = getStartMillis
    val thisEnd = getEndMillis
    if (thisStart > otherEnd) {
      new Interval(otherEnd, thisStart, getChronology)
    } else if (otherStart > thisEnd) {
      new Interval(thisEnd, otherStart, getChronology)
    } else {
      null
    }
  }

  def abuts(interval: ReadableInterval): Boolean = {
    if (interval == null) {
      val now = DateTimeUtils.currentTimeMillis()
      (getStartMillis == now || getEndMillis == now)
    } else {
      (interval.getEndMillis == getStartMillis || getEndMillis == interval.getStartMillis)
    }
  }

  def withChronology(chronology: Chronology): Interval = {
    if (getChronology == chronology) {
      return this
    }
    new Interval(getStartMillis, getEndMillis, chronology)
  }

  def withStartMillis(startInstant: Long): Interval = {
    if (startInstant == getStartMillis) {
      return this
    }
    new Interval(startInstant, getEndMillis, getChronology)
  }

  def withStart(start: ReadableInstant): Interval = {
    val startMillis = DateTimeUtils.getInstantMillis(start)
    withStartMillis(startMillis)
  }

  def withEndMillis(endInstant: Long): Interval = {
    if (endInstant == getEndMillis) {
      return this
    }
    new Interval(getStartMillis, endInstant, getChronology)
  }

  def withEnd(end: ReadableInstant): Interval = {
    val endMillis = DateTimeUtils.getInstantMillis(end)
    withEndMillis(endMillis)
  }

  def withDurationAfterStart(duration: ReadableDuration): Interval = {
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    if (durationMillis == toDurationMillis()) {
      return this
    }
    val chrono = getChronology
    val startMillis = getStartMillis
    val endMillis = chrono.add(startMillis, durationMillis, 1)
    new Interval(startMillis, endMillis, chrono)
  }

  def withDurationBeforeEnd(duration: ReadableDuration): Interval = {
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    if (durationMillis == toDurationMillis()) {
      return this
    }
    val chrono = getChronology
    val endMillis = getEndMillis
    val startMillis = chrono.add(endMillis, durationMillis, -1)
    new Interval(startMillis, endMillis, chrono)
  }

  def withPeriodAfterStart(period: ReadablePeriod): Interval = {
    if (period == null) {
      return withDurationAfterStart(null)
    }
    val chrono = getChronology
    val startMillis = getStartMillis
    val endMillis = chrono.add(period, startMillis, 1)
    new Interval(startMillis, endMillis, chrono)
  }

  def withPeriodBeforeEnd(period: ReadablePeriod): Interval = {
    if (period == null) {
      return withDurationBeforeEnd(null)
    }
    val chrono = getChronology
    val endMillis = getEndMillis
    val startMillis = chrono.add(period, endMillis, -1)
    new Interval(startMillis, endMillis, chrono)
  }
}
