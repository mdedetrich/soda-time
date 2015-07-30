package org.joda.time

trait ReadWritableInterval extends ReadableInterval {

  def setInterval(startInstant: Long, endInstant: Long): Unit

  def setInterval(interval: ReadableInterval): Unit

  def setInterval(startInstant: ReadableInstant, endInstant: ReadableInstant): Unit

  def setChronology(chrono: Chronology): Unit

  def setStartMillis(millisInstant: Long): Unit

  def setStart(instant: ReadableInstant): Unit

  def setEndMillis(millisInstant: Long): Unit

  def setEnd(instant: ReadableInstant): Unit

  def setDurationAfterStart(duration: ReadableDuration): Unit

  def setDurationBeforeEnd(duration: ReadableDuration): Unit

  def setPeriodAfterStart(period: ReadablePeriod): Unit

  def setPeriodBeforeEnd(period: ReadablePeriod): Unit
}
