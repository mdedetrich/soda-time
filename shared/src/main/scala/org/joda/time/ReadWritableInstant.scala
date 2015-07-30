package org.joda.time

trait ReadWritableInstant extends ReadableInstant {

  def setMillis(instant: Long): Unit

  def setMillis(instant: ReadableInstant): Unit

  def setChronology(chronology: Chronology): Unit

  def setZone(zone: DateTimeZone): Unit

  def setZoneRetainFields(zone: DateTimeZone): Unit

  def add(duration: Long): Unit

  def add(duration: ReadableDuration): Unit

  def add(duration: ReadableDuration, scalar: Int): Unit

  def add(period: ReadablePeriod): Unit

  def add(period: ReadablePeriod, scalar: Int): Unit

  def set(`type`: DateTimeFieldType, value: Int): Unit

  def add(`type`: DurationFieldType, amount: Int): Unit
}
