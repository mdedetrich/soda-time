package org.joda.time

trait ReadableInstant extends Comparable[ReadableInstant] {

  def getMillis(): Long

  def getChronology(): Chronology

  def getZone(): DateTimeZone

  def get(`type`: DateTimeFieldType): Int

  def isSupported(field: DateTimeFieldType): Boolean

  def toInstant(): Instant

  def isEqual(instant: ReadableInstant): Boolean

  def isAfter(instant: ReadableInstant): Boolean

  def isBefore(instant: ReadableInstant): Boolean

  override def equals(readableInstant: Any): Boolean

  override def hashCode(): Int

  override def toString(): String
}
