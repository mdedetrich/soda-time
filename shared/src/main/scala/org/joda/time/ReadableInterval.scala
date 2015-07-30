package org.joda.time

trait ReadableInterval {

  def getChronology(): Chronology

  def getStartMillis(): Long

  def getStart(): DateTime

  def getEndMillis(): Long

  def getEnd(): DateTime

  def contains(instant: ReadableInstant): Boolean

  def contains(interval: ReadableInterval): Boolean

  def overlaps(interval: ReadableInterval): Boolean

  def isAfter(instant: ReadableInstant): Boolean

  def isAfter(interval: ReadableInterval): Boolean

  def isBefore(instant: ReadableInstant): Boolean

  def isBefore(interval: ReadableInterval): Boolean

  def toInterval(): Interval

  def toMutableInterval(): MutableInterval

  def toDuration(): Duration

  def toDurationMillis(): Long

  def toPeriod(): Period

  def toPeriod(`type`: PeriodType): Period

  override def equals(readableInterval: Any): Boolean

  override def hashCode(): Int

  override def toString(): String
}
