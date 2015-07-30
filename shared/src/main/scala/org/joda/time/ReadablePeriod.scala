package org.joda.time

trait ReadablePeriod {

  def getPeriodType(): PeriodType

  def size(): Int

  def getFieldType(index: Int): DurationFieldType

  def getValue(index: Int): Int

  def get(field: DurationFieldType): Int

  def isSupported(field: DurationFieldType): Boolean

  def toPeriod(): Period

  def toMutablePeriod(): MutablePeriod

  override def equals(readablePeriod: Any): Boolean

  override def hashCode(): Int

  override def toString(): String
}
