package org.joda.time

trait ReadablePartial extends Comparable[ReadablePartial] {

  def size(): Int

  def getFieldType(index: Int): DateTimeFieldType

  def getField(index: Int): DateTimeField

  def getValue(index: Int): Int

  def getChronology(): Chronology

  def get(field: DateTimeFieldType): Int

  def isSupported(field: DateTimeFieldType): Boolean

  def toDateTime(baseInstant: ReadableInstant): DateTime

  override def equals(partial: Any): Boolean

  override def hashCode(): Int

  override def toString(): String
}
