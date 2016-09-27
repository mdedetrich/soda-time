package org.joda.time

trait ReadableDuration extends Comparable[ReadableDuration] {

  def getMillis(): Long

  def toDuration(): Duration

  def toPeriod(): Period

  def isEqual(duration: ReadableDuration): Boolean

  def isLongerThan(duration: ReadableDuration): Boolean

  def isShorterThan(duration: ReadableDuration): Boolean

  override def equals(readableDuration: Any): Boolean

  override def hashCode(): Int

  override def toString(): String
}
