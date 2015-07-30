package org.joda.time.field

import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.DurationFieldType

@SerialVersionUID(7190739608550251860L)
abstract class ImpreciseDateTimeField(`type`: DateTimeFieldType, val unitMillis: Long)
  extends BaseDateTimeField(`type`) {

  private val iDurationField = new LinkedDurationField(`type`.getDurationType)
  private var iUnitMillis: Long = _
  
  iUnitMillis = unitMillis
  

  def get(instant: Long): Int

  def set(instant: Long, value: Int): Long

  def add(instant: Long, value: Int): Long

  def add(instant: Long, value: Long): Long

  override def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
    FieldUtils.safeToInt(getDifferenceAsLong(minuendInstant, subtrahendInstant))
  }

  override def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
    if (minuendInstant < subtrahendInstant) {
      return -getDifferenceAsLong(subtrahendInstant, minuendInstant)
    }
    var difference = (minuendInstant - subtrahendInstant) / iUnitMillis
    if (add(subtrahendInstant, difference) < minuendInstant) {
      do {
        difference += 1
      } while (add(subtrahendInstant, difference) <= minuendInstant);
      difference -= 1
    } else if (add(subtrahendInstant, difference) > minuendInstant) {
      do {
        difference -= 1
      } while (add(subtrahendInstant, difference) > minuendInstant);
    }
    difference
  }

  def getDurationField(): DurationField = iDurationField

  def getRangeDurationField(): DurationField

  def roundFloor(instant: Long): Long

  protected def getDurationUnitMillis(): Long = iUnitMillis

  @SerialVersionUID(-203813474600094134L)
  private class LinkedDurationField(`type`: DurationFieldType) extends BaseDurationField(`type`) {

    def isPrecise(): Boolean = false

    def getUnitMillis(): Long = iUnitMillis

    override def getValue(duration: Long, instant: Long): Int = {
      ImpreciseDateTimeField.this.getDifference(instant + duration, instant)
    }

    def getValueAsLong(duration: Long, instant: Long): Long = {
      ImpreciseDateTimeField.this.getDifferenceAsLong(instant + duration, instant)
    }

    def getMillis(value: Int, instant: Long): Long = {
      ImpreciseDateTimeField.this.add(instant, value) - instant
    }

    def getMillis(value: Long, instant: Long): Long = {
      ImpreciseDateTimeField.this.add(instant, value) - instant
    }

    def add(instant: Long, value: Int): Long = {
      ImpreciseDateTimeField.this.add(instant, value)
    }

    def add(instant: Long, value: Long): Long = {
      ImpreciseDateTimeField.this.add(instant, value)
    }

    override def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
      ImpreciseDateTimeField.this.getDifference(minuendInstant, subtrahendInstant)
    }

    def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
      ImpreciseDateTimeField.this.getDifferenceAsLong(minuendInstant, subtrahendInstant)
    }
  }
}
