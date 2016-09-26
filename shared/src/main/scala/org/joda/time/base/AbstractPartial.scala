package org.joda.time.base

import org.joda.time.Chronology
import org.joda.time.DateTime
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeUtils
import org.joda.time.DurationFieldType
import org.joda.time.ReadableInstant
import org.joda.time.ReadablePartial
import org.joda.time.field.FieldUtils
import org.joda.time.format.DateTimeFormatter

abstract class AbstractPartial protected () extends ReadablePartial() with Comparable[ReadablePartial] {

  protected def getField(index: Int, chrono: Chronology): DateTimeField

  def getFieldType(index: Int): DateTimeFieldType = getField(index, getChronology).getType

  def getFieldTypes(): Array[DateTimeFieldType] = {
    val result = Array.ofDim[DateTimeFieldType](size)
    for (i <- 0 until result.length) {
      result(i) = getFieldType(i)
    }
    result
  }

  def getField(index: Int): DateTimeField = getField(index, getChronology)

  def getFields(): Array[DateTimeField] = {
    val result = Array.ofDim[DateTimeField](size)
    for (i <- 0 until result.length) {
      result(i) = getField(i)
    }
    result
  }

  def getValues(): Array[Int] = {
    val result = Array.ofDim[Int](size)
    for (i <- 0 until result.length) {
      result(i) = getValue(i)
    }
    result
  }

  def get(`type`: DateTimeFieldType): Int = getValue(indexOfSupported(`type`))

  def isSupported(`type`: DateTimeFieldType): Boolean = (indexOf(`type`) != -1)

  def indexOf(`type`: DateTimeFieldType): Int = {
    (0 until this.size()).find(getFieldType(_) == `type`).getOrElse(-1)
  }

  protected def indexOfSupported(`type`: DateTimeFieldType): Int = {
    val index = indexOf(`type`)
    if (index == -1) {
      throw new IllegalArgumentException("Field '" + `type` + "' is not supported")
    }
    index
  }

  protected def indexOf(`type`: DurationFieldType): Int = {
    (0 until this.size()).find(getFieldType(_).getDurationType == `type`)
      .getOrElse(-1)
  }

  protected def indexOfSupported(`type`: DurationFieldType): Int = {
    val index = indexOf(`type`)
    if (index == -1) {
      throw new IllegalArgumentException("Field '" + `type` + "' is not supported")
    }
    index
  }

  def toDateTime(baseInstant: ReadableInstant): DateTime = {
    val chrono = DateTimeUtils.getInstantChronology(baseInstant)
    val instantMillis = DateTimeUtils.getInstantMillis(baseInstant)
    val resolved = chrono.set(this, instantMillis)
    new DateTime(resolved, chrono)
  }

  override def equals(partial: Any): Boolean = {
    if (super.equals(partial)) {
      return true
    }
    if (partial.isInstanceOf[ReadablePartial] == false) {
      return false
    }
    val other = partial.asInstanceOf[ReadablePartial]
    if (size != other.size) {
      return false
    }
    for (i <- 0 until this.size() if getValue(i) != other.getValue(i) || getFieldType(i) != other.getFieldType(i)) {
      return false
    }
    FieldUtils.equals(getChronology, other.getChronology)
  }

  override def hashCode(): Int = {
    var total = 157
    for (i <- 0 until this.size()) {
      total = 23 * total + getValue(i)
      total = 23 * total + getFieldType(i).hashCode
    }
    total += getChronology.hashCode
    total
  }

  def compareTo(other: ReadablePartial): Int = {
    if (this == other) {
      return 0
    }
    if (size != other.size) {
      throw new ClassCastException("ReadablePartial objects must have matching field types")
    }
    for (i <- 0 until this.size() if getFieldType(i) != other.getFieldType(i)) {
      throw new ClassCastException("ReadablePartial objects must have matching field types")
    }
    for (i <- 0 until this.size()) {
      if (getValue(i) > other.getValue(i)) {
        return 1
      }
      if (getValue(i) < other.getValue(i)) {
        return -1
      }
    }
    0
  }

  def isAfter(partial: ReadablePartial): Boolean = {
    if (partial == null) {
      throw new IllegalArgumentException("Partial cannot be null")
    }
    compareTo(partial) > 0
  }

  def isBefore(partial: ReadablePartial): Boolean = {
    if (partial == null) {
      throw new IllegalArgumentException("Partial cannot be null")
    }
    compareTo(partial) < 0
  }

  def isEqual(partial: ReadablePartial): Boolean = {
    if (partial == null) {
      throw new IllegalArgumentException("Partial cannot be null")
    }
    compareTo(partial) == 0
  }

  def toString(formatter: DateTimeFormatter): String = {
    if (formatter == null) {
      return toString
    }
    formatter.print(this)
  }
}
