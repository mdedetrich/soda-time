package org.joda.time.base

import org.joda.convert.ToString
import org.joda.time.DurationFieldType
import org.joda.time.MutablePeriod
import org.joda.time.Period
import org.joda.time.ReadablePeriod
import org.joda.time.format.ISOPeriodFormat
import org.joda.time.format.PeriodFormatter

abstract class AbstractPeriod protected () extends ReadablePeriod() {

  def size(): Int = getPeriodType.size

  def getFieldType(index: Int): DurationFieldType =
    getPeriodType.getFieldType(index)

  def getFieldTypes(): Array[DurationFieldType] = {
    val result = Array.ofDim[DurationFieldType](size)
    for (i <- 0 until result.length) {
      result(i) = getFieldType(i)
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

  def get(`type`: DurationFieldType): Int = {
    val index = indexOf(`type`)
    if (index == -1) {
      return 0
    }
    getValue(index)
  }

  def isSupported(`type`: DurationFieldType): Boolean =
    getPeriodType.isSupported(`type`)

  def indexOf(`type`: DurationFieldType): Int = getPeriodType.indexOf(`type`)

  def toPeriod(): Period = new Period(this)

  def toMutablePeriod(): MutablePeriod = new MutablePeriod(this)

  override def equals(period: Any): Boolean = {
    if (this == period) {
      return true
    }
    if (period.isInstanceOf[ReadablePeriod] == false) {
      return false
    }
    val other = period.asInstanceOf[ReadablePeriod]
    if (size != other.size) {
      return false
    }
    for (i <- 0 until size()
         if getValue(i) != other.getValue(i) || getFieldType(i) != other
           .getFieldType(i)) {
      return false
    }
    true
  }

  override def hashCode(): Int = {
    var total = 17
    for (i <- 0 until size()) {
      total = 27 * total + getValue(i)
      total = 27 * total + getFieldType(i).hashCode
    }
    total
  }

  @ToString
  override def toString(): String = ISOPeriodFormat.standard().print(this)

  def toString(formatter: PeriodFormatter): String = {
    if (formatter == null) {
      return toString
    }
    formatter.print(this)
  }
}
