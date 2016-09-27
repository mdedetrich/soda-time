package org.joda.time.chrono

import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadablePartial
import org.joda.time.field.PreciseDurationDateTimeField

@SerialVersionUID(-4677223814028011723L)
class BasicDayOfMonthDateTimeField(private val iChronology: BasicChronology,
                                   days: DurationField)
    extends PreciseDurationDateTimeField(DateTimeFieldType.dayOfMonth(), days) {

  def get(instant: Long): Int = iChronology.getDayOfMonth(instant)

  def getRangeDurationField(): DurationField = iChronology.months()

  override def getMinimumValue(): Int = 1

  def getMaximumValue(): Int = iChronology.getDaysInMonthMax

  override def getMaximumValue(instant: Long): Int =
    iChronology.getDaysInMonthMax(instant)

  override def getMaximumValue(partial: ReadablePartial): Int = {
    if (partial.isSupported(DateTimeFieldType.monthOfYear())) {
      val month = partial.get(DateTimeFieldType.monthOfYear())
      if (partial.isSupported(DateTimeFieldType.year())) {
        val year = partial.get(DateTimeFieldType.year())
        return iChronology.getDaysInYearMonth(year, month)
      }
      return iChronology.getDaysInMonthMax(month)
    }
    getMaximumValue
  }

  override def getMaximumValue(partial: ReadablePartial,
                               values: Array[Int]): Int = {
    val size = partial.size
    for (i <- 0 until size
         if partial.getFieldType(i) == DateTimeFieldType.monthOfYear()) {
      val month = values(i)
      for (j <- 0 until size
           if partial.getFieldType(j) == DateTimeFieldType.year()) {
        val year = values(j)
        return iChronology.getDaysInYearMonth(year, month)
      }
      return iChronology.getDaysInMonthMax(month)
    }
    getMaximumValue
  }

  override protected def getMaximumValueForSet(instant: Long,
                                               value: Int): Int = {
    iChronology.getDaysInMonthMaxForSet(instant, value)
  }

  override def isLeap(instant: Long): Boolean = iChronology.isLeapDay(instant)

  private def readResolve(): AnyRef = iChronology.dayOfMonth()
}
