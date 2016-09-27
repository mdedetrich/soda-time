package org.joda.time.chrono

import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadablePartial
import org.joda.time.field.PreciseDurationDateTimeField

@SerialVersionUID(-6821236822336841037L)
class BasicDayOfYearDateTimeField(private val iChronology: BasicChronology,
                                  days: DurationField)
    extends PreciseDurationDateTimeField(DateTimeFieldType.dayOfYear(), days) {

  def get(instant: Long): Int = iChronology.getDayOfYear(instant)

  def getRangeDurationField(): DurationField = iChronology.years()

  override def getMinimumValue(): Int = 1

  def getMaximumValue(): Int = iChronology.getDaysInYearMax

  override def getMaximumValue(instant: Long): Int = {
    val year = iChronology.getYear(instant)
    iChronology.getDaysInYear(year)
  }

  override def getMaximumValue(partial: ReadablePartial): Int = {
    if (partial.isSupported(DateTimeFieldType.year())) {
      val year = partial.get(DateTimeFieldType.year())
      return iChronology.getDaysInYear(year)
    }
    iChronology.getDaysInYearMax
  }

  override def getMaximumValue(partial: ReadablePartial,
                               values: Array[Int]): Int = {
    val size = partial.size
    for (i <- 0 until size
         if partial.getFieldType(i) == DateTimeFieldType.year()) {
      val year = values(i)
      return iChronology.getDaysInYear(year)
    }
    iChronology.getDaysInYearMax
  }

  override protected def getMaximumValueForSet(instant: Long,
                                               value: Int): Int = {
    val maxLessOne = iChronology.getDaysInYearMax - 1
    if ((value > maxLessOne || value < 1)) getMaximumValue(instant)
    else maxLessOne
  }

  override def isLeap(instant: Long): Boolean = iChronology.isLeapDay(instant)

  private def readResolve(): AnyRef = iChronology.dayOfYear()
}
