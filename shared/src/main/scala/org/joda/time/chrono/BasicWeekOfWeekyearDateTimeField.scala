package org.joda.time.chrono

import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadablePartial
import org.joda.time.field.PreciseDurationDateTimeField

@SerialVersionUID(-1587436826395135328L)
class BasicWeekOfWeekyearDateTimeField(private val chronology: BasicChronology,
                                       weeks: DurationField)
    extends PreciseDurationDateTimeField(DateTimeFieldType.weekOfWeekyear(),
                                         weeks) {

  private var iChronology: BasicChronology = null

  iChronology = chronology

  def get(instant: Long): Int = iChronology.getWeekOfWeekyear(instant)

  def getRangeDurationField(): DurationField = iChronology.weekyears()

  override def roundFloor(instant: Long): Long = {
    super.roundFloor(instant + 3 * DateTimeConstants.MILLIS_PER_DAY) -
      3 * DateTimeConstants.MILLIS_PER_DAY
  }

  override def roundCeiling(instant: Long): Long = {
    super.roundCeiling(instant + 3 * DateTimeConstants.MILLIS_PER_DAY) -
      3 * DateTimeConstants.MILLIS_PER_DAY
  }

  override def remainder(instant: Long): Long = {
    super.remainder(instant + 3 * DateTimeConstants.MILLIS_PER_DAY)
  }

  override def getMinimumValue(): Int = 1

  def getMaximumValue(): Int = 53

  override def getMaximumValue(instant: Long): Int = {
    val weekyear = iChronology.getWeekyear(instant)
    iChronology.getWeeksInYear(weekyear)
  }

  override def getMaximumValue(partial: ReadablePartial): Int = {
    if (partial.isSupported(DateTimeFieldType.weekyear())) {
      val weekyear = partial.get(DateTimeFieldType.weekyear())
      return iChronology.getWeeksInYear(weekyear)
    }
    53
  }

  override def getMaximumValue(partial: ReadablePartial,
                               values: Array[Int]): Int = {
    val size = partial.size
    for (i <- 0 until size
         if partial.getFieldType(i) == DateTimeFieldType.weekyear()) {
      val weekyear = values(i)
      return iChronology.getWeeksInYear(weekyear)
    }
    53
  }

  override protected def getMaximumValueForSet(instant: Long,
                                               value: Int): Int = {
    if (value > 52) getMaximumValue(instant) else 52
  }

  private def readResolve(): AnyRef = iChronology.weekOfWeekyear()
}
