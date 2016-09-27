package org.joda.time.chrono

import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadablePartial
import org.joda.time.field.DecoratedDateTimeField
import org.joda.time.field.FieldUtils

@SerialVersionUID(-5961050944769862059L)
class GJYearOfEraDateTimeField(yearField: DateTimeField,
                               private val chronology: BasicChronology)
    extends DecoratedDateTimeField(yearField, DateTimeFieldType.yearOfEra()) {

  private var iChronology: BasicChronology = null

  iChronology = chronology

  override def getRangeDurationField(): DurationField = iChronology.eras()

  override def get(instant: Long): Int = {
    var year = getWrappedField.get(instant)
    if (year <= 0) {
      year = 1 - year
    }
    year
  }

  override def add(instant: Long, years: Int): Long =
    getWrappedField.add(instant, years)

  override def add(instant: Long, years: Long): Long =
    getWrappedField.add(instant, years)

  override def addWrapField(instant: Long, years: Int): Long = {
    getWrappedField.addWrapField(instant, years)
  }

  override def addWrapField(instant: ReadablePartial,
                            fieldIndex: Int,
                            values: Array[Int],
                            years: Int): Array[Int] = {
    getWrappedField.addWrapField(instant, fieldIndex, values, years)
  }

  override def getDifference(minuendInstant: Long,
                             subtrahendInstant: Long): Int = {
    getWrappedField.getDifference(minuendInstant, subtrahendInstant)
  }

  override def getDifferenceAsLong(minuendInstant: Long,
                                   subtrahendInstant: Long): Long = {
    getWrappedField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
  }

  override def set(instant: Long, year: Int): Long = {
    var _year: Int = year
    FieldUtils.verifyValueBounds(this, _year, 1, getMaximumValue)
    if (iChronology.getYear(instant) <= 0) {
      _year = 1 - _year
    }
    super.set(instant, _year)
  }

  override def getMinimumValue(): Int = 1

  override def getMaximumValue(): Int = getWrappedField.getMaximumValue

  override def roundFloor(instant: Long): Long =
    getWrappedField.roundFloor(instant)

  override def roundCeiling(instant: Long): Long =
    getWrappedField.roundCeiling(instant)

  override def remainder(instant: Long): Long =
    getWrappedField.remainder(instant)

  private def readResolve(): AnyRef = iChronology.yearOfEra()
}
