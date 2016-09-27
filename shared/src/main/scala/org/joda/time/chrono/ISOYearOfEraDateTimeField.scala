package org.joda.time.chrono

import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadablePartial
import org.joda.time.field.DecoratedDateTimeField
import org.joda.time.field.FieldUtils
import ISOYearOfEraDateTimeField._

object ISOYearOfEraDateTimeField {

  val INSTANCE = new ISOYearOfEraDateTimeField()
}

@SerialVersionUID(7037524068969447317L)
class ISOYearOfEraDateTimeField private ()
    extends DecoratedDateTimeField(GregorianChronology.getInstanceUTC.year(),
                                   DateTimeFieldType.yearOfEra()) {

  override def getRangeDurationField(): DurationField = {
    GregorianChronology.getInstanceUTC.eras()
  }

  override def get(instant: Long): Int = {
    val year = getWrappedField.get(instant)
    if (year < 0) -year else year
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
    var _year = year
    FieldUtils.verifyValueBounds(this, _year, 0, getMaximumValue)
    if (getWrappedField.get(instant) < 0) {
      _year = -_year
    }
    super.set(instant, _year)
  }

  override def getMinimumValue(): Int = 0

  override def getMaximumValue(): Int = getWrappedField.getMaximumValue

  override def roundFloor(instant: Long): Long =
    getWrappedField.roundFloor(instant)

  override def roundCeiling(instant: Long): Long =
    getWrappedField.roundCeiling(instant)

  override def remainder(instant: Long): Long =
    getWrappedField.remainder(instant)

  private def readResolve(): AnyRef = INSTANCE
}
