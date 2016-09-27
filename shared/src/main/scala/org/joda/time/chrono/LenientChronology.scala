package org.joda.time.chrono

import org.joda.time.Chronology
import org.joda.time.DateTimeField
import org.joda.time.DateTimeZone
import org.joda.time.chrono.AssembledChronology.Fields
import org.joda.time.field.LenientDateTimeField

object LenientChronology {

  def getInstance(base: Chronology): LenientChronology = {
    if (base == null) {
      throw new IllegalArgumentException("Must supply a chronology")
    }
    new LenientChronology(base)
  }
}

@SerialVersionUID(-3148237568046877177L)
class LenientChronology private (base: Chronology)
    extends AssembledChronology(base, null) {

  @transient private var iWithUTC: Chronology = null

  def withUTC(): Chronology = {
    if (iWithUTC == null) {
      iWithUTC =
        if (getZone == DateTimeZone.UTC) this
        else LenientChronology.getInstance(getBase.withUTC())
    }
    iWithUTC
  }

  def withZone(zone: DateTimeZone): Chronology = {
    var _zone: DateTimeZone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    if (_zone == DateTimeZone.UTC) {
      return withUTC()
    }
    if (_zone == getZone) {
      return this
    }
    LenientChronology.getInstance(getBase.withZone(_zone))
  }

  protected def assemble(fields: Fields) {
    fields.year = convertField(fields.year)
    fields.yearOfEra = convertField(fields.yearOfEra)
    fields.yearOfCentury = convertField(fields.yearOfCentury)
    fields.centuryOfEra = convertField(fields.centuryOfEra)
    fields.era = convertField(fields.era)
    fields.dayOfWeek = convertField(fields.dayOfWeek)
    fields.dayOfMonth = convertField(fields.dayOfMonth)
    fields.dayOfYear = convertField(fields.dayOfYear)
    fields.monthOfYear = convertField(fields.monthOfYear)
    fields.weekOfWeekyear = convertField(fields.weekOfWeekyear)
    fields.weekyear = convertField(fields.weekyear)
    fields.weekyearOfCentury = convertField(fields.weekyearOfCentury)
    fields.millisOfSecond = convertField(fields.millisOfSecond)
    fields.millisOfDay = convertField(fields.millisOfDay)
    fields.secondOfMinute = convertField(fields.secondOfMinute)
    fields.secondOfDay = convertField(fields.secondOfDay)
    fields.minuteOfHour = convertField(fields.minuteOfHour)
    fields.minuteOfDay = convertField(fields.minuteOfDay)
    fields.hourOfDay = convertField(fields.hourOfDay)
    fields.hourOfHalfday = convertField(fields.hourOfHalfday)
    fields.clockhourOfDay = convertField(fields.clockhourOfDay)
    fields.clockhourOfHalfday = convertField(fields.clockhourOfHalfday)
    fields.halfdayOfDay = convertField(fields.halfdayOfDay)
  }

  private def convertField(field: DateTimeField): DateTimeField = {
    LenientDateTimeField.getInstance(field, getBase)
  }

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    }
    if (obj.isInstanceOf[LenientChronology] == false) {
      return false
    }
    val chrono = obj.asInstanceOf[LenientChronology]
    getBase == chrono.getBase
  }

  override def hashCode(): Int = 236548278 + getBase.hashCode * 7

  override def toString(): String = {
    "LenientChronology[" + getBase.toString + ']'
  }
}
