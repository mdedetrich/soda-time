package org.joda.time.chrono

import java.util.HashMap
import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeField
import org.joda.time.DateTimeZone
import org.joda.time.DurationField
import org.joda.time.IllegalFieldValueException
import org.joda.time.IllegalInstantException
import org.joda.time.ReadablePartial
import org.joda.time.chrono.AssembledChronology.Fields
import org.joda.time.field.BaseDateTimeField
import org.joda.time.field.BaseDurationField
import ZonedChronology._

object ZonedChronology {

  def getInstance(base: Chronology, zone: DateTimeZone): ZonedChronology = {
    var _base: Chronology = base
    if (_base == null) {
      throw new IllegalArgumentException("Must supply a chronology")
    }
    _base = _base.withUTC()
    if (_base == null) {
      throw new IllegalArgumentException("UTC chronology must not be null")
    }
    if (zone == null) {
      throw new IllegalArgumentException("DateTimeZone must not be null")
    }
    new ZonedChronology(_base, zone)
  }

  def useTimeArithmetic(field: DurationField): Boolean = {
    field != null &&
      field.getUnitMillis < DateTimeConstants.MILLIS_PER_HOUR * 12
  }

  @SerialVersionUID(-485345310999208286L)
  class ZonedDurationField(val field: DurationField, val zone: DateTimeZone)
    extends BaseDurationField(field.getType) {

    var iField: DurationField = null
    var iZone: DateTimeZone = null

    val iTimeField = useTimeArithmetic(field)

    if (!field.isSupported) {
      throw new IllegalArgumentException()
    }

    iField = field
    iZone = zone

    def isPrecise(): Boolean = {
      if (iTimeField) iField.isPrecise else iField.isPrecise && this.iZone.isFixed
    }

    def getUnitMillis(): Long = iField.getUnitMillis

    override def getValue(duration: Long, instant: Long): Int = {
      iField.getValue(duration, addOffset(instant))
    }

    def getValueAsLong(duration: Long, instant: Long): Long = {
      iField.getValueAsLong(duration, addOffset(instant))
    }

    def getMillis(value: Int, instant: Long): Long = {
      iField.getMillis(value, addOffset(instant))
    }

    def getMillis(value: Long, instant: Long): Long = {
      iField.getMillis(value, addOffset(instant))
    }

    def add(instant: Long, value: Int): Long = {
      var _instant:Long = instant
      val offset = getOffsetToAdd(_instant)
      _instant = iField.add(_instant + offset, value)
      _instant -
        (if (iTimeField) offset else getOffsetFromLocalToSubtract(_instant))
    }

    def add(instant: Long, value: Long): Long = {
      var _instant:Long = instant
      val offset = getOffsetToAdd(_instant)
      _instant = iField.add(_instant + offset, value)
      _instant -
        (if (iTimeField) offset else getOffsetFromLocalToSubtract(_instant))
    }

    override def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
      val offset = getOffsetToAdd(subtrahendInstant)
      iField.getDifference(minuendInstant +
        (if (iTimeField) offset else getOffsetToAdd(minuendInstant)), subtrahendInstant + offset)
    }

    def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
      val offset = getOffsetToAdd(subtrahendInstant)
      iField.getDifferenceAsLong(minuendInstant +
        (if (iTimeField) offset else getOffsetToAdd(minuendInstant)), subtrahendInstant + offset)
    }

    private def getOffsetToAdd(instant: Long): Int = {
      val offset = this.iZone.getOffset(instant)
      val sum = instant + offset
      if ((instant ^ sum) < 0 && (instant ^ offset) >= 0) {
        throw new ArithmeticException("Adding time zone offset caused overflow")
      }
      offset
    }

    private def getOffsetFromLocalToSubtract(instant: Long): Int = {
      val offset = this.iZone.getOffsetFromLocal(instant)
      val diff = instant - offset
      if ((instant ^ diff) < 0 && (instant ^ offset) < 0) {
        throw new ArithmeticException("Subtracting time zone offset caused overflow")
      }
      offset
    }

    private def addOffset(instant: Long): Long = iZone.convertUTCToLocal(instant)

    override def equals(obj: Any): Boolean = {
      if (this == obj) {
        return true
      } else if (obj.isInstanceOf[ZonedDurationField]) {
        val other = obj.asInstanceOf[ZonedDurationField]
        return iField == other.iField && iZone == other.iZone
      }
      false
    }

    override def hashCode(): Int = iField.hashCode ^ iZone.hashCode
  }

  @SerialVersionUID(-3968986277775529794L)
  class ZonedDateTimeField(val field: DateTimeField,
                           val zone: DateTimeZone,
                           val durationField: DurationField,
                           val rangeDurationField: DurationField,
                           val leapDurationField: DurationField) extends BaseDateTimeField(field.getType) {

    var iTimeField = useTimeArithmetic(durationField)
    var iField: DateTimeField = null
    var iZone: DateTimeZone = null
    var iDurationField: DurationField = null
    var iRangeDurationField: DurationField = null
    var iLeapDurationField: DurationField = null


    if (!field.isSupported) {
      throw new IllegalArgumentException()
    }

    iField = field
    iZone = zone
    iDurationField = durationField
    iTimeField = useTimeArithmetic(durationField)
    iRangeDurationField = rangeDurationField
    iLeapDurationField = leapDurationField

    def isLenient(): Boolean = iField.isLenient

    def get(instant: Long): Int = {
      val localInstant = iZone.convertUTCToLocal(instant)
      iField.get(localInstant)
    }

    override def getAsText(instant: Long, locale: Locale): String = {
      val localInstant = iZone.convertUTCToLocal(instant)
      iField.getAsText(localInstant, locale)
    }

    override def getAsShortText(instant: Long, locale: Locale): String = {
      val localInstant = iZone.convertUTCToLocal(instant)
      iField.getAsShortText(localInstant, locale)
    }

    override def getAsText(fieldValue: Int, locale: Locale): String = iField.getAsText(fieldValue, locale)

    override def getAsShortText(fieldValue: Int, locale: Locale): String = {
      iField.getAsShortText(fieldValue, locale)
    }

    override def add(instant: Long, value: Int): Long = {
      if (iTimeField) {
        val offset = getOffsetToAdd(instant)
        val localInstant = iField.add(instant + offset, value)
        localInstant - offset
      } else {
        var localInstant = iZone.convertUTCToLocal(instant)
        localInstant = iField.add(localInstant, value)
        iZone.convertLocalToUTC(localInstant, false, instant)
      }
    }

    override def add(instant: Long, value: Long): Long = {
      if (iTimeField) {
        val offset = getOffsetToAdd(instant)
        val localInstant = iField.add(instant + offset, value)
        localInstant - offset
      } else {
        var localInstant = iZone.convertUTCToLocal(instant)
        localInstant = iField.add(localInstant, value)
        iZone.convertLocalToUTC(localInstant, false, instant)
      }
    }

    override def addWrapField(instant: Long, value: Int): Long = {
      if (iTimeField) {
        val offset = getOffsetToAdd(instant)
        val localInstant = iField.addWrapField(instant + offset, value)
        localInstant - offset
      } else {
        var localInstant = iZone.convertUTCToLocal(instant)
        localInstant = iField.addWrapField(localInstant, value)
        iZone.convertLocalToUTC(localInstant, false, instant)
      }
    }

    override def set(instant: Long, value: Int): Long = {
      var localInstant = iZone.convertUTCToLocal(instant)
      localInstant = iField.set(localInstant, value)
      val result = iZone.convertLocalToUTC(localInstant, false, instant)
      if (get(result) != value) {
        val cause = IllegalInstantException.create(localInstant, iZone.getID)
        val ex = IllegalFieldValueException.create(iField.getType, Integer.valueOf(value), cause.getMessage)
        ex.initCause(cause)
        throw ex
      }
      result
    }

    override def set(instant: Long, text: String, locale: Locale): Long = {
      var localInstant = iZone.convertUTCToLocal(instant)
      localInstant = iField.set(localInstant, text, locale)
      iZone.convertLocalToUTC(localInstant, false, instant)
    }

    override def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
      val offset = getOffsetToAdd(subtrahendInstant)
      iField.getDifference(minuendInstant +
        (if (iTimeField) offset else getOffsetToAdd(minuendInstant)), subtrahendInstant + offset)
    }

    override def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
      val offset = getOffsetToAdd(subtrahendInstant)
      iField.getDifferenceAsLong(minuendInstant +
        (if (iTimeField) offset else getOffsetToAdd(minuendInstant)), subtrahendInstant + offset)
    }

    def getDurationField(): DurationField = iDurationField

    def getRangeDurationField(): DurationField = iRangeDurationField

    override def isLeap(instant: Long): Boolean = {
      val localInstant = iZone.convertUTCToLocal(instant)
      iField.isLeap(localInstant)
    }

    override def getLeapAmount(instant: Long): Int = {
      val localInstant = iZone.convertUTCToLocal(instant)
      iField.getLeapAmount(localInstant)
    }

    override def getLeapDurationField(): DurationField = iLeapDurationField

    def roundFloor(instant: Long): Long = {
      var _instant:Long = instant
      if (iTimeField) {
        val offset = getOffsetToAdd(_instant)
        _instant = iField.roundFloor(_instant + offset)
        _instant - offset
      } else {
        var localInstant = iZone.convertUTCToLocal(_instant)
        localInstant = iField.roundFloor(localInstant)
        iZone.convertLocalToUTC(localInstant, false, _instant)
      }
    }

    override def roundCeiling(instant: Long): Long = {
      var _instant:Long = instant
      if (iTimeField) {
        val offset = getOffsetToAdd(_instant)
        _instant = iField.roundCeiling(_instant + offset)
        _instant - offset
      } else {
        var localInstant = iZone.convertUTCToLocal(_instant)
        localInstant = iField.roundCeiling(localInstant)
        iZone.convertLocalToUTC(localInstant, false, _instant)
      }
    }

    override def remainder(instant: Long): Long = {
      val localInstant = iZone.convertUTCToLocal(instant)
      iField.remainder(localInstant)
    }

    def getMinimumValue(): Int = iField.getMinimumValue

    override def getMinimumValue(instant: Long): Int = {
      val localInstant = iZone.convertUTCToLocal(instant)
      iField.getMinimumValue(localInstant)
    }

    override def getMinimumValue(instant: ReadablePartial): Int = iField.getMinimumValue(instant)

    override def getMinimumValue(instant: ReadablePartial, values: Array[Int]): Int = iField.getMinimumValue(instant, values)

    def getMaximumValue(): Int = iField.getMaximumValue

    override def getMaximumValue(instant: Long): Int = {
      val localInstant = iZone.convertUTCToLocal(instant)
      iField.getMaximumValue(localInstant)
    }

    override def getMaximumValue(instant: ReadablePartial): Int = iField.getMaximumValue(instant)

    override def getMaximumValue(instant: ReadablePartial, values: Array[Int]): Int = iField.getMaximumValue(instant, values)

    override def getMaximumTextLength(locale: Locale): Int = iField.getMaximumTextLength(locale)

    override def getMaximumShortTextLength(locale: Locale): Int = {
      iField.getMaximumShortTextLength(locale)
    }

    private def getOffsetToAdd(instant: Long): Int = {
      val offset = this.iZone.getOffset(instant)
      val sum = instant + offset
      if ((instant ^ sum) < 0 && (instant ^ offset) >= 0) {
        throw new ArithmeticException("Adding time zone offset caused overflow")
      }
      offset
    }

    override def equals(obj: Any): Boolean = {
      if (this == obj) {
        return true
      } else if (obj.isInstanceOf[ZonedDateTimeField]) {
        val other = obj.asInstanceOf[ZonedDateTimeField]
        return iField == other.iField && iZone == other.iZone && iDurationField == other.iDurationField &&
          iRangeDurationField == other.iRangeDurationField
      }
      false
    }

    override def hashCode(): Int = iField.hashCode ^ iZone.hashCode
  }
}

@SerialVersionUID(-1079258847191166848L)
class ZonedChronology private (base: Chronology, zone: DateTimeZone) extends AssembledChronology(base,
  zone) {

  override def getZone(): DateTimeZone = getParam.asInstanceOf[DateTimeZone]

  def withUTC(): Chronology = getBase

  def withZone(zone: DateTimeZone): Chronology = {
    var _zone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    if (_zone == getParam) {
      return this
    }
    if (_zone == DateTimeZone.UTC) {
      return getBase
    }
    new ZonedChronology(getBase, _zone)
  }

  override def getDateTimeMillis(year: Int,
                                 monthOfYear: Int,
                                 dayOfMonth: Int,
                                 millisOfDay: Int): Long = {
    localToUTC(getBase.getDateTimeMillis(year, monthOfYear, dayOfMonth, millisOfDay))
  }

  override def getDateTimeMillis(year: Int,
                                 monthOfYear: Int,
                                 dayOfMonth: Int,
                                 hourOfDay: Int,
                                 minuteOfHour: Int,
                                 secondOfMinute: Int,
                                 millisOfSecond: Int): Long = {
    localToUTC(getBase.getDateTimeMillis(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute,
      millisOfSecond))
  }

  override def getDateTimeMillis(instant: Long,
                                 hourOfDay: Int,
                                 minuteOfHour: Int,
                                 secondOfMinute: Int,
                                 millisOfSecond: Int): Long = {
    localToUTC(getBase.getDateTimeMillis(instant + getZone.getOffset(instant), hourOfDay, minuteOfHour,
      secondOfMinute, millisOfSecond))
  }

  private def localToUTC(localInstant: Long): Long = {
    val zone = getZone
    val offset = zone.getOffsetFromLocal(localInstant)
    val utcInstant = localInstant - offset
    val offsetBasedOnUtc = zone.getOffset(utcInstant)
    if (offset != offsetBasedOnUtc) {
      throw IllegalInstantException.create(localInstant, zone.getID)
    }
    utcInstant
  }

  protected def assemble(fields: Fields) {
    val converted = new HashMap[Any, Any]()
    fields.eras = convertField(fields.eras, converted)
    fields.centuries = convertField(fields.centuries, converted)
    fields.years = convertField(fields.years, converted)
    fields.months = convertField(fields.months, converted)
    fields.weekyears = convertField(fields.weekyears, converted)
    fields.weeks = convertField(fields.weeks, converted)
    fields.days = convertField(fields.days, converted)
    fields.halfdays = convertField(fields.halfdays, converted)
    fields.hours = convertField(fields.hours, converted)
    fields.minutes = convertField(fields.minutes, converted)
    fields.seconds = convertField(fields.seconds, converted)
    fields.millis = convertField(fields.millis, converted)
    fields.year = convertField(fields.year, converted)
    fields.yearOfEra = convertField(fields.yearOfEra, converted)
    fields.yearOfCentury = convertField(fields.yearOfCentury, converted)
    fields.centuryOfEra = convertField(fields.centuryOfEra, converted)
    fields.era = convertField(fields.era, converted)
    fields.dayOfWeek = convertField(fields.dayOfWeek, converted)
    fields.dayOfMonth = convertField(fields.dayOfMonth, converted)
    fields.dayOfYear = convertField(fields.dayOfYear, converted)
    fields.monthOfYear = convertField(fields.monthOfYear, converted)
    fields.weekOfWeekyear = convertField(fields.weekOfWeekyear, converted)
    fields.weekyear = convertField(fields.weekyear, converted)
    fields.weekyearOfCentury = convertField(fields.weekyearOfCentury, converted)
    fields.millisOfSecond = convertField(fields.millisOfSecond, converted)
    fields.millisOfDay = convertField(fields.millisOfDay, converted)
    fields.secondOfMinute = convertField(fields.secondOfMinute, converted)
    fields.secondOfDay = convertField(fields.secondOfDay, converted)
    fields.minuteOfHour = convertField(fields.minuteOfHour, converted)
    fields.minuteOfDay = convertField(fields.minuteOfDay, converted)
    fields.hourOfDay = convertField(fields.hourOfDay, converted)
    fields.hourOfHalfday = convertField(fields.hourOfHalfday, converted)
    fields.clockhourOfDay = convertField(fields.clockhourOfDay, converted)
    fields.clockhourOfHalfday = convertField(fields.clockhourOfHalfday, converted)
    fields.halfdayOfDay = convertField(fields.halfdayOfDay, converted)
  }

  private def convertField(field: DurationField, converted: HashMap[Any, Any]): DurationField = {
    if (field == null || !field.isSupported) {
      return field
    }
    if (converted.containsKey(field)) {
      return converted.get(field).asInstanceOf[DurationField]
    }
    val zonedField = new ZonedDurationField(field, getZone)
    converted.put(field, zonedField)
    zonedField
  }

  private def convertField(field: DateTimeField, converted: HashMap[Any, Any]): DateTimeField = {
    if (field == null || !field.isSupported) {
      return field
    }
    if (converted.containsKey(field)) {
      return converted.get(field).asInstanceOf[DateTimeField]
    }
    val zonedField = new ZonedDateTimeField(field, getZone, convertField(field.getDurationField, converted),
      convertField(field.getRangeDurationField, converted), convertField(field.getLeapDurationField,
        converted))
    converted.put(field, zonedField)
    zonedField
  }

  override def equals(obj: Any): Boolean = {
    if (this == obj) {
      return true
    }
    if (obj.isInstanceOf[ZonedChronology] == false) {
      return false
    }
    val chrono = obj.asInstanceOf[ZonedChronology]
    getBase == chrono.getBase && getZone == chrono.getZone
  }

  override def hashCode(): Int = {
    326565 + getZone.hashCode * 11 + getBase.hashCode * 7
  }

  override def toString(): String = {
    "ZonedChronology[" + getBase + ", " + getZone.getID +
      ']'
  }
}
