package org.joda.time.chrono

import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTime
import org.joda.time.DateTimeField
import org.joda.time.DateTimeZone
import org.joda.time.DurationField
import org.joda.time.ReadableDateTime
import org.joda.time.chrono.AssembledChronology.Fields
import org.joda.time.field.DecoratedDateTimeField
import org.joda.time.field.DecoratedDurationField
import org.joda.time.field.FieldUtils
import org.joda.time.format.ISODateTimeFormat
import LimitChronology._

object LimitChronology {

  def getInstance(base: Chronology, lowerLimit: ReadableDateTime, upperLimit: ReadableDateTime): LimitChronology = {
    var _lowerLimit = lowerLimit
    var _upperLimit = upperLimit
    if (base == null) {
      throw new IllegalArgumentException("Must supply a chronology")
    }
    _lowerLimit = if (_lowerLimit == null) null else _lowerLimit.toDateTime()
    _upperLimit = if (_upperLimit == null) null else _upperLimit.toDateTime()
    if (_lowerLimit != null && _upperLimit != null) {
      if (!_lowerLimit.isBefore(_upperLimit)) {
        throw new IllegalArgumentException("The lower limit must be come before than the upper limit")
      }
    }
    new LimitChronology(base, _lowerLimit.asInstanceOf[DateTime], _upperLimit.asInstanceOf[DateTime])
  }
}

@SerialVersionUID(7670866536893052522L)
class LimitChronology private (base: Chronology, val lowerLimit: DateTime, val upperLimit: DateTime)
  extends AssembledChronology(base, null) {

  private var iLowerLimit: DateTime = null
  private var iUpperLimit: DateTime = null
  @transient private var iWithUTC: LimitChronology = null

  iLowerLimit = lowerLimit
  iUpperLimit = upperLimit


  def getLowerLimit(): DateTime = iLowerLimit

  def getUpperLimit(): DateTime = iUpperLimit

  def withUTC(): Chronology = withZone(DateTimeZone.UTC)

  def withZone(zone: DateTimeZone): Chronology = {
    var _zone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    if (_zone == getZone) {
      return this
    }
    if (_zone == DateTimeZone.UTC && iWithUTC != null) {
      return iWithUTC
    }
    var lowerLimit = iLowerLimit
    if (lowerLimit != null) {
      val mdt = lowerLimit.toMutableDateTime()
      mdt.setZoneRetainFields(_zone)
      lowerLimit = mdt.toDateTime()
    }
    var upperLimit = iUpperLimit
    if (upperLimit != null) {
      val mdt = upperLimit.toMutableDateTime()
      mdt.setZoneRetainFields(_zone)
      upperLimit = mdt.toDateTime()
    }
    val chrono = getInstance(getBase.withZone(_zone), lowerLimit, upperLimit)
    if (_zone == DateTimeZone.UTC) {
      iWithUTC = chrono
    }
    chrono
  }

  override def getDateTimeMillis(year: Int,
                                 monthOfYear: Int,
                                 dayOfMonth: Int,
                                 millisOfDay: Int): Long = {
    val instant = getBase.getDateTimeMillis(year, monthOfYear, dayOfMonth, millisOfDay)
    checkLimits(instant, "resulting")
    instant
  }

  override def getDateTimeMillis(year: Int,
                                 monthOfYear: Int,
                                 dayOfMonth: Int,
                                 hourOfDay: Int,
                                 minuteOfHour: Int,
                                 secondOfMinute: Int,
                                 millisOfSecond: Int): Long = {
    val instant = getBase.getDateTimeMillis(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute,
      millisOfSecond)
    checkLimits(instant, "resulting")
    instant
  }

  override def getDateTimeMillis(instant: Long,
                                 hourOfDay: Int,
                                 minuteOfHour: Int,
                                 secondOfMinute: Int,
                                 millisOfSecond: Int): Long = {
    var _instant = instant
    checkLimits(_instant, null)
    _instant = getBase.getDateTimeMillis(_instant, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond)
    checkLimits(_instant, "resulting")
    _instant
  }

  protected def assemble(fields: Fields) {
    val converted = new collection.mutable.HashMap[Any, Any]()
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

  private def convertField(field: DurationField, converted: collection.mutable.HashMap[Any, Any]): DurationField = {
    if (field == null || !field.isSupported) {
      return field
    }
    if (converted.contains(field)) {
      return converted.get(field).asInstanceOf[DurationField]
    }
    val limitField = new LimitDurationField(field)
    converted.put(field, limitField)
    limitField
  }

  private def convertField(field: DateTimeField, converted: collection.mutable.HashMap[Any, Any]): DateTimeField = {
    if (field == null || !field.isSupported) {
      return field
    }
    if (converted.contains(field)) {
      return converted.get(field).asInstanceOf[DateTimeField]
    }
    val limitField = new LimitDateTimeField(field, convertField(field.getDurationField, converted), convertField(field.getRangeDurationField,
      converted), convertField(field.getLeapDurationField, converted))
    converted.put(field, limitField)
    limitField
  }

  def checkLimits(instant: Long, desc: String) {
    var limit: DateTime = null
    if ( {
      limit = iLowerLimit; limit
    } != null && instant < limit.getMillis) {
      throw new LimitException(desc, true)
    }
    if ( {
      limit = iUpperLimit; limit
    } != null && instant >= limit.getMillis) {
      throw new LimitException(desc, false)
    }
  }

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    }
    if (obj.isInstanceOf[LimitChronology] == false) {
      return false
    }
    val chrono = obj.asInstanceOf[LimitChronology]
    getBase == chrono.getBase && FieldUtils.==(getLowerLimit, chrono.getLowerLimit) &&
      FieldUtils.==(getUpperLimit, chrono.getUpperLimit)
  }

  override def hashCode(): Int = {
    var hash = 317351877
    hash += (if (getLowerLimit != null) getLowerLimit.hashCode else 0)
    hash += (if (getUpperLimit != null) getUpperLimit.hashCode else 0)
    hash += getBase.hashCode * 7
    hash
  }

  override def toString(): String = {
    "LimitChronology[" + getBase.toString + ", " +
      (if (getLowerLimit == null) "NoLimit" else getLowerLimit.toString) +
      ", " +
      (if (getUpperLimit == null) "NoLimit" else getUpperLimit.toString) +
      ']'
  }

  @SerialVersionUID(-5924689995607498581L)
  private class LimitException(desc: String, private val iIsLow: Boolean) extends IllegalArgumentException(desc) {

    override def getMessage(): String = {
      val buf = new StringBuffer(85)
      buf.append("The")
      val desc = super.getMessage
      if (desc != null) {
        buf.append(' ')
        buf.append(desc)
      }
      buf.append(" instant is ")
      var p = ISODateTimeFormat.dateTime()
      p = p.withChronology(getBase)
      if (iIsLow) {
        buf.append("below the supported minimum of ")
        p.printTo(buf, getLowerLimit.getMillis)
      } else {
        buf.append("above the supported maximum of ")
        p.printTo(buf, getUpperLimit.getMillis)
      }
      buf.append(" (")
      buf.append(getBase)
      buf.append(')')
      buf.toString
    }

    override def toString(): String = {
      "IllegalArgumentException: " + getMessage
    }
  }

  @SerialVersionUID(8049297699408782284L)
  private class LimitDurationField(field: DurationField) extends DecoratedDurationField(field, field.getType) {

    override def getValue(duration: Long, instant: Long): Int = {
      checkLimits(instant, null)
      getWrappedField.getValue(duration, instant)
    }

    override def getValueAsLong(duration: Long, instant: Long): Long = {
      checkLimits(instant, null)
      getWrappedField.getValueAsLong(duration, instant)
    }

    override def getMillis(value: Int, instant: Long): Long = {
      checkLimits(instant, null)
      getWrappedField.getMillis(value, instant)
    }

    override def getMillis(value: Long, instant: Long): Long = {
      checkLimits(instant, null)
      getWrappedField.getMillis(value, instant)
    }

    override def add(instant: Long, amount: Int): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.add(instant, amount)
      checkLimits(result, "resulting")
      result
    }

    override def add(instant: Long, amount: Long): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.add(instant, amount)
      checkLimits(result, "resulting")
      result
    }

    override def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
      checkLimits(minuendInstant, "minuend")
      checkLimits(subtrahendInstant, "subtrahend")
      getWrappedField.getDifference(minuendInstant, subtrahendInstant)
    }

    override def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
      checkLimits(minuendInstant, "minuend")
      checkLimits(subtrahendInstant, "subtrahend")
      getWrappedField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
    }
  }

  @SerialVersionUID(-2435306746995699312L)
  private class LimitDateTimeField(field: DateTimeField,
                                   private val durationField: DurationField,
                                   private val rangeDurationField: DurationField,
                                   private val leapDurationField: DurationField) extends DecoratedDateTimeField(field, field.getType) {

    private var iDurationField: DurationField = null
    private var iRangeDurationField: DurationField = null
    private var iLeapDurationField: DurationField  = null

    iDurationField = durationField
    iRangeDurationField = rangeDurationField
    iLeapDurationField = leapDurationField

    override def get(instant: Long): Int = {
      checkLimits(instant, null)
      getWrappedField.get(instant)
    }

    override def getAsText(instant: Long, locale: Locale): String = {
      checkLimits(instant, null)
      getWrappedField.getAsText(instant, locale)
    }

    override def getAsShortText(instant: Long, locale: Locale): String = {
      checkLimits(instant, null)
      getWrappedField.getAsShortText(instant, locale)
    }

    override def add(instant: Long, amount: Int): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.add(instant, amount)
      checkLimits(result, "resulting")
      result
    }

    override def add(instant: Long, amount: Long): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.add(instant, amount)
      checkLimits(result, "resulting")
      result
    }

    override def addWrapField(instant: Long, amount: Int): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.addWrapField(instant, amount)
      checkLimits(result, "resulting")
      result
    }

    override def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
      checkLimits(minuendInstant, "minuend")
      checkLimits(subtrahendInstant, "subtrahend")
      getWrappedField.getDifference(minuendInstant, subtrahendInstant)
    }

    override def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
      checkLimits(minuendInstant, "minuend")
      checkLimits(subtrahendInstant, "subtrahend")
      getWrappedField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
    }

    override def set(instant: Long, value: Int): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.set(instant, value)
      checkLimits(result, "resulting")
      result
    }

    override def set(instant: Long, text: String, locale: Locale): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.set(instant, text, locale)
      checkLimits(result, "resulting")
      result
    }

    override def getDurationField(): DurationField = iDurationField

    override def getRangeDurationField(): DurationField = iRangeDurationField

    override def isLeap(instant: Long): Boolean = {
      checkLimits(instant, null)
      getWrappedField.isLeap(instant)
    }

    override def getLeapAmount(instant: Long): Int = {
      checkLimits(instant, null)
      getWrappedField.getLeapAmount(instant)
    }

    override def getLeapDurationField(): DurationField = iLeapDurationField

    override def roundFloor(instant: Long): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.roundFloor(instant)
      checkLimits(result, "resulting")
      result
    }

    override def roundCeiling(instant: Long): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.roundCeiling(instant)
      checkLimits(result, "resulting")
      result
    }

    override def roundHalfFloor(instant: Long): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.roundHalfFloor(instant)
      checkLimits(result, "resulting")
      result
    }

    override def roundHalfCeiling(instant: Long): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.roundHalfCeiling(instant)
      checkLimits(result, "resulting")
      result
    }

    override def roundHalfEven(instant: Long): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.roundHalfEven(instant)
      checkLimits(result, "resulting")
      result
    }

    override def remainder(instant: Long): Long = {
      checkLimits(instant, null)
      val result = getWrappedField.remainder(instant)
      checkLimits(result, "resulting")
      result
    }

    override def getMinimumValue(instant: Long): Int = {
      checkLimits(instant, null)
      getWrappedField.getMinimumValue(instant)
    }

    override def getMaximumValue(instant: Long): Int = {
      checkLimits(instant, null)
      getWrappedField.getMaximumValue(instant)
    }

    override def getMaximumTextLength(locale: Locale): Int = {
      getWrappedField.getMaximumTextLength(locale)
    }

    override def getMaximumShortTextLength(locale: Locale): Int = {
      getWrappedField.getMaximumShortTextLength(locale)
    }
  }
}
