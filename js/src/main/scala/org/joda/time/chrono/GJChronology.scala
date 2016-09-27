package org.joda.time.chrono

import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeField
import org.joda.time.DateTimeUtils
import org.joda.time.DateTimeZone
import org.joda.time.DurationField
import org.joda.time.IllegalFieldValueException
import org.joda.time.Instant
import org.joda.time.LocalDate
import org.joda.time.ReadableInstant
import org.joda.time.ReadablePartial
import org.joda.time.chrono.AssembledChronology.Fields
import org.joda.time.field.BaseDateTimeField
import org.joda.time.field.DecoratedDurationField
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat

object GJChronology {

  private val cCache =
    new collection.mutable.HashMap[GJCacheKey, GJChronology]()

  val DEFAULT_CUTOVER = new Instant(-12219292800000L)

  def getInstanceUTC(): GJChronology = {
    getInstance(DateTimeZone.UTC, DEFAULT_CUTOVER, 4)
  }

  def getInstance(): GJChronology = {
    getInstance(DateTimeZone.getDefault, DEFAULT_CUTOVER, 4)
  }

  def getInstance(zone: DateTimeZone): GJChronology =
    getInstance(zone, DEFAULT_CUTOVER, 4)

  def getInstance(zone: DateTimeZone,
                  gregorianCutover: ReadableInstant): GJChronology =
    getInstance(zone, gregorianCutover, 4)

  def getInstance(zone: DateTimeZone,
                  gregorianCutover: ReadableInstant,
                  minDaysInFirstWeek: Int): GJChronology = {
    var _zone: DateTimeZone = zone
    _zone = DateTimeUtils.getZone(_zone)
    var cutoverInstant: Instant = null
    if (gregorianCutover == null) {
      cutoverInstant = GJChronology.DEFAULT_CUTOVER
    } else {
      cutoverInstant = gregorianCutover.toInstant()
      val cutoverDate = new LocalDate(cutoverInstant.getMillis,
                                      GregorianChronology.getInstance(_zone))
      if (cutoverDate.getYear <= 0) {
        throw new IllegalArgumentException(
          "Cutover too early. Must be on or after 0001-01-01.")
      }
    }
    val cacheKey = new GJCacheKey(_zone, cutoverInstant, minDaysInFirstWeek)
    var chrono = cCache.get(cacheKey).orNull
    if (chrono == null) {
      if (_zone == DateTimeZone.UTC) {
        chrono = new GJChronology(
          JulianChronology.getInstance(_zone, minDaysInFirstWeek),
          GregorianChronology.getInstance(_zone, minDaysInFirstWeek),
          cutoverInstant)
      } else {
        chrono =
          getInstance(DateTimeZone.UTC, cutoverInstant, minDaysInFirstWeek)
        chrono = new GJChronology(ZonedChronology.getInstance(chrono, _zone),
                                  chrono.iJulianChronology,
                                  chrono.iGregorianChronology,
                                  chrono.iCutoverInstant)
      }
      val oldChrono = if (cCache.get(cacheKey).isEmpty) {
        cCache(cacheKey) = chrono
        chrono
      } else {
        chrono
      }

      if (oldChrono != null) {
        chrono = oldChrono
      }
    }
    chrono
  }

  def getInstance(zone: DateTimeZone,
                  gregorianCutover: Long,
                  minDaysInFirstWeek: Int): GJChronology = {
    var cutoverInstant: Instant = null
    cutoverInstant =
      if (gregorianCutover == DEFAULT_CUTOVER.getMillis) null
      else new Instant(gregorianCutover)
    getInstance(zone, cutoverInstant, minDaysInFirstWeek)
  }
}

@SerialVersionUID(-2545574827706931671L)
class GJChronology extends AssembledChronology {

  private def this(julian: JulianChronology,
                   gregorian: GregorianChronology,
                   cutoverInstant: Instant) {
    this()
    super
      .auxConstructor(null, Array[AnyRef](julian, gregorian, cutoverInstant))
  }

  private def this(base: Chronology,
                   julian: JulianChronology,
                   gregorian: GregorianChronology,
                   cutoverInstant: Instant) {
    this()
    super
      .auxConstructor(base, Array[AnyRef](julian, gregorian, cutoverInstant))
  }

  private def convertByYear(instant: Long,
                            from: Chronology,
                            to: Chronology): Long = {
    to.getDateTimeMillis(from.year().get(instant),
                         from.monthOfYear().get(instant),
                         from.dayOfMonth().get(instant),
                         from.millisOfDay().get(instant))
  }

  private def convertByWeekyear(instant: Long,
                                from: Chronology,
                                to: Chronology): Long = {
    var newInstant: Long = 0l
    newInstant = to.weekyear().set(0, from.weekyear().get(instant))
    newInstant =
      to.weekOfWeekyear().set(newInstant, from.weekOfWeekyear().get(instant))
    newInstant = to.dayOfWeek().set(newInstant, from.dayOfWeek().get(instant))
    newInstant =
      to.millisOfDay().set(newInstant, from.millisOfDay().get(instant))
    newInstant
  }

  @SerialVersionUID(4097975388007713084L)
  private class LinkedDurationField(durationField: DurationField,
                                    dateTimeFiled: ImpreciseCutoverField)
      extends DecoratedDurationField(durationField, durationField.getType) {

    private val iField: ImpreciseCutoverField = dateTimeFiled

    override def add(instant: Long, value: Int): Long =
      iField.add(instant, value)

    override def add(instant: Long, value: Long): Long =
      iField.add(instant, value)

    override def getDifference(minuendInstant: Long,
                               subtrahendInstant: Long): Int = {
      iField.getDifference(minuendInstant, subtrahendInstant)
    }

    override def getDifferenceAsLong(minuendInstant: Long,
                                     subtrahendInstant: Long): Long = {
      iField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
    }
  }

  private var iJulianChronology: JulianChronology = null
  private var iGregorianChronology: GregorianChronology = null
  private var iCutoverInstant: Instant = null
  private var iCutoverMillis: Long = _
  private var iGapDuration: Long = _

  private def readResolve(): AnyRef = {
    GJChronology
      .getInstance(getZone, iCutoverInstant, getMinimumDaysInFirstWeek)
  }

  override def getZone(): DateTimeZone = {
    var base: Chronology = null
    if ({
      base = getBase; base
    } != null) {
      return base.getZone
    }
    DateTimeZone.UTC
  }

  def withUTC(): Chronology = withZone(DateTimeZone.UTC)

  def withZone(zone: DateTimeZone): Chronology = {
    var _zone: DateTimeZone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    if (_zone == getZone) {
      return this
    }
    GJChronology.getInstance(_zone, iCutoverInstant, getMinimumDaysInFirstWeek)
  }

  override def getDateTimeMillis(year: Int,
                                 monthOfYear: Int,
                                 dayOfMonth: Int,
                                 millisOfDay: Int): Long = {
    var base: Chronology = null
    if ({
      base = getBase; base
    } != null) {
      return base.getDateTimeMillis(year, monthOfYear, dayOfMonth, millisOfDay)
    }
    var instant = iGregorianChronology
      .getDateTimeMillis(year, monthOfYear, dayOfMonth, millisOfDay)
    if (instant < iCutoverMillis) {
      instant = iJulianChronology
        .getDateTimeMillis(year, monthOfYear, dayOfMonth, millisOfDay)
      if (instant >= iCutoverMillis) {
        throw new IllegalArgumentException("Specified date does not exist")
      }
    }
    instant
  }

  override def getDateTimeMillis(year: Int,
                                 monthOfYear: Int,
                                 dayOfMonth: Int,
                                 hourOfDay: Int,
                                 minuteOfHour: Int,
                                 secondOfMinute: Int,
                                 millisOfSecond: Int): Long = {
    var base: Chronology = null
    if ({
      base = getBase; base
    } != null) {
      return base.getDateTimeMillis(year,
                                    monthOfYear,
                                    dayOfMonth,
                                    hourOfDay,
                                    minuteOfHour,
                                    secondOfMinute,
                                    millisOfSecond)
    }
    var instant: Long = 0l
    try {
      instant = iGregorianChronology.getDateTimeMillis(year,
                                                       monthOfYear,
                                                       dayOfMonth,
                                                       hourOfDay,
                                                       minuteOfHour,
                                                       secondOfMinute,
                                                       millisOfSecond)
    } catch {
      case ex: IllegalFieldValueException => {
        if (monthOfYear != 2 || dayOfMonth != 29) {
          throw ex
        }
        instant = iGregorianChronology.getDateTimeMillis(year,
                                                         monthOfYear,
                                                         28,
                                                         hourOfDay,
                                                         minuteOfHour,
                                                         secondOfMinute,
                                                         millisOfSecond)
        if (instant >= iCutoverMillis) {
          throw ex
        }
      }
    }
    if (instant < iCutoverMillis) {
      instant = iJulianChronology.getDateTimeMillis(year,
                                                    monthOfYear,
                                                    dayOfMonth,
                                                    hourOfDay,
                                                    minuteOfHour,
                                                    secondOfMinute,
                                                    millisOfSecond)
      if (instant >= iCutoverMillis) {
        throw new IllegalArgumentException("Specified date does not exist")
      }
    }
    instant
  }

  def getGregorianCutover(): Instant = iCutoverInstant

  def getMinimumDaysInFirstWeek(): Int = {
    iGregorianChronology.getMinimumDaysInFirstWeek
  }

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    }
    if (obj.isInstanceOf[GJChronology]) {
      val chrono = obj.asInstanceOf[GJChronology]
      return iCutoverMillis == chrono.iCutoverMillis &&
        getMinimumDaysInFirstWeek == chrono.getMinimumDaysInFirstWeek &&
        getZone == chrono.getZone
    }
    false
  }

  override def hashCode(): Int = {
    "GJ".hashCode * 11 + getZone.hashCode + getMinimumDaysInFirstWeek +
      iCutoverInstant.hashCode
  }

  override def toString(): String = {
    val sb = new StringBuffer(60)
    sb.append("GJChronology")
    sb.append('[')
    sb.append(getZone.getID)
    if (iCutoverMillis != GJChronology.DEFAULT_CUTOVER.getMillis) {
      sb.append(",cutover=")
      var printer: DateTimeFormatter = null
      printer =
        if (withUTC().dayOfYear().remainder(iCutoverMillis) == 0)
          ISODateTimeFormat.date()
        else ISODateTimeFormat.dateTime()
      printer.withChronology(withUTC()).printTo(sb, iCutoverMillis)
    }
    if (getMinimumDaysInFirstWeek != 4) {
      sb.append(",mdfw=")
      sb.append(getMinimumDaysInFirstWeek)
    }
    sb.append(']')
    sb.toString
  }

  protected def assemble(fields: Fields) {
    val params = getParam.asInstanceOf[Array[Any]]
    val julian = params(0).asInstanceOf[JulianChronology]
    val gregorian = params(1).asInstanceOf[GregorianChronology]
    val cutoverInstant = params(2).asInstanceOf[Instant]
    iCutoverMillis = cutoverInstant.getMillis
    iJulianChronology = julian
    iGregorianChronology = gregorian
    iCutoverInstant = cutoverInstant
    if (getBase != null) {
      return
    }
    if (julian.getMinimumDaysInFirstWeek != gregorian.getMinimumDaysInFirstWeek) {
      throw new IllegalArgumentException()
    }
    iGapDuration = iCutoverMillis - julianToGregorianByYear(iCutoverMillis)
    fields.copyFieldsFrom(gregorian)
    if (gregorian.millisOfDay().get(iCutoverMillis) == 0) {
      fields.millisOfSecond = new CutoverField(julian.millisOfSecond(),
                                               fields.millisOfSecond,
                                               iCutoverMillis)
      fields.millisOfDay = new CutoverField(julian.millisOfDay(),
                                            fields.millisOfDay,
                                            iCutoverMillis)
      fields.secondOfMinute = new CutoverField(julian.secondOfMinute(),
                                               fields.secondOfMinute,
                                               iCutoverMillis)
      fields.secondOfDay = new CutoverField(julian.secondOfDay(),
                                            fields.secondOfDay,
                                            iCutoverMillis)
      fields.minuteOfHour = new CutoverField(julian.minuteOfHour(),
                                             fields.minuteOfHour,
                                             iCutoverMillis)
      fields.minuteOfDay = new CutoverField(julian.minuteOfDay(),
                                            fields.minuteOfDay,
                                            iCutoverMillis)
      fields.hourOfDay =
        new CutoverField(julian.hourOfDay(), fields.hourOfDay, iCutoverMillis)
      fields.hourOfHalfday = new CutoverField(julian.hourOfHalfday(),
                                              fields.hourOfHalfday,
                                              iCutoverMillis)
      fields.clockhourOfDay = new CutoverField(julian.clockhourOfDay(),
                                               fields.clockhourOfDay,
                                               iCutoverMillis)
      fields.clockhourOfHalfday = new CutoverField(julian.clockhourOfHalfday(),
                                                   fields.clockhourOfHalfday,
                                                   iCutoverMillis)
      fields.halfdayOfDay = new CutoverField(julian.halfdayOfDay(),
                                             fields.halfdayOfDay,
                                             iCutoverMillis)
    }
    fields.era = new CutoverField(julian.era(), fields.era, iCutoverMillis)
    fields.year =
      new ImpreciseCutoverField(julian.year(), fields.year, iCutoverMillis)
    fields.years = fields.year.getDurationField
    fields.yearOfEra = new ImpreciseCutoverField(julian.yearOfEra(),
                                                 fields.yearOfEra,
                                                 fields.years,
                                                 iCutoverMillis)
    fields.centuryOfEra = new ImpreciseCutoverField(julian.centuryOfEra(),
                                                    fields.centuryOfEra,
                                                    iCutoverMillis)
    fields.centuries = fields.centuryOfEra.getDurationField
    fields.yearOfCentury = new ImpreciseCutoverField(julian.yearOfCentury(),
                                                     fields.yearOfCentury,
                                                     fields.years,
                                                     fields.centuries,
                                                     iCutoverMillis)
    fields.monthOfYear = new ImpreciseCutoverField(julian.monthOfYear(),
                                                   fields.monthOfYear,
                                                   null,
                                                   fields.years,
                                                   iCutoverMillis)
    fields.months = fields.monthOfYear.getDurationField
    fields.weekyear = new ImpreciseCutoverField(julian.weekyear(),
                                                fields.weekyear,
                                                null,
                                                iCutoverMillis,
                                                true)
    fields.weekyears = fields.weekyear.getDurationField
    fields.weekyearOfCentury = new ImpreciseCutoverField(
      julian.weekyearOfCentury(),
      fields.weekyearOfCentury,
      fields.weekyears,
      fields.centuries,
      iCutoverMillis)

    {
      val cutover = gregorian.year().roundCeiling(iCutoverMillis)
      fields.dayOfYear = new CutoverField(julian.dayOfYear(),
                                          fields.dayOfYear,
                                          fields.years,
                                          cutover,
                                          false)
    }

    {
      val cutover = gregorian.weekyear().roundCeiling(iCutoverMillis)
      fields.weekOfWeekyear = new CutoverField(julian.weekOfWeekyear(),
                                               fields.weekOfWeekyear,
                                               fields.weekyears,
                                               cutover,
                                               true)
    }

    val cf =
      new CutoverField(julian.dayOfMonth(), fields.dayOfMonth, iCutoverMillis)
    cf.iRangeDurationField = fields.months
    fields.dayOfMonth = cf
  }

  def julianToGregorianByYear(instant: Long): Long = {
    convertByYear(instant, iJulianChronology, iGregorianChronology)
  }

  def gregorianToJulianByYear(instant: Long): Long = {
    convertByYear(instant, iGregorianChronology, iJulianChronology)
  }

  def julianToGregorianByWeekyear(instant: Long): Long = {
    convertByWeekyear(instant, iJulianChronology, iGregorianChronology)
  }

  def gregorianToJulianByWeekyear(instant: Long): Long = {
    convertByWeekyear(instant, iGregorianChronology, iJulianChronology)
  }

  @SerialVersionUID(3528501219481026402L)
  private class CutoverField(val julianField: DateTimeField,
                             val gregorianField: DateTimeField,
                             protected var rangeField: DurationField,
                             val cutoverMillis: Long,
                             val convertByWeekyear: Boolean)
      extends BaseDateTimeField(gregorianField.getType) {

    var iJulianField: DateTimeField = null
    var iGregorianField: DateTimeField = null
    var iRangeDurationField: DurationField = null
    var iCutover: Long = _
    var iConvertByWeekyear: Boolean = _

    protected var iDurationField: DurationField =
      gregorianField.getDurationField

    iJulianField = julianField
    iGregorianField = gregorianField
    iCutover = cutoverMillis
    iConvertByWeekyear = convertByWeekyear
    // Although average length of Julian and Gregorian years differ,
    // use the Gregorian duration field because it is more accurate.
    if (rangeField == null) {
      rangeField = gregorianField.getRangeDurationField()
      if (rangeField == null) {
        rangeField = julianField.getRangeDurationField()
      }
    }
    iRangeDurationField = rangeField

    if (rangeField == null) {
      rangeField = gregorianField.getRangeDurationField
      if (rangeField == null) {
        rangeField = julianField.getRangeDurationField
      }
    }

    def this(julianField: DateTimeField,
             gregorianField: DateTimeField,
             cutoverMillis: Long,
             convertByWeekyear: Boolean) {
      this(julianField, gregorianField, null, cutoverMillis, convertByWeekyear)
    }

    def this(julianField: DateTimeField,
             gregorianField: DateTimeField,
             cutoverMillis: Long) {
      this(julianField, gregorianField, cutoverMillis, false)
    }

    def isLenient(): Boolean = false

    def get(instant: Long): Int = {
      if (instant >= iCutover) {
        iGregorianField.get(instant)
      } else {
        iJulianField.get(instant)
      }
    }

    override def getAsText(instant: Long, locale: Locale): String = {
      if (instant >= iCutover) {
        iGregorianField.getAsText(instant, locale)
      } else {
        iJulianField.getAsText(instant, locale)
      }
    }

    override def getAsText(fieldValue: Int, locale: Locale): String = {
      iGregorianField.getAsText(fieldValue, locale)
    }

    override def getAsShortText(instant: Long, locale: Locale): String = {
      if (instant >= iCutover) {
        iGregorianField.getAsShortText(instant, locale)
      } else {
        iJulianField.getAsShortText(instant, locale)
      }
    }

    override def getAsShortText(fieldValue: Int, locale: Locale): String = {
      iGregorianField.getAsShortText(fieldValue, locale)
    }

    override def add(instant: Long, value: Int): Long =
      iGregorianField.add(instant, value)

    override def add(instant: Long, value: Long): Long =
      iGregorianField.add(instant, value)

    override def add(partial: ReadablePartial,
                     fieldIndex: Int,
                     values: Array[Int],
                     valueToAdd: Int): Array[Int] = {
      if (valueToAdd == 0) {
        return values
      }
      if (DateTimeUtils.isContiguous(partial)) {
        var instant = 0L
        for (i <- 0 until partial.size()) {
          instant = partial
            .getFieldType(i)
            .getField(GJChronology.this)
            .set(instant, values(i))
        }
        instant = add(instant, valueToAdd)
        GJChronology.this.get(partial, instant)
      } else {
        super.add(partial, fieldIndex, values, valueToAdd)
      }
    }

    override def getDifference(minuendInstant: Long,
                               subtrahendInstant: Long): Int = {
      iGregorianField.getDifference(minuendInstant, subtrahendInstant)
    }

    override def getDifferenceAsLong(minuendInstant: Long,
                                     subtrahendInstant: Long): Long = {
      iGregorianField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
    }

    def set(instant: Long, value: Int): Long = {
      var _instant: Long = instant
      if (_instant >= iCutover) {
        _instant = iGregorianField.set(_instant, value)
        if (_instant < iCutover) {
          if (_instant + iGapDuration < iCutover) {
            _instant = gregorianToJulian(_instant)
          }
          if (get(_instant) != value) {
            throw IllegalFieldValueException.create(iGregorianField.getType,
                                                    Integer.valueOf(value),
                                                    null,
                                                    null)
          }
        }
      } else {
        _instant = iJulianField.set(_instant, value)
        if (_instant >= iCutover) {
          if (_instant - iGapDuration >= iCutover) {
            _instant = julianToGregorian(_instant)
          }
          if (get(_instant) != value) {
            throw IllegalFieldValueException
              .create(iJulianField.getType, Integer.valueOf(value), null, null)
          }
        }
      }
      _instant
    }

    override def set(instant: Long, text: String, locale: Locale): Long = {
      var _instant: Long = instant
      if (_instant >= iCutover) {
        _instant = iGregorianField.set(_instant, text, locale)
        if (_instant < iCutover) {
          if (_instant + iGapDuration < iCutover) {
            _instant = gregorianToJulian(_instant)
          }
        }
      } else {
        _instant = iJulianField.set(_instant, text, locale)
        if (_instant >= iCutover) {
          if (_instant - iGapDuration >= iCutover) {
            _instant = julianToGregorian(_instant)
          }
        }
      }
      _instant
    }

    def getDurationField(): DurationField = iDurationField

    def getRangeDurationField(): DurationField = iRangeDurationField

    override def isLeap(instant: Long): Boolean = {
      if (instant >= iCutover) {
        iGregorianField.isLeap(instant)
      } else {
        iJulianField.isLeap(instant)
      }
    }

    override def getLeapAmount(instant: Long): Int = {
      if (instant >= iCutover) {
        iGregorianField.getLeapAmount(instant)
      } else {
        iJulianField.getLeapAmount(instant)
      }
    }

    override def getLeapDurationField(): DurationField =
      iGregorianField.getLeapDurationField

    def getMinimumValue(): Int = iJulianField.getMinimumValue

    override def getMinimumValue(partial: ReadablePartial): Int =
      iJulianField.getMinimumValue(partial)

    override def getMinimumValue(partial: ReadablePartial,
                                 values: Array[Int]): Int = {
      iJulianField.getMinimumValue(partial, values)
    }

    override def getMinimumValue(instant: Long): Int = {
      var _instant: Long = instant
      if (_instant < iCutover) {
        return iJulianField.getMinimumValue(_instant)
      }
      var min = iGregorianField.getMinimumValue(_instant)
      _instant = iGregorianField.set(_instant, min)
      if (_instant < iCutover) {
        min = iGregorianField.get(iCutover)
      }
      min
    }

    def getMaximumValue(): Int = iGregorianField.getMaximumValue

    override def getMaximumValue(instant: Long): Int = {
      var _instant: Long = instant
      if (_instant >= iCutover) {
        return iGregorianField.getMaximumValue(_instant)
      }
      var max = iJulianField.getMaximumValue(_instant)
      _instant = iJulianField.set(_instant, max)
      if (_instant >= iCutover) {
        max = iJulianField.get(iJulianField.add(iCutover, -1))
      }
      max
    }

    override def getMaximumValue(partial: ReadablePartial): Int = {
      val instant = GJChronology.getInstanceUTC.set(partial, 0L)
      getMaximumValue(instant)
    }

    override def getMaximumValue(partial: ReadablePartial,
                                 values: Array[Int]): Int = {
      val chrono = GJChronology.getInstanceUTC
      var instant = 0L
      for (i <- 0 until partial.size()) {
        val field = partial.getFieldType(i).getField(chrono)
        if (values(i) <= field.getMaximumValue(instant)) {
          instant = field.set(instant, values(i))
        }
      }
      getMaximumValue(instant)
    }

    def roundFloor(instant: Long): Long = {
      var _instant: Long = instant
      if (_instant >= iCutover) {
        _instant = iGregorianField.roundFloor(_instant)
        if (_instant < iCutover) {
          if (_instant + iGapDuration < iCutover) {
            _instant = gregorianToJulian(_instant)
          }
        }
      } else {
        _instant = iJulianField.roundFloor(_instant)
      }
      _instant
    }

    override def roundCeiling(instant: Long): Long = {
      var _instant: Long = instant
      if (_instant >= iCutover) {
        _instant = iGregorianField.roundCeiling(_instant)
      } else {
        _instant = iJulianField.roundCeiling(_instant)
        if (_instant >= iCutover) {
          if (_instant - iGapDuration >= iCutover) {
            _instant = julianToGregorian(_instant)
          }
        }
      }
      _instant
    }

    override def getMaximumTextLength(locale: Locale): Int = {
      Math.max(iJulianField.getMaximumTextLength(locale),
               iGregorianField.getMaximumTextLength(locale))
    }

    override def getMaximumShortTextLength(locale: Locale): Int = {
      Math.max(iJulianField.getMaximumShortTextLength(locale),
               iGregorianField.getMaximumShortTextLength(locale))
    }

    protected def julianToGregorian(instant: Long): Long = {
      if (iConvertByWeekyear) {
        julianToGregorianByWeekyear(instant)
      } else {
        julianToGregorianByYear(instant)
      }
    }

    protected def gregorianToJulian(instant: Long): Long = {
      if (iConvertByWeekyear) {
        gregorianToJulianByWeekyear(instant)
      } else {
        gregorianToJulianByYear(instant)
      }
    }
  }

  @SerialVersionUID(3410248757173576441L)
  private class ImpreciseCutoverField(julianField: DateTimeField,
                                      gregorianField: DateTimeField,
                                      var durationField: DurationField,
                                      cutoverMillis: Long,
                                      convertByWeekyear: Boolean)
      extends CutoverField(julianField,
                           gregorianField,
                           cutoverMillis,
                           convertByWeekyear) {

    if (durationField == null) {
      durationField = new LinkedDurationField(iDurationField, this)
    }

    iDurationField = durationField

    def this(julianField: DateTimeField,
             gregorianField: DateTimeField,
             cutoverMillis: Long) {
      this(julianField, gregorianField, null, cutoverMillis, false)
    }

    def this(julianField: DateTimeField,
             gregorianField: DateTimeField,
             durationField: DurationField,
             cutoverMillis: Long) {
      this(julianField, gregorianField, durationField, cutoverMillis, false)
    }

    def this(julianField: DateTimeField,
             gregorianField: DateTimeField,
             durationField: DurationField,
             rangeDurationField: DurationField,
             cutoverMillis: Long) {
      this(julianField, gregorianField, durationField, cutoverMillis, false)
      iRangeDurationField = rangeDurationField
    }

    override def add(instant: Long, value: Int): Long = {
      var _instant: Long = instant
      if (_instant >= iCutover) {
        _instant = iGregorianField.add(_instant, value)
        if (_instant < iCutover) {
          if (_instant + iGapDuration < iCutover) {
            if (iConvertByWeekyear) {
              val wyear = iGregorianChronology.weekyear().get(_instant)
              if (wyear <= 0) {
                _instant = iGregorianChronology.weekyear().add(_instant, -1)
              }
            } else {
              val year = iGregorianChronology.year().get(_instant)
              if (year <= 0) {
                _instant = iGregorianChronology.year().add(_instant, -1)
              }
            }
            _instant = gregorianToJulian(_instant)
          }
        }
      } else {
        _instant = iJulianField.add(_instant, value)
        if (_instant >= iCutover) {
          if (_instant - iGapDuration >= iCutover) {
            _instant = julianToGregorian(_instant)
          }
        }
      }
      _instant
    }

    override def add(instant: Long, value: Long): Long = {
      var _instant: Long = instant
      if (_instant >= iCutover) {
        _instant = iGregorianField.add(_instant, value)
        if (_instant < iCutover) {
          if (_instant + iGapDuration < iCutover) {
            if (iConvertByWeekyear) {
              val wyear = iGregorianChronology.weekyear().get(_instant)
              if (wyear <= 0) {
                _instant = iGregorianChronology.weekyear().add(_instant, -1)
              }
            } else {
              val year = iGregorianChronology.year().get(_instant)
              if (year <= 0) {
                _instant = iGregorianChronology.year().add(_instant, -1)
              }
            }
            _instant = gregorianToJulian(_instant)
          }
        }
      } else {
        _instant = iJulianField.add(_instant, value)
        if (_instant >= iCutover) {
          if (_instant - iGapDuration >= iCutover) {
            _instant = julianToGregorian(_instant)
          }
        }
      }
      _instant
    }

    override def getDifference(minuendInstant: Long,
                               subtrahendInstant: Long): Int = {
      var _minuendInstant: Long = minuendInstant
      if (_minuendInstant >= iCutover) {
        if (subtrahendInstant >= iCutover) {
          return iGregorianField
            .getDifference(_minuendInstant, subtrahendInstant)
        }
        _minuendInstant = gregorianToJulian(_minuendInstant)
        iJulianField.getDifference(_minuendInstant, subtrahendInstant)
      } else {
        if (subtrahendInstant < iCutover) {
          return iJulianField.getDifference(_minuendInstant, subtrahendInstant)
        }
        _minuendInstant = julianToGregorian(_minuendInstant)
        iGregorianField.getDifference(_minuendInstant, subtrahendInstant)
      }
    }

    override def getDifferenceAsLong(minuendInstant: Long,
                                     subtrahendInstant: Long): Long = {
      var _minuendInstant: Long = minuendInstant
      if (_minuendInstant >= iCutover) {
        if (subtrahendInstant >= iCutover) {
          return iGregorianField
            .getDifferenceAsLong(_minuendInstant, subtrahendInstant)
        }
        _minuendInstant = gregorianToJulian(_minuendInstant)
        iJulianField.getDifferenceAsLong(_minuendInstant, subtrahendInstant)
      } else {
        if (subtrahendInstant < iCutover) {
          return iJulianField
            .getDifferenceAsLong(_minuendInstant, subtrahendInstant)
        }
        _minuendInstant = julianToGregorian(_minuendInstant)
        iGregorianField.getDifferenceAsLong(_minuendInstant, subtrahendInstant)
      }
    }

    override def getMinimumValue(instant: Long): Int = {
      if (instant >= iCutover) {
        iGregorianField.getMinimumValue(instant)
      } else {
        iJulianField.getMinimumValue(instant)
      }
    }

    override def getMaximumValue(instant: Long): Int = {
      if (instant >= iCutover) {
        iGregorianField.getMaximumValue(instant)
      } else {
        iJulianField.getMaximumValue(instant)
      }
    }
  }
}
