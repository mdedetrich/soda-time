package org.joda.time.chrono

import java.io.ObjectInputStream

import org.joda.time.Chronology
import org.joda.time.DateTimeField
import org.joda.time.DateTimeZone
import org.joda.time.DurationField
import org.joda.time.chrono.AssembledChronology.Fields

object AssembledChronology {

  object Fields {

    private def isSupported(field: DurationField): Boolean = {
      if (field == null) false else field.isSupported
    }

    private def isSupported(field: DateTimeField): Boolean = {
      if (field == null) false else field.isSupported
    }
  }

  class Fields() {

    var millis: DurationField = null
    var seconds: DurationField = null
    var minutes: DurationField = null
    var hours: DurationField = null
    var halfdays: DurationField = null
    var days: DurationField = null
    var weeks: DurationField = null
    var weekyears: DurationField = null
    var months: DurationField = null
    var years: DurationField = null
    var centuries: DurationField = null
    var eras: DurationField = null
    var millisOfSecond: DateTimeField = null
    var millisOfDay: DateTimeField = null
    var secondOfMinute: DateTimeField = null
    var secondOfDay: DateTimeField = null
    var minuteOfHour: DateTimeField = null
    var minuteOfDay: DateTimeField = null
    var hourOfDay: DateTimeField = null
    var clockhourOfDay: DateTimeField = null
    var hourOfHalfday: DateTimeField = null
    var clockhourOfHalfday: DateTimeField = null
    var halfdayOfDay: DateTimeField = null
    var dayOfWeek: DateTimeField = null
    var dayOfMonth: DateTimeField = null
    var dayOfYear: DateTimeField = null
    var weekOfWeekyear: DateTimeField = null
    var weekyear: DateTimeField = null
    var weekyearOfCentury: DateTimeField = null
    var monthOfYear: DateTimeField = null
    var year: DateTimeField = null
    var yearOfEra: DateTimeField = null
    var yearOfCentury: DateTimeField = null
    var centuryOfEra: DateTimeField = null
    var era: DateTimeField = null

    def copyFieldsFrom(chrono: Chronology) {
      {
        var f: DurationField = null

        if (Fields.isSupported({ f = chrono.millis(); f })) {
          millis = f
        }
        if (Fields.isSupported({ f = chrono.seconds(); f })) {
          seconds = f
        }
        if (Fields.isSupported({ f = chrono.minutes(); f })) {
          minutes = f
        }
        if (Fields.isSupported({ f = chrono.hours(); f })) {
          hours = f
        }
        if (Fields.isSupported({ f = chrono.halfdays(); f })) {
          halfdays = f
        }
        if (Fields.isSupported({ f = chrono.days(); f })) {
          days = f
        }
        if (Fields.isSupported({ f = chrono.weeks(); f })) {
          weeks = f
        }
        if (Fields.isSupported({ f = chrono.weekyears(); f })) {
          weekyears = f
        }
        if (Fields.isSupported({ f = chrono.months(); f })) {
          months = f
        }
        if (Fields.isSupported({ f = chrono.years(); f })) {
          years = f
        }
        if (Fields.isSupported({ f = chrono.centuries(); f })) {
          centuries = f
        }
        if (Fields.isSupported({ f = chrono.eras(); f })) {
          eras = f
        }
      }
      {
        var f: DateTimeField = null
        if (Fields.isSupported({ f = chrono.millisOfSecond(); f })) {
          millisOfSecond = f
        }
        if (Fields.isSupported({ f = chrono.millisOfDay(); f })) {
          millisOfDay = f
        }
        if (Fields.isSupported({ f = chrono.secondOfMinute(); f })) {
          secondOfMinute = f
        }
        if (Fields.isSupported({ f = chrono.secondOfDay(); f })) {
          secondOfDay = f
        }
        if (Fields.isSupported({ f = chrono.minuteOfHour(); f })) {
          minuteOfHour = f
        }
        if (Fields.isSupported({ f = chrono.minuteOfDay(); f })) {
          minuteOfDay = f
        }
        if (Fields.isSupported({ f = chrono.hourOfDay(); f })) {
          hourOfDay = f
        }
        if (Fields.isSupported({ f = chrono.clockhourOfDay(); f })) {
          clockhourOfDay = f
        }
        if (Fields.isSupported({ f = chrono.hourOfHalfday(); f })) {
          hourOfHalfday = f
        }
        if (Fields.isSupported({ f = chrono.clockhourOfHalfday(); f })) {
          clockhourOfHalfday = f
        }
        if (Fields.isSupported({ f = chrono.halfdayOfDay(); f })) {
          halfdayOfDay = f
        }
        if (Fields.isSupported({ f = chrono.dayOfWeek(); f })) {
          dayOfWeek = f
        }
        if (Fields.isSupported({ f = chrono.dayOfMonth(); f })) {
          dayOfMonth = f
        }
        if (Fields.isSupported({ f = chrono.dayOfYear(); f })) {
          dayOfYear = f
        }
        if (Fields.isSupported({ f = chrono.weekOfWeekyear(); f })) {
          weekOfWeekyear = f
        }
        if (Fields.isSupported({ f = chrono.weekyear(); f })) {
          weekyear = f
        }
        if (Fields.isSupported({ f = chrono.weekyearOfCentury(); f })) {
          weekyearOfCentury = f
        }
        if (Fields.isSupported({ f = chrono.monthOfYear(); f })) {
          monthOfYear = f
        }
        if (Fields.isSupported({ f = chrono.year(); f })) {
          year = f
        }
        if (Fields.isSupported({ f = chrono.yearOfEra(); f })) {
          yearOfEra = f
        }
        if (Fields.isSupported({ f = chrono.yearOfCentury(); f })) {
          yearOfCentury = f
        }
        if (Fields.isSupported({ f = chrono.centuryOfEra(); f })) {
          centuryOfEra = f
        }
        if (Fields.isSupported({ f = chrono.era(); f })) {
          era = f
        }
      }
    }
  }
}

@SerialVersionUID(-6728465968995518215L)
abstract class AssembledChronology extends BaseChronology {

  private var iBase: Chronology = null
  private var iParam: AnyRef = null

  @transient private var iMillis: DurationField = null
  @transient private var iSeconds: DurationField = null
  @transient private var iMinutes: DurationField = null
  @transient private var iHours: DurationField = null
  @transient private var iHalfdays: DurationField = null
  @transient private var iDays: DurationField = null
  @transient private var iWeeks: DurationField = null
  @transient private var iWeekyears: DurationField = null
  @transient private var iMonths: DurationField = null
  @transient private var iYears: DurationField = null
  @transient private var iCenturies: DurationField = null
  @transient private var iEras: DurationField = null
  @transient private var iMillisOfSecond: DateTimeField = null
  @transient private var iMillisOfDay: DateTimeField = null
  @transient private var iSecondOfMinute: DateTimeField = null
  @transient private var iSecondOfDay: DateTimeField = null
  @transient private var iMinuteOfHour: DateTimeField = null
  @transient private var iMinuteOfDay: DateTimeField = null
  @transient private var iHourOfDay: DateTimeField = null
  @transient private var iClockhourOfDay: DateTimeField = null
  @transient private var iHourOfHalfday: DateTimeField = null
  @transient private var iClockhourOfHalfday: DateTimeField = null
  @transient private var iHalfdayOfDay: DateTimeField = null
  @transient private var iDayOfWeek: DateTimeField = null
  @transient private var iDayOfMonth: DateTimeField = null
  @transient private var iDayOfYear: DateTimeField = null
  @transient private var iWeekOfWeekyear: DateTimeField = null
  @transient private var iWeekyear: DateTimeField = null
  @transient private var iWeekyearOfCentury: DateTimeField = null
  @transient private var iMonthOfYear: DateTimeField = null
  @transient private var iYear: DateTimeField = null
  @transient private var iYearOfEra: DateTimeField = null
  @transient private var iYearOfCentury: DateTimeField = null
  @transient private var iCenturyOfEra: DateTimeField = null
  @transient private var iEra: DateTimeField = null
  @transient private var iBaseFlags: Int = _

  protected def this(base: Chronology, param: AnyRef) {
    this()
    iBase = base
    iParam = param
    setFields()
  }

  protected def auxConstructor(base: Chronology, param: AnyRef): Unit = {
    iBase = base
    iParam = param
    setFields()
  }

  def getZone(): DateTimeZone = {
    var base: Chronology = null
    if ({
      base = iBase; base
    } != null) {
      return base.getZone
    }
    null
  }

  override def getDateTimeMillis(year: Int,
                                 monthOfYear: Int,
                                 dayOfMonth: Int,
                                 millisOfDay: Int): Long = {
    var base: Chronology = null
    if ({
      base = iBase; base
    } != null && (iBaseFlags & 6) == 6) {
      return base.getDateTimeMillis(year, monthOfYear, dayOfMonth, millisOfDay)
    }
    super.getDateTimeMillis(year, monthOfYear, dayOfMonth, millisOfDay)
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
      base = iBase; base
    } != null && (iBaseFlags & 5) == 5) {
      return base.getDateTimeMillis(year,
                                    monthOfYear,
                                    dayOfMonth,
                                    hourOfDay,
                                    minuteOfHour,
                                    secondOfMinute,
                                    millisOfSecond)
    }
    super.getDateTimeMillis(year,
                            monthOfYear,
                            dayOfMonth,
                            hourOfDay,
                            minuteOfHour,
                            secondOfMinute,
                            millisOfSecond)
  }

  override def getDateTimeMillis(instant: Long,
                                 hourOfDay: Int,
                                 minuteOfHour: Int,
                                 secondOfMinute: Int,
                                 millisOfSecond: Int): Long = {
    var base: Chronology = null
    if ({
      base = iBase; base
    } != null && (iBaseFlags & 1) == 1) {
      return base.getDateTimeMillis(instant,
                                    hourOfDay,
                                    minuteOfHour,
                                    secondOfMinute,
                                    millisOfSecond)
    }
    super.getDateTimeMillis(instant,
                            hourOfDay,
                            minuteOfHour,
                            secondOfMinute,
                            millisOfSecond)
  }

  override def millis(): DurationField = iMillis

  override def millisOfSecond(): DateTimeField = iMillisOfSecond

  override def millisOfDay(): DateTimeField = iMillisOfDay

  override def seconds(): DurationField = iSeconds

  override def secondOfMinute(): DateTimeField = iSecondOfMinute

  override def secondOfDay(): DateTimeField = iSecondOfDay

  override def minutes(): DurationField = iMinutes

  override def minuteOfHour(): DateTimeField = iMinuteOfHour

  override def minuteOfDay(): DateTimeField = iMinuteOfDay

  override def hours(): DurationField = iHours

  override def hourOfDay(): DateTimeField = iHourOfDay

  override def clockhourOfDay(): DateTimeField = iClockhourOfDay

  override def halfdays(): DurationField = iHalfdays

  override def hourOfHalfday(): DateTimeField = iHourOfHalfday

  override def clockhourOfHalfday(): DateTimeField = iClockhourOfHalfday

  override def halfdayOfDay(): DateTimeField = iHalfdayOfDay

  override def days(): DurationField = iDays

  override def dayOfWeek(): DateTimeField = iDayOfWeek

  override def dayOfMonth(): DateTimeField = iDayOfMonth

  override def dayOfYear(): DateTimeField = iDayOfYear

  override def weeks(): DurationField = iWeeks

  override def weekOfWeekyear(): DateTimeField = iWeekOfWeekyear

  override def weekyears(): DurationField = iWeekyears

  override def weekyear(): DateTimeField = iWeekyear

  override def weekyearOfCentury(): DateTimeField = iWeekyearOfCentury

  override def months(): DurationField = iMonths

  override def monthOfYear(): DateTimeField = iMonthOfYear

  override def years(): DurationField = iYears

  override def year(): DateTimeField = iYear

  override def yearOfEra(): DateTimeField = iYearOfEra

  override def yearOfCentury(): DateTimeField = iYearOfCentury

  override def centuries(): DurationField = iCenturies

  override def centuryOfEra(): DateTimeField = iCenturyOfEra

  override def eras(): DurationField = iEras

  override def era(): DateTimeField = iEra

  protected def assemble(fields: Fields): Unit

  protected def getBase(): Chronology = iBase

  protected def getParam(): AnyRef = iParam

  private def setFields() {
    val fields = new Fields()
    if (iBase != null) {
      fields.copyFieldsFrom(iBase)
    }
    assemble(fields)

    {
      var f: DurationField = null
      iMillis = if ({ f = fields.millis; f } != null) f else super.millis()
      iSeconds = if ({ f = fields.seconds; f } != null) f else super.seconds()
      iMinutes = if ({ f = fields.minutes; f } != null) f else super.minutes()
      iHours = if ({ f = fields.hours; f } != null) f else super.hours()
      iHalfdays =
        if ({ f = fields.halfdays; f } != null) f else super.halfdays()
      iDays = if ({ f = fields.days; f } != null) f else super.days()
      iWeeks = if ({ f = fields.weeks; f } != null) f else super.weeks()
      iWeekyears =
        if ({ f = fields.weekyears; f } != null) f else super.weekyears()
      iMonths = if ({ f = fields.months; f } != null) f else super.months()
      iYears = if ({ f = fields.years; f } != null) f else super.years()
      iCenturies =
        if ({ f = fields.centuries; f } != null) f else super.centuries()
      iEras = if ({ f = fields.eras; f } != null) f else super.eras()
    }

    {
      var f: DateTimeField = null
      iMillisOfSecond = if ({ f = fields.millisOfSecond; f } != null) f
      else super.millisOfSecond()
      iMillisOfDay =
        if ({ f = fields.millisOfDay; f } != null) f else super.millisOfDay()
      iSecondOfMinute = if ({ f = fields.secondOfMinute; f } != null) f
      else super.secondOfMinute()
      iSecondOfDay =
        if ({ f = fields.secondOfDay; f } != null) f else super.secondOfDay()
      iMinuteOfHour =
        if ({ f = fields.minuteOfHour; f } != null) f else super.minuteOfHour()
      iMinuteOfDay =
        if ({ f = fields.minuteOfDay; f } != null) f else super.minuteOfDay()
      iHourOfDay =
        if ({ f = fields.hourOfDay; f } != null) f else super.hourOfDay()
      iClockhourOfDay = if ({ f = fields.clockhourOfDay; f } != null) f
      else super.clockhourOfDay()
      iHourOfHalfday = if ({ f = fields.hourOfHalfday; f } != null) f
      else super.hourOfHalfday()
      iClockhourOfHalfday = if ({ f = fields.clockhourOfHalfday; f } != null) f
      else super.clockhourOfHalfday()
      iHalfdayOfDay =
        if ({ f = fields.halfdayOfDay; f } != null) f else super.halfdayOfDay()
      iDayOfWeek =
        if ({ f = fields.dayOfWeek; f } != null) f else super.dayOfWeek()
      iDayOfMonth =
        if ({ f = fields.dayOfMonth; f } != null) f else super.dayOfMonth()
      iDayOfYear =
        if ({ f = fields.dayOfYear; f } != null) f else super.dayOfYear()
      iWeekOfWeekyear = if ({ f = fields.weekOfWeekyear; f } != null) f
      else super.weekOfWeekyear()
      iWeekyear =
        if ({ f = fields.weekyear; f } != null) f else super.weekyear()
      iWeekyearOfCentury = if ({ f = fields.weekyearOfCentury; f } != null) f
      else super.weekyearOfCentury()
      iMonthOfYear =
        if ({ f = fields.monthOfYear; f } != null) f else super.monthOfYear()
      iYear = if ({ f = fields.year; f } != null) f else super.year()
      iYearOfEra =
        if ({ f = fields.yearOfEra; f } != null) f else super.yearOfEra()
      iYearOfCentury = if ({ f = fields.yearOfCentury; f } != null) f
      else super.yearOfCentury()
      iCenturyOfEra =
        if ({ f = fields.centuryOfEra; f } != null) f else super.centuryOfEra()
      iEra = if ({ f = fields.era; f } != null) f else super.era()
      var flags: Int = 0
      flags =
        if (iBase == null) 0
        else
          (if (iHourOfDay == iBase.hourOfDay() && iMinuteOfHour == iBase
                 .minuteOfHour() &&
               iSecondOfMinute == iBase.secondOfMinute() &&
               iMillisOfSecond == iBase.millisOfSecond()) 1
           else 0) |
            (if (iMillisOfDay == iBase.millisOfDay()) 2 else 0) |
            (if (iYear == iBase.year() && iMonthOfYear == iBase
                   .monthOfYear() &&
                 iDayOfMonth == iBase.dayOfMonth()) 4
             else 0)
      iBaseFlags = flags
    }
  }

  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    setFields()
  }
}
