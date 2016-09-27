package org.joda.time.format

import java.util.Collection
import java.util.HashSet
import org.joda.time.DateTimeFieldType

object ISODateTimeFormat {

  def forFields(fields: Collection[DateTimeFieldType],
                extended: Boolean,
                strictISO: Boolean): DateTimeFormatter = {
    if (fields == null || fields.size == 0) {
      throw new IllegalArgumentException(
        "The fields must not be null or empty")
    }
    val workingFields = new HashSet[DateTimeFieldType](fields)
    val inputSize = workingFields.size
    var reducedPrec = false
    val bld = new DateTimeFormatterBuilder()
    if (workingFields.contains(DateTimeFieldType.monthOfYear())) {
      reducedPrec = dateByMonth(bld, workingFields, extended, strictISO)
    } else if (workingFields.contains(DateTimeFieldType.dayOfYear())) {
      reducedPrec = dateByOrdinal(bld, workingFields, extended, strictISO)
    } else if (workingFields.contains(DateTimeFieldType.weekOfWeekyear())) {
      reducedPrec = dateByWeek(bld, workingFields, extended, strictISO)
    } else if (workingFields.contains(DateTimeFieldType.dayOfMonth())) {
      reducedPrec = dateByMonth(bld, workingFields, extended, strictISO)
    } else if (workingFields.contains(DateTimeFieldType.dayOfWeek())) {
      reducedPrec = dateByWeek(bld, workingFields, extended, strictISO)
    } else if (workingFields.remove(DateTimeFieldType.year())) {
      bld.append(Constants.ye)
      reducedPrec = true
    } else if (workingFields.remove(DateTimeFieldType.weekyear())) {
      bld.append(Constants.we)
      reducedPrec = true
    }
    val datePresent = workingFields.size < inputSize
    time(bld, workingFields, extended, strictISO, reducedPrec, datePresent)
    if (bld.canBuildFormatter() == false) {
      throw new IllegalArgumentException(
        "No valid format for fields: " + fields)
    }
    try {
      fields.retainAll(workingFields)
    } catch {
      case ex: UnsupportedOperationException =>
    }
    bld.toFormatter()
  }

  private def dateByMonth(bld: DateTimeFormatterBuilder,
                          fields: Collection[DateTimeFieldType],
                          extended: Boolean,
                          strictISO: Boolean): Boolean = {
    var reducedPrec = false
    if (fields.remove(DateTimeFieldType.year())) {
      bld.append(Constants.ye)
      if (fields.remove(DateTimeFieldType.monthOfYear())) {
        if (fields.remove(DateTimeFieldType.dayOfMonth())) {
          appendSeparator(bld, extended)
          bld.appendMonthOfYear(2)
          appendSeparator(bld, extended)
          bld.appendDayOfMonth(2)
        } else {
          bld.appendLiteral('-')
          bld.appendMonthOfYear(2)
          reducedPrec = true
        }
      } else {
        if (fields.remove(DateTimeFieldType.dayOfMonth())) {
          checkNotStrictISO(fields, strictISO)
          bld.appendLiteral('-')
          bld.appendLiteral('-')
          bld.appendDayOfMonth(2)
        } else {
          reducedPrec = true
        }
      }
    } else if (fields.remove(DateTimeFieldType.monthOfYear())) {
      bld.appendLiteral('-')
      bld.appendLiteral('-')
      bld.appendMonthOfYear(2)
      if (fields.remove(DateTimeFieldType.dayOfMonth())) {
        appendSeparator(bld, extended)
        bld.appendDayOfMonth(2)
      } else {
        reducedPrec = true
      }
    } else if (fields.remove(DateTimeFieldType.dayOfMonth())) {
      bld.appendLiteral('-')
      bld.appendLiteral('-')
      bld.appendLiteral('-')
      bld.appendDayOfMonth(2)
    }
    reducedPrec
  }

  private def dateByOrdinal(bld: DateTimeFormatterBuilder,
                            fields: Collection[DateTimeFieldType],
                            extended: Boolean,
                            strictISO: Boolean): Boolean = {
    var reducedPrec = false
    if (fields.remove(DateTimeFieldType.year())) {
      bld.append(Constants.ye)
      if (fields.remove(DateTimeFieldType.dayOfYear())) {
        appendSeparator(bld, extended)
        bld.appendDayOfYear(3)
      } else {
        reducedPrec = true
      }
    } else if (fields.remove(DateTimeFieldType.dayOfYear())) {
      bld.appendLiteral('-')
      bld.appendDayOfYear(3)
    }
    reducedPrec
  }

  private def dateByWeek(bld: DateTimeFormatterBuilder,
                         fields: Collection[DateTimeFieldType],
                         extended: Boolean,
                         strictISO: Boolean): Boolean = {
    var reducedPrec = false
    if (fields.remove(DateTimeFieldType.weekyear())) {
      bld.append(Constants.we)
      if (fields.remove(DateTimeFieldType.weekOfWeekyear())) {
        appendSeparator(bld, extended)
        bld.appendLiteral('W')
        bld.appendWeekOfWeekyear(2)
        if (fields.remove(DateTimeFieldType.dayOfWeek())) {
          appendSeparator(bld, extended)
          bld.appendDayOfWeek(1)
        } else {
          reducedPrec = true
        }
      } else {
        if (fields.remove(DateTimeFieldType.dayOfWeek())) {
          checkNotStrictISO(fields, strictISO)
          appendSeparator(bld, extended)
          bld.appendLiteral('W')
          bld.appendLiteral('-')
          bld.appendDayOfWeek(1)
        } else {
          reducedPrec = true
        }
      }
    } else if (fields.remove(DateTimeFieldType.weekOfWeekyear())) {
      bld.appendLiteral('-')
      bld.appendLiteral('W')
      bld.appendWeekOfWeekyear(2)
      if (fields.remove(DateTimeFieldType.dayOfWeek())) {
        appendSeparator(bld, extended)
        bld.appendDayOfWeek(1)
      } else {
        reducedPrec = true
      }
    } else if (fields.remove(DateTimeFieldType.dayOfWeek())) {
      bld.appendLiteral('-')
      bld.appendLiteral('W')
      bld.appendLiteral('-')
      bld.appendDayOfWeek(1)
    }
    reducedPrec
  }

  private def time(bld: DateTimeFormatterBuilder,
                   fields: Collection[DateTimeFieldType],
                   extended: Boolean,
                   strictISO: Boolean,
                   reducedPrec: Boolean,
                   datePresent: Boolean) {
    val hour = fields.remove(DateTimeFieldType.hourOfDay())
    val minute = fields.remove(DateTimeFieldType.minuteOfHour())
    val second = fields.remove(DateTimeFieldType.secondOfMinute())
    val milli = fields.remove(DateTimeFieldType.millisOfSecond())
    if (!hour && !minute && !second && !milli) {
      return
    }
    if (hour || minute || second || milli) {
      if (strictISO && reducedPrec) {
        throw new IllegalArgumentException(
          "No valid ISO8601 format for fields because Date was reduced precision: " +
            fields)
      }
      if (datePresent) {
        bld.appendLiteral('T')
      }
    }
    if (hour && minute && second || (hour && !second && !milli)) {} else {
      if (strictISO && datePresent) {
        throw new IllegalArgumentException(
          "No valid ISO8601 format for fields because Time was truncated: " +
            fields)
      }
      if (!hour && (minute && second || (minute && !milli) || second)) {} else {
        if (strictISO) {
          throw new IllegalArgumentException(
            "No valid ISO8601 format for fields: " + fields)
        }
      }
    }
    if (hour) {
      bld.appendHourOfDay(2)
    } else if (minute || second || milli) {
      bld.appendLiteral('-')
    }
    if (extended && hour && minute) {
      bld.appendLiteral(':')
    }
    if (minute) {
      bld.appendMinuteOfHour(2)
    } else if (second || milli) {
      bld.appendLiteral('-')
    }
    if (extended && minute && second) {
      bld.appendLiteral(':')
    }
    if (second) {
      bld.appendSecondOfMinute(2)
    } else if (milli) {
      bld.appendLiteral('-')
    }
    if (milli) {
      bld.appendLiteral('.')
      bld.appendMillisOfSecond(3)
    }
  }

  private def checkNotStrictISO(fields: Collection[DateTimeFieldType],
                                strictISO: Boolean) {
    if (strictISO) {
      throw new IllegalArgumentException(
        "No valid ISO8601 format for fields: " + fields)
    }
  }

  private def appendSeparator(bld: DateTimeFormatterBuilder,
                              extended: Boolean) {
    if (extended) {
      bld.appendLiteral('-')
    }
  }

  def dateParser(): DateTimeFormatter = Constants.dp

  def localDateParser(): DateTimeFormatter = Constants.ldp

  def dateElementParser(): DateTimeFormatter = Constants.dpe

  def timeParser(): DateTimeFormatter = Constants.tp

  def localTimeParser(): DateTimeFormatter = Constants.ltp

  def timeElementParser(): DateTimeFormatter = Constants.tpe

  def dateTimeParser(): DateTimeFormatter = Constants.dtp

  def dateOptionalTimeParser(): DateTimeFormatter = Constants.dotp

  def localDateOptionalTimeParser(): DateTimeFormatter = Constants.ldotp

  def date(): DateTimeFormatter = yearMonthDay()

  def time(): DateTimeFormatter = Constants.t

  def timeNoMillis(): DateTimeFormatter = Constants.tx

  def tTime(): DateTimeFormatter = Constants.tt

  def tTimeNoMillis(): DateTimeFormatter = Constants.ttx

  def dateTime(): DateTimeFormatter = Constants.dt

  def dateTimeNoMillis(): DateTimeFormatter = Constants.dtx

  def ordinalDate(): DateTimeFormatter = Constants.od

  def ordinalDateTime(): DateTimeFormatter = Constants.odt

  def ordinalDateTimeNoMillis(): DateTimeFormatter = Constants.odtx

  def weekDate(): DateTimeFormatter = Constants.wwd

  def weekDateTime(): DateTimeFormatter = Constants.wdt

  def weekDateTimeNoMillis(): DateTimeFormatter = Constants.wdtx

  def basicDate(): DateTimeFormatter = Constants.bd

  def basicTime(): DateTimeFormatter = Constants.bt

  def basicTimeNoMillis(): DateTimeFormatter = Constants.btx

  def basicTTime(): DateTimeFormatter = Constants.btt

  def basicTTimeNoMillis(): DateTimeFormatter = Constants.bttx

  def basicDateTime(): DateTimeFormatter = Constants.bdt

  def basicDateTimeNoMillis(): DateTimeFormatter = Constants.bdtx

  def basicOrdinalDate(): DateTimeFormatter = Constants.bod

  def basicOrdinalDateTime(): DateTimeFormatter = Constants.bodt

  def basicOrdinalDateTimeNoMillis(): DateTimeFormatter = Constants.bodtx

  def basicWeekDate(): DateTimeFormatter = Constants.bwd

  def basicWeekDateTime(): DateTimeFormatter = Constants.bwdt

  def basicWeekDateTimeNoMillis(): DateTimeFormatter = Constants.bwdtx

  def year(): DateTimeFormatter = Constants.ye

  def yearMonth(): DateTimeFormatter = Constants.ym

  def yearMonthDay(): DateTimeFormatter = Constants.ymd

  def weekyear(): DateTimeFormatter = Constants.we

  def weekyearWeek(): DateTimeFormatter = Constants.ww

  def weekyearWeekDay(): DateTimeFormatter = Constants.wwd

  def hour(): DateTimeFormatter = Constants.hde

  def hourMinute(): DateTimeFormatter = Constants.hm

  def hourMinuteSecond(): DateTimeFormatter = Constants.hms

  def hourMinuteSecondMillis(): DateTimeFormatter = Constants.hmsl

  def hourMinuteSecondFraction(): DateTimeFormatter = Constants.hmsf

  def dateHour(): DateTimeFormatter = Constants.dh

  def dateHourMinute(): DateTimeFormatter = Constants.dhm

  def dateHourMinuteSecond(): DateTimeFormatter = Constants.dhms

  def dateHourMinuteSecondMillis(): DateTimeFormatter = Constants.dhmsl

  def dateHourMinuteSecondFraction(): DateTimeFormatter = Constants.dhmsf

  private object Constants {

    val ye: DateTimeFormatter = yearElement()
    val mye: DateTimeFormatter = monthElement()
    val dme: DateTimeFormatter = dayOfMonthElement()
    val we: DateTimeFormatter = weekyearElement()
    val wwe: DateTimeFormatter = weekElement()
    val dwe: DateTimeFormatter = dayOfWeekElement()
    val dye: DateTimeFormatter = dayOfYearElement()
    val hde: DateTimeFormatter = hourElement()
    val mhe: DateTimeFormatter = minuteElement()
    val sme: DateTimeFormatter = secondElement()
    val fse: DateTimeFormatter = fractionElement()
    val ze: DateTimeFormatter = offsetElement()
    val lte: DateTimeFormatter = literalTElement()
    val ym: DateTimeFormatter = yearMonth()
    val ymd: DateTimeFormatter = yearMonthDay()
    val ww: DateTimeFormatter = weekyearWeek()
    val wwd: DateTimeFormatter = weekyearWeekDay()
    val hm: DateTimeFormatter = hourMinute()
    val hms: DateTimeFormatter = hourMinuteSecond()
    val hmsl: DateTimeFormatter = hourMinuteSecondMillis()
    val hmsf: DateTimeFormatter = hourMinuteSecondFraction()
    val dh: DateTimeFormatter = dateHour()
    val dhm: DateTimeFormatter = dateHourMinute()
    val dhms: DateTimeFormatter = dateHourMinuteSecond()
    val dhmsl: DateTimeFormatter = dateHourMinuteSecondMillis()
    val dhmsf: DateTimeFormatter = dateHourMinuteSecondFraction()
    val t: DateTimeFormatter = time()
    val tx: DateTimeFormatter = timeNoMillis()
    val tt: DateTimeFormatter = tTime()
    val ttx: DateTimeFormatter = tTimeNoMillis()
    val dt: DateTimeFormatter = dateTime()
    val dtx: DateTimeFormatter = dateTimeNoMillis()
    val wdt: DateTimeFormatter = weekDateTime()
    val wdtx: DateTimeFormatter = weekDateTimeNoMillis()
    val od: DateTimeFormatter = ordinalDate()
    val odt: DateTimeFormatter = ordinalDateTime()
    val odtx: DateTimeFormatter = ordinalDateTimeNoMillis()
    val bd: DateTimeFormatter = basicDate()
    val bt: DateTimeFormatter = basicTime()
    val btx: DateTimeFormatter = basicTimeNoMillis()
    val btt: DateTimeFormatter = basicTTime()
    val bttx: DateTimeFormatter = basicTTimeNoMillis()
    val bdt: DateTimeFormatter = basicDateTime()
    val bdtx: DateTimeFormatter = basicDateTimeNoMillis()
    val bod: DateTimeFormatter = basicOrdinalDate()
    val bodt: DateTimeFormatter = basicOrdinalDateTime()
    val bodtx: DateTimeFormatter = basicOrdinalDateTimeNoMillis()
    val bwd: DateTimeFormatter = basicWeekDate()
    val bwdt: DateTimeFormatter = basicWeekDateTime()
    val bwdtx: DateTimeFormatter = basicWeekDateTimeNoMillis()
    val dpe: DateTimeFormatter = dateElementParser()
    val tpe: DateTimeFormatter = timeElementParser()
    val dp: DateTimeFormatter = dateParser()
    val ldp: DateTimeFormatter = localDateParser()
    val tp: DateTimeFormatter = timeParser()
    val ltp: DateTimeFormatter = localTimeParser()
    val dtp: DateTimeFormatter = dateTimeParser()
    val dotp: DateTimeFormatter = dateOptionalTimeParser()
    val ldotp: DateTimeFormatter = localDateOptionalTimeParser()

    private def dateParser(): DateTimeFormatter = {
      if (dp == null) {
        val tOffset = new DateTimeFormatterBuilder()
          .appendLiteral('T')
          .append(offsetElement())
          .toParser()
        return new DateTimeFormatterBuilder()
          .append(dateElementParser())
          .appendOptional(tOffset)
          .toFormatter()
      }
      dp
    }

    private def localDateParser(): DateTimeFormatter = {
      if (ldp == null) {
        return dateElementParser().withZoneUTC()
      }
      ldp
    }

    private def dateElementParser(): DateTimeFormatter = {
      if (dpe == null) {
        return new DateTimeFormatterBuilder()
          .append(null,
                  Array(new DateTimeFormatterBuilder()
                          .append(yearElement())
                          .appendOptional(
                            new DateTimeFormatterBuilder()
                              .append(monthElement())
                              .appendOptional(dayOfMonthElement().getParser)
                              .toParser())
                          .toParser(),
                        new DateTimeFormatterBuilder()
                          .append(weekyearElement())
                          .append(weekElement())
                          .appendOptional(dayOfWeekElement().getParser)
                          .toParser(),
                        new DateTimeFormatterBuilder()
                          .append(yearElement())
                          .append(dayOfYearElement())
                          .toParser()))
          .toFormatter()
      }
      dpe
    }

    private def timeParser(): DateTimeFormatter = {
      if (tp == null) {
        return new DateTimeFormatterBuilder()
          .appendOptional(literalTElement().getParser)
          .append(timeElementParser())
          .appendOptional(offsetElement().getParser)
          .toFormatter()
      }
      tp
    }

    private def localTimeParser(): DateTimeFormatter = {
      if (ltp == null) {
        return new DateTimeFormatterBuilder()
          .appendOptional(literalTElement().getParser)
          .append(timeElementParser())
          .toFormatter()
          .withZoneUTC()
      }
      ltp
    }

    private def timeElementParser(): DateTimeFormatter = {
      if (tpe == null) {
        val decimalPoint = new DateTimeFormatterBuilder()
          .append(
            null,
            Array(
              new DateTimeFormatterBuilder().appendLiteral('.').toParser(),
              new DateTimeFormatterBuilder().appendLiteral(',').toParser()))
          .toParser()
        return new DateTimeFormatterBuilder()
          .append(hourElement())
          .append(null,
                  Array(new DateTimeFormatterBuilder()
                          .append(minuteElement())
                          .append(null,
                                  Array(new DateTimeFormatterBuilder()
                                          .append(secondElement())
                                          .appendOptional(
                                            new DateTimeFormatterBuilder()
                                              .append(decimalPoint)
                                              .appendFractionOfSecond(1, 9)
                                              .toParser())
                                          .toParser(),
                                        new DateTimeFormatterBuilder()
                                          .append(decimalPoint)
                                          .appendFractionOfMinute(1, 9)
                                          .toParser(),
                                        null))
                          .toParser(),
                        new DateTimeFormatterBuilder()
                          .append(decimalPoint)
                          .appendFractionOfHour(1, 9)
                          .toParser(),
                        null))
          .toFormatter()
      }
      tpe
    }

    private def dateTimeParser(): DateTimeFormatter = {
      if (dtp == null) {
        val time = new DateTimeFormatterBuilder()
          .appendLiteral('T')
          .append(timeElementParser())
          .appendOptional(offsetElement().getParser)
          .toParser()
        return new DateTimeFormatterBuilder()
          .append(null, Array(time, dateOptionalTimeParser().getParser))
          .toFormatter()
      }
      dtp
    }

    private def dateOptionalTimeParser(): DateTimeFormatter = {
      if (dotp == null) {
        val timeOrOffset = new DateTimeFormatterBuilder()
          .appendLiteral('T')
          .appendOptional(timeElementParser().getParser)
          .appendOptional(offsetElement().getParser)
          .toParser()
        return new DateTimeFormatterBuilder()
          .append(dateElementParser())
          .appendOptional(timeOrOffset)
          .toFormatter()
      }
      dotp
    }

    private def localDateOptionalTimeParser(): DateTimeFormatter = {
      if (ldotp == null) {
        val time = new DateTimeFormatterBuilder()
          .appendLiteral('T')
          .append(timeElementParser())
          .toParser()
        return new DateTimeFormatterBuilder()
          .append(dateElementParser())
          .appendOptional(time)
          .toFormatter()
          .withZoneUTC()
      }
      ldotp
    }

    private def time(): DateTimeFormatter = {
      if (t == null) {
        return new DateTimeFormatterBuilder()
          .append(hourMinuteSecondFraction())
          .append(offsetElement())
          .toFormatter()
      }
      t
    }

    private def timeNoMillis(): DateTimeFormatter = {
      if (tx == null) {
        return new DateTimeFormatterBuilder()
          .append(hourMinuteSecond())
          .append(offsetElement())
          .toFormatter()
      }
      tx
    }

    private def tTime(): DateTimeFormatter = {
      if (tt == null) {
        return new DateTimeFormatterBuilder()
          .append(literalTElement())
          .append(time())
          .toFormatter()
      }
      tt
    }

    private def tTimeNoMillis(): DateTimeFormatter = {
      if (ttx == null) {
        return new DateTimeFormatterBuilder()
          .append(literalTElement())
          .append(timeNoMillis())
          .toFormatter()
      }
      ttx
    }

    private def dateTime(): DateTimeFormatter = {
      if (dt == null) {
        return new DateTimeFormatterBuilder()
          .append(date())
          .append(tTime())
          .toFormatter()
      }
      dt
    }

    private def dateTimeNoMillis(): DateTimeFormatter = {
      if (dtx == null) {
        return new DateTimeFormatterBuilder()
          .append(date())
          .append(tTimeNoMillis())
          .toFormatter()
      }
      dtx
    }

    private def ordinalDate(): DateTimeFormatter = {
      if (od == null) {
        return new DateTimeFormatterBuilder()
          .append(yearElement())
          .append(dayOfYearElement())
          .toFormatter()
      }
      od
    }

    private def ordinalDateTime(): DateTimeFormatter = {
      if (odt == null) {
        return new DateTimeFormatterBuilder()
          .append(ordinalDate())
          .append(tTime())
          .toFormatter()
      }
      odt
    }

    private def ordinalDateTimeNoMillis(): DateTimeFormatter = {
      if (odtx == null) {
        return new DateTimeFormatterBuilder()
          .append(ordinalDate())
          .append(tTimeNoMillis())
          .toFormatter()
      }
      odtx
    }

    private def weekDateTime(): DateTimeFormatter = {
      if (wdt == null) {
        return new DateTimeFormatterBuilder()
          .append(weekDate())
          .append(tTime())
          .toFormatter()
      }
      wdt
    }

    private def weekDateTimeNoMillis(): DateTimeFormatter = {
      if (wdtx == null) {
        return new DateTimeFormatterBuilder()
          .append(weekDate())
          .append(tTimeNoMillis())
          .toFormatter()
      }
      wdtx
    }

    private def basicDate(): DateTimeFormatter = {
      if (bd == null) {
        return new DateTimeFormatterBuilder()
          .appendYear(4, 4)
          .appendFixedDecimal(DateTimeFieldType.monthOfYear(), 2)
          .appendFixedDecimal(DateTimeFieldType.dayOfMonth(), 2)
          .toFormatter()
      }
      bd
    }

    private def basicTime(): DateTimeFormatter = {
      if (bt == null) {
        return new DateTimeFormatterBuilder()
          .appendFixedDecimal(DateTimeFieldType.hourOfDay(), 2)
          .appendFixedDecimal(DateTimeFieldType.minuteOfHour(), 2)
          .appendFixedDecimal(DateTimeFieldType.secondOfMinute(), 2)
          .appendLiteral('.')
          .appendFractionOfSecond(3, 9)
          .appendTimeZoneOffset("Z", showSeparators = false, 2, 2)
          .toFormatter()
      }
      bt
    }

    private def basicTimeNoMillis(): DateTimeFormatter = {
      if (btx == null) {
        return new DateTimeFormatterBuilder()
          .appendFixedDecimal(DateTimeFieldType.hourOfDay(), 2)
          .appendFixedDecimal(DateTimeFieldType.minuteOfHour(), 2)
          .appendFixedDecimal(DateTimeFieldType.secondOfMinute(), 2)
          .appendTimeZoneOffset("Z", showSeparators = false, 2, 2)
          .toFormatter()
      }
      btx
    }

    private def basicTTime(): DateTimeFormatter = {
      if (btt == null) {
        return new DateTimeFormatterBuilder()
          .append(literalTElement())
          .append(basicTime())
          .toFormatter()
      }
      btt
    }

    private def basicTTimeNoMillis(): DateTimeFormatter = {
      if (bttx == null) {
        return new DateTimeFormatterBuilder()
          .append(literalTElement())
          .append(basicTimeNoMillis())
          .toFormatter()
      }
      bttx
    }

    private def basicDateTime(): DateTimeFormatter = {
      if (bdt == null) {
        return new DateTimeFormatterBuilder()
          .append(basicDate())
          .append(basicTTime())
          .toFormatter()
      }
      bdt
    }

    private def basicDateTimeNoMillis(): DateTimeFormatter = {
      if (bdtx == null) {
        return new DateTimeFormatterBuilder()
          .append(basicDate())
          .append(basicTTimeNoMillis())
          .toFormatter()
      }
      bdtx
    }

    private def basicOrdinalDate(): DateTimeFormatter = {
      if (bod == null) {
        return new DateTimeFormatterBuilder()
          .appendYear(4, 4)
          .appendFixedDecimal(DateTimeFieldType.dayOfYear(), 3)
          .toFormatter()
      }
      bod
    }

    private def basicOrdinalDateTime(): DateTimeFormatter = {
      if (bodt == null) {
        return new DateTimeFormatterBuilder()
          .append(basicOrdinalDate())
          .append(basicTTime())
          .toFormatter()
      }
      bodt
    }

    private def basicOrdinalDateTimeNoMillis(): DateTimeFormatter = {
      if (bodtx == null) {
        return new DateTimeFormatterBuilder()
          .append(basicOrdinalDate())
          .append(basicTTimeNoMillis())
          .toFormatter()
      }
      bodtx
    }

    private def basicWeekDate(): DateTimeFormatter = {
      if (bwd == null) {
        return new DateTimeFormatterBuilder()
          .appendWeekyear(4, 4)
          .appendLiteral('W')
          .appendFixedDecimal(DateTimeFieldType.weekOfWeekyear(), 2)
          .appendFixedDecimal(DateTimeFieldType.dayOfWeek(), 1)
          .toFormatter()
      }
      bwd
    }

    private def basicWeekDateTime(): DateTimeFormatter = {
      if (bwdt == null) {
        return new DateTimeFormatterBuilder()
          .append(basicWeekDate())
          .append(basicTTime())
          .toFormatter()
      }
      bwdt
    }

    private def basicWeekDateTimeNoMillis(): DateTimeFormatter = {
      if (bwdtx == null) {
        return new DateTimeFormatterBuilder()
          .append(basicWeekDate())
          .append(basicTTimeNoMillis())
          .toFormatter()
      }
      bwdtx
    }

    private def yearMonth(): DateTimeFormatter = {
      if (ym == null) {
        return new DateTimeFormatterBuilder()
          .append(yearElement())
          .append(monthElement())
          .toFormatter()
      }
      ym
    }

    private def yearMonthDay(): DateTimeFormatter = {
      if (ymd == null) {
        return new DateTimeFormatterBuilder()
          .append(yearElement())
          .append(monthElement())
          .append(dayOfMonthElement())
          .toFormatter()
      }
      ymd
    }

    private def weekyearWeek(): DateTimeFormatter = {
      if (ww == null) {
        return new DateTimeFormatterBuilder()
          .append(weekyearElement())
          .append(weekElement())
          .toFormatter()
      }
      ww
    }

    private def weekyearWeekDay(): DateTimeFormatter = {
      if (wwd == null) {
        return new DateTimeFormatterBuilder()
          .append(weekyearElement())
          .append(weekElement())
          .append(dayOfWeekElement())
          .toFormatter()
      }
      wwd
    }

    private def hourMinute(): DateTimeFormatter = {
      if (hm == null) {
        return new DateTimeFormatterBuilder()
          .append(hourElement())
          .append(minuteElement())
          .toFormatter()
      }
      hm
    }

    private def hourMinuteSecond(): DateTimeFormatter = {
      if (hms == null) {
        return new DateTimeFormatterBuilder()
          .append(hourElement())
          .append(minuteElement())
          .append(secondElement())
          .toFormatter()
      }
      hms
    }

    private def hourMinuteSecondMillis(): DateTimeFormatter = {
      if (hmsl == null) {
        return new DateTimeFormatterBuilder()
          .append(hourElement())
          .append(minuteElement())
          .append(secondElement())
          .appendLiteral('.')
          .appendFractionOfSecond(3, 3)
          .toFormatter()
      }
      hmsl
    }

    private def hourMinuteSecondFraction(): DateTimeFormatter = {
      if (hmsf == null) {
        return new DateTimeFormatterBuilder()
          .append(hourElement())
          .append(minuteElement())
          .append(secondElement())
          .append(fractionElement())
          .toFormatter()
      }
      hmsf
    }

    private def dateHour(): DateTimeFormatter = {
      if (dh == null) {
        return new DateTimeFormatterBuilder()
          .append(date())
          .append(literalTElement())
          .append(hour())
          .toFormatter()
      }
      dh
    }

    private def dateHourMinute(): DateTimeFormatter = {
      if (dhm == null) {
        return new DateTimeFormatterBuilder()
          .append(date())
          .append(literalTElement())
          .append(hourMinute())
          .toFormatter()
      }
      dhm
    }

    private def dateHourMinuteSecond(): DateTimeFormatter = {
      if (dhms == null) {
        return new DateTimeFormatterBuilder()
          .append(date())
          .append(literalTElement())
          .append(hourMinuteSecond())
          .toFormatter()
      }
      dhms
    }

    private def dateHourMinuteSecondMillis(): DateTimeFormatter = {
      if (dhmsl == null) {
        return new DateTimeFormatterBuilder()
          .append(date())
          .append(literalTElement())
          .append(hourMinuteSecondMillis())
          .toFormatter()
      }
      dhmsl
    }

    private def dateHourMinuteSecondFraction(): DateTimeFormatter = {
      if (dhmsf == null) {
        return new DateTimeFormatterBuilder()
          .append(date())
          .append(literalTElement())
          .append(hourMinuteSecondFraction())
          .toFormatter()
      }
      dhmsf
    }

    private def yearElement(): DateTimeFormatter = {
      if (ye == null) {
        return new DateTimeFormatterBuilder().appendYear(4, 9).toFormatter()
      }
      ye
    }

    private def monthElement(): DateTimeFormatter = {
      if (mye == null) {
        return new DateTimeFormatterBuilder()
          .appendLiteral('-')
          .appendMonthOfYear(2)
          .toFormatter()
      }
      mye
    }

    private def dayOfMonthElement(): DateTimeFormatter = {
      if (dme == null) {
        return new DateTimeFormatterBuilder()
          .appendLiteral('-')
          .appendDayOfMonth(2)
          .toFormatter()
      }
      dme
    }

    private def weekyearElement(): DateTimeFormatter = {
      if (we == null) {
        return new DateTimeFormatterBuilder()
          .appendWeekyear(4, 9)
          .toFormatter()
      }
      we
    }

    private def weekElement(): DateTimeFormatter = {
      if (wwe == null) {
        return new DateTimeFormatterBuilder()
          .appendLiteral("-W")
          .appendWeekOfWeekyear(2)
          .toFormatter()
      }
      wwe
    }

    private def dayOfWeekElement(): DateTimeFormatter = {
      if (dwe == null) {
        return new DateTimeFormatterBuilder()
          .appendLiteral('-')
          .appendDayOfWeek(1)
          .toFormatter()
      }
      dwe
    }

    private def dayOfYearElement(): DateTimeFormatter = {
      if (dye == null) {
        return new DateTimeFormatterBuilder()
          .appendLiteral('-')
          .appendDayOfYear(3)
          .toFormatter()
      }
      dye
    }

    private def literalTElement(): DateTimeFormatter = {
      if (lte == null) {
        return new DateTimeFormatterBuilder().appendLiteral('T').toFormatter()
      }
      lte
    }

    private def hourElement(): DateTimeFormatter = {
      if (hde == null) {
        return new DateTimeFormatterBuilder().appendHourOfDay(2).toFormatter()
      }
      hde
    }

    private def minuteElement(): DateTimeFormatter = {
      if (mhe == null) {
        return new DateTimeFormatterBuilder()
          .appendLiteral(':')
          .appendMinuteOfHour(2)
          .toFormatter()
      }
      mhe
    }

    private def secondElement(): DateTimeFormatter = {
      if (sme == null) {
        return new DateTimeFormatterBuilder()
          .appendLiteral(':')
          .appendSecondOfMinute(2)
          .toFormatter()
      }
      sme
    }

    private def fractionElement(): DateTimeFormatter = {
      if (fse == null) {
        return new DateTimeFormatterBuilder()
          .appendLiteral('.')
          .appendFractionOfSecond(3, 9)
          .toFormatter()
      }
      fse
    }

    private def offsetElement(): DateTimeFormatter = {
      if (ze == null) {
        return new DateTimeFormatterBuilder()
          .appendTimeZoneOffset("Z", showSeparators = true, 2, 4)
          .toFormatter()
      }
      ze
    }
  }
}
