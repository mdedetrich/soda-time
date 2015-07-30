package org.joda.time.format

object ISOPeriodFormat {

  private var cStandard: PeriodFormatter = null
  private var cAlternate: PeriodFormatter = null
  private var cAlternateExtended: PeriodFormatter = null
  private var cAlternateWithWeeks: PeriodFormatter = null
  private var cAlternateExtendedWihWeeks: PeriodFormatter = null

  def standard(): PeriodFormatter = {
    if (cStandard == null) {
      cStandard = new PeriodFormatterBuilder().appendLiteral("P").appendYears()
        .appendSuffix("Y")
        .appendMonths()
        .appendSuffix("M")
        .appendWeeks()
        .appendSuffix("W")
        .appendDays()
        .appendSuffix("D")
        .appendSeparatorIfFieldsAfter("T")
        .appendHours()
        .appendSuffix("H")
        .appendMinutes()
        .appendSuffix("M")
        .appendSecondsWithOptionalMillis()
        .appendSuffix("S")
        .toFormatter()
    }
    cStandard
  }

  def alternate(): PeriodFormatter = {
    if (cAlternate == null) {
      cAlternate = new PeriodFormatterBuilder().appendLiteral("P").printZeroAlways()
        .minimumPrintedDigits(4)
        .appendYears()
        .minimumPrintedDigits(2)
        .appendMonths()
        .appendDays()
        .appendSeparatorIfFieldsAfter("T")
        .appendHours()
        .appendMinutes()
        .appendSecondsWithOptionalMillis()
        .toFormatter()
    }
    cAlternate
  }

  def alternateExtended(): PeriodFormatter = {
    if (cAlternateExtended == null) {
      cAlternateExtended = new PeriodFormatterBuilder().appendLiteral("P").printZeroAlways()
        .minimumPrintedDigits(4)
        .appendYears()
        .appendSeparator("-")
        .minimumPrintedDigits(2)
        .appendMonths()
        .appendSeparator("-")
        .appendDays()
        .appendSeparatorIfFieldsAfter("T")
        .appendHours()
        .appendSeparator(":")
        .appendMinutes()
        .appendSeparator(":")
        .appendSecondsWithOptionalMillis()
        .toFormatter()
    }
    cAlternateExtended
  }

  def alternateWithWeeks(): PeriodFormatter = {
    if (cAlternateWithWeeks == null) {
      cAlternateWithWeeks = new PeriodFormatterBuilder().appendLiteral("P").printZeroAlways()
        .minimumPrintedDigits(4)
        .appendYears()
        .minimumPrintedDigits(2)
        .appendPrefix("W")
        .appendWeeks()
        .appendDays()
        .appendSeparatorIfFieldsAfter("T")
        .appendHours()
        .appendMinutes()
        .appendSecondsWithOptionalMillis()
        .toFormatter()
    }
    cAlternateWithWeeks
  }

  def alternateExtendedWithWeeks(): PeriodFormatter = {
    if (cAlternateExtendedWihWeeks == null) {
      cAlternateExtendedWihWeeks = new PeriodFormatterBuilder().appendLiteral("P").printZeroAlways()
        .minimumPrintedDigits(4)
        .appendYears()
        .appendSeparator("-")
        .minimumPrintedDigits(2)
        .appendPrefix("W")
        .appendWeeks()
        .appendSeparator("-")
        .appendDays()
        .appendSeparatorIfFieldsAfter("T")
        .appendHours()
        .appendSeparator(":")
        .appendMinutes()
        .appendSeparator(":")
        .appendSecondsWithOptionalMillis()
        .toFormatter()
    }
    cAlternateExtendedWihWeeks
  }
}

class ISOPeriodFormat
