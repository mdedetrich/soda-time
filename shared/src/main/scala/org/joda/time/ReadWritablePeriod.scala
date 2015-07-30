package org.joda.time

trait ReadWritablePeriod extends ReadablePeriod {

  def clear(): Unit

  def setValue(index: Int, value: Int): Unit

  def set(field: DurationFieldType, value: Int): Unit

  def setPeriod(period: ReadablePeriod): Unit

  def setPeriod(years: Int,
                months: Int,
                weeks: Int,
                days: Int,
                hours: Int,
                minutes: Int,
                seconds: Int,
                millis: Int): Unit

  def setPeriod(interval: ReadableInterval): Unit

  def add(field: DurationFieldType, value: Int): Unit

  def add(period: ReadablePeriod): Unit

  def add(years: Int,
          months: Int,
          weeks: Int,
          days: Int,
          hours: Int,
          minutes: Int,
          seconds: Int,
          millis: Int): Unit

  def add(interval: ReadableInterval): Unit

  def setYears(years: Int): Unit

  def addYears(years: Int): Unit

  def setMonths(months: Int): Unit

  def addMonths(months: Int): Unit

  def setWeeks(weeks: Int): Unit

  def addWeeks(weeks: Int): Unit

  def setDays(days: Int): Unit

  def addDays(days: Int): Unit

  def setHours(hours: Int): Unit

  def addHours(hours: Int): Unit

  def setMinutes(minutes: Int): Unit

  def addMinutes(minutes: Int): Unit

  def setSeconds(seconds: Int): Unit

  def addSeconds(seconds: Int): Unit

  def setMillis(millis: Int): Unit

  def addMillis(millis: Int): Unit
}
