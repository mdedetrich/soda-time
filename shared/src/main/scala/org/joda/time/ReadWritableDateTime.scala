package org.joda.time

trait ReadWritableDateTime extends ReadableDateTime with ReadWritableInstant {

  def setYear(year: Int): Unit

  def addYears(years: Int): Unit

  def setWeekyear(weekyear: Int): Unit

  def addWeekyears(weekyears: Int): Unit

  def setMonthOfYear(monthOfYear: Int): Unit

  def addMonths(months: Int): Unit

  def setWeekOfWeekyear(weekOfWeekyear: Int): Unit

  def addWeeks(weeks: Int): Unit

  def setDayOfYear(dayOfYear: Int): Unit

  def setDayOfMonth(dayOfMonth: Int): Unit

  def setDayOfWeek(dayOfWeek: Int): Unit

  def addDays(days: Int): Unit

  def setHourOfDay(hourOfDay: Int): Unit

  def addHours(hours: Int): Unit

  def setMinuteOfDay(minuteOfDay: Int): Unit

  def setMinuteOfHour(minuteOfHour: Int): Unit

  def addMinutes(minutes: Int): Unit

  def setSecondOfDay(secondOfDay: Int): Unit

  def setSecondOfMinute(secondOfMinute: Int): Unit

  def addSeconds(seconds: Int): Unit

  def setMillisOfDay(millisOfDay: Int): Unit

  def setMillisOfSecond(millisOfSecond: Int): Unit

  def addMillis(millis: Int): Unit

  def setDate(year: Int, monthOfYear: Int, dayOfMonth: Int): Unit

  def setTime(hour: Int,
              minuteOfHour: Int,
              secondOfMinute: Int,
              millisOfSecond: Int): Unit

  def setDateTime(year: Int,
                  monthOfYear: Int,
                  dayOfMonth: Int,
                  hourOfDay: Int,
                  minuteOfHour: Int,
                  secondOfMinute: Int,
                  millisOfSecond: Int): Unit
}
