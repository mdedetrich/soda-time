package org.joda.time

import java.util.Locale

trait ReadableDateTime extends ReadableInstant {

  def getDayOfWeek(): Int

  def getDayOfMonth(): Int

  def getDayOfYear(): Int

  def getWeekOfWeekyear(): Int

  def getWeekyear(): Int

  def getMonthOfYear(): Int

  def getYear(): Int

  def getYearOfEra(): Int

  def getYearOfCentury(): Int

  def getCenturyOfEra(): Int

  def getEra(): Int

  def getMillisOfSecond(): Int

  def getMillisOfDay(): Int

  def getSecondOfMinute(): Int

  def getSecondOfDay(): Int

  def getMinuteOfHour(): Int

  def getMinuteOfDay(): Int

  def getHourOfDay(): Int

  def toDateTime(): DateTime

  def toMutableDateTime(): MutableDateTime

  def toString(pattern: String): String

  def toString(pattern: String, locale: Locale): String
}
