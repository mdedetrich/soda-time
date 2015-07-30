package org.joda.time

abstract class Chronology {

  def getZone(): DateTimeZone

  def withUTC(): Chronology

  def withZone(zone: DateTimeZone): Chronology

  def getDateTimeMillis(year: Int,
                        monthOfYear: Int,
                        dayOfMonth: Int,
                        millisOfDay: Int): Long

  def getDateTimeMillis(year: Int,
                        monthOfYear: Int,
                        dayOfMonth: Int,
                        hourOfDay: Int,
                        minuteOfHour: Int,
                        secondOfMinute: Int,
                        millisOfSecond: Int): Long

  def getDateTimeMillis(instant: Long,
                        hourOfDay: Int,
                        minuteOfHour: Int,
                        secondOfMinute: Int,
                        millisOfSecond: Int): Long

  def validate(partial: ReadablePartial, values: Array[Int]): Unit

  def get(partial: ReadablePartial, instant: Long): Array[Int]

  def set(partial: ReadablePartial, instant: Long): Long

  def get(period: ReadablePeriod, startInstant: Long, endInstant: Long): Array[Int]

  def get(period: ReadablePeriod, duration: Long): Array[Int]

  def add(period: ReadablePeriod, instant: Long, scalar: Int): Long

  def add(instant: Long, duration: Long, scalar: Int): Long

  def millis(): DurationField

  def millisOfSecond(): DateTimeField

  def millisOfDay(): DateTimeField

  def seconds(): DurationField

  def secondOfMinute(): DateTimeField

  def secondOfDay(): DateTimeField

  def minutes(): DurationField

  def minuteOfHour(): DateTimeField

  def minuteOfDay(): DateTimeField

  def hours(): DurationField

  def hourOfDay(): DateTimeField

  def clockhourOfDay(): DateTimeField

  def halfdays(): DurationField

  def hourOfHalfday(): DateTimeField

  def clockhourOfHalfday(): DateTimeField

  def halfdayOfDay(): DateTimeField

  def days(): DurationField

  def dayOfWeek(): DateTimeField

  def dayOfMonth(): DateTimeField

  def dayOfYear(): DateTimeField

  def weeks(): DurationField

  def weekOfWeekyear(): DateTimeField

  def weekyears(): DurationField

  def weekyear(): DateTimeField

  def weekyearOfCentury(): DateTimeField

  def months(): DurationField

  def monthOfYear(): DateTimeField

  def years(): DurationField

  def year(): DateTimeField

  def yearOfEra(): DateTimeField

  def yearOfCentury(): DateTimeField

  def centuries(): DurationField

  def centuryOfEra(): DateTimeField

  def eras(): DurationField

  def era(): DateTimeField

  override def toString(): String
}
