package org.joda.time

import java.util.Locale

abstract class DateTimeField {

  def getType(): DateTimeFieldType

  def getName(): String

  def isSupported(): Boolean

  def isLenient(): Boolean

  def get(instant: Long): Int

  def getAsText(instant: Long, locale: Locale): String

  def getAsText(instant: Long): String

  def getAsText(partial: ReadablePartial, fieldValue: Int, locale: Locale): String

  def getAsText(partial: ReadablePartial, locale: Locale): String

  def getAsText(fieldValue: Int, locale: Locale): String

  def getAsShortText(instant: Long, locale: Locale): String

  def getAsShortText(instant: Long): String

  def getAsShortText(partial: ReadablePartial, fieldValue: Int, locale: Locale): String

  def getAsShortText(partial: ReadablePartial, locale: Locale): String

  def getAsShortText(fieldValue: Int, locale: Locale): String

  def add(instant: Long, value: Int): Long

  def add(instant: Long, value: Long): Long

  def add(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          valueToAdd: Int): Array[Int]

  def addWrapPartial(instant: ReadablePartial,
                     fieldIndex: Int,
                     values: Array[Int],
                     valueToAdd: Int): Array[Int]

  def addWrapField(instant: Long, value: Int): Long

  def addWrapField(instant: ReadablePartial,
                   fieldIndex: Int,
                   values: Array[Int],
                   valueToAdd: Int): Array[Int]

  def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int

  def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long

  def set(instant: Long, value: Int): Long

  def set(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          newValue: Int): Array[Int]

  def set(instant: Long, text: String, locale: Locale): Long

  def set(instant: Long, text: String): Long

  def set(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          text: String,
          locale: Locale): Array[Int]

  def getDurationField(): DurationField

  def getRangeDurationField(): DurationField

  def isLeap(instant: Long): Boolean

  def getLeapAmount(instant: Long): Int

  def getLeapDurationField(): DurationField

  def getMinimumValue(): Int

  def getMinimumValue(instant: Long): Int

  def getMinimumValue(instant: ReadablePartial): Int

  def getMinimumValue(instant: ReadablePartial, values: Array[Int]): Int

  def getMaximumValue(): Int

  def getMaximumValue(instant: Long): Int

  def getMaximumValue(instant: ReadablePartial): Int

  def getMaximumValue(instant: ReadablePartial, values: Array[Int]): Int

  def getMaximumTextLength(locale: Locale): Int

  def getMaximumShortTextLength(locale: Locale): Int

  def roundFloor(instant: Long): Long

  def roundCeiling(instant: Long): Long

  def roundHalfFloor(instant: Long): Long

  def roundHalfCeiling(instant: Long): Long

  def roundHalfEven(instant: Long): Long

  def remainder(instant: Long): Long

  override def toString(): String
}
