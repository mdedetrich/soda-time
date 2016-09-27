package org.joda.time.chrono

import java.util.Locale
import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.DurationFieldType
import org.joda.time.IllegalFieldValueException
import org.joda.time.field.BaseDateTimeField
import org.joda.time.field.FieldUtils
import org.joda.time.field.UnsupportedDurationField
import BasicSingleEraDateTimeField._

object BasicSingleEraDateTimeField {
  private val ERA_VALUE = DateTimeConstants.CE
}

class BasicSingleEraDateTimeField(private val iEraText: String)
    extends BaseDateTimeField(DateTimeFieldType.era()) {

  def isLenient(): Boolean = false

  def get(instant: Long): Int = ERA_VALUE

  def set(instant: Long, era: Int): Long = {
    FieldUtils.verifyValueBounds(this, era, ERA_VALUE, ERA_VALUE)
    instant
  }

  override def set(instant: Long, text: String, locale: Locale): Long = {
    if (iEraText == text == false && "1" == text == false) {
      throw IllegalFieldValueException.create(DateTimeFieldType.era(), text)
    }
    instant
  }

  def roundFloor(instant: Long): Long = Long.MinValue

  override def roundCeiling(instant: Long): Long = Long.MaxValue

  override def roundHalfFloor(instant: Long): Long = Long.MinValue

  override def roundHalfCeiling(instant: Long): Long = Long.MinValue

  override def roundHalfEven(instant: Long): Long = Long.MinValue

  def getDurationField(): DurationField = {
    UnsupportedDurationField.getInstance(DurationFieldType.eras())
  }

  def getRangeDurationField(): DurationField = null

  def getMinimumValue(): Int = ERA_VALUE

  def getMaximumValue(): Int = ERA_VALUE

  override def getAsText(fieldValue: Int, locale: Locale): String = iEraText

  override def getMaximumTextLength(locale: Locale): Int = iEraText.length
}
