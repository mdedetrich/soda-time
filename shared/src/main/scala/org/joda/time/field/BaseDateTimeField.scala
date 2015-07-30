package org.joda.time.field

import java.util.Locale
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DurationField
import org.joda.time.ReadablePartial
import scala.util.control.Breaks._

abstract class BaseDateTimeField protected (private val `type`: DateTimeFieldType)
  extends DateTimeField() {
  
  private var iType: DateTimeFieldType = null

  if (`type` == null) {
    throw new IllegalArgumentException("The type must not be null")
  }
  
  iType = `type`

  def getType(): DateTimeFieldType = iType

  def getName(): String = iType.getName

  def isSupported(): Boolean = true

  def get(instant: Long): Int

  def getAsText(instant: Long, locale: Locale): String = getAsText(get(instant), locale)

  def getAsText(instant: Long): String = getAsText(instant, null)

  def getAsText(partial: ReadablePartial, fieldValue: Int, locale: Locale): String = getAsText(fieldValue, locale)

  def getAsText(partial: ReadablePartial, locale: Locale): String = {
    getAsText(partial, partial.get(getType), locale)
  }

  def getAsText(fieldValue: Int, locale: Locale): String = Integer.toString(fieldValue)

  def getAsShortText(instant: Long, locale: Locale): String = getAsShortText(get(instant), locale)

  def getAsShortText(instant: Long): String = getAsShortText(instant, null)

  def getAsShortText(partial: ReadablePartial, fieldValue: Int, locale: Locale): String = getAsShortText(fieldValue, locale)

  def getAsShortText(partial: ReadablePartial, locale: Locale): String = {
    getAsShortText(partial, partial.get(getType), locale)
  }

  def getAsShortText(fieldValue: Int, locale: Locale): String = getAsText(fieldValue, locale)

  def add(instant: Long, value: Int): Long = getDurationField.add(instant, value)

  def add(instant: Long, value: Long): Long = getDurationField.add(instant, value)

  def add(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          valueToAdd: Int): Array[Int] = {
    var _values: Array[Int] = values
    var _valueToAdd: Int = valueToAdd
    if (_valueToAdd == 0) {
      return _values
    }
    var nextField: DateTimeField = null
    while (_valueToAdd > 0) {
      val max = getMaximumValue(instant, _values)
      val proposed = _values(fieldIndex) + _valueToAdd
      if (proposed <= max) {
        _values(fieldIndex) = proposed.toInt
        break()
      }
      if (nextField == null) {
        if (fieldIndex == 0) {
          throw new IllegalArgumentException("Maximum value exceeded for add")
        }
        nextField = instant.getField(fieldIndex - 1)
        if (getRangeDurationField.getType != nextField.getDurationField.getType) {
          throw new IllegalArgumentException("Fields invalid for add")
        }
      }
      _valueToAdd -= (max + 1) - _values(fieldIndex)
      _values = nextField.add(instant, fieldIndex - 1, _values, 1)
      _values(fieldIndex) = getMinimumValue(instant, _values)
    }
    while (_valueToAdd < 0) {
      val min = getMinimumValue(instant, _values)
      val proposed = _values(fieldIndex) + _valueToAdd
      if (proposed >= min) {
        _values(fieldIndex) = proposed.toInt
        break()
      }
      if (nextField == null) {
        if (fieldIndex == 0) {
          throw new IllegalArgumentException("Maximum value exceeded for add")
        }
        nextField = instant.getField(fieldIndex - 1)
        if (getRangeDurationField.getType != nextField.getDurationField.getType) {
          throw new IllegalArgumentException("Fields invalid for add")
        }
      }
      _valueToAdd -= (min - 1) - _values(fieldIndex)
      _values = nextField.add(instant, fieldIndex - 1, _values, -1)
      _values(fieldIndex) = getMaximumValue(instant, _values)
    }
    set(instant, fieldIndex, _values, _values(fieldIndex))
  }

  def addWrapPartial(instant: ReadablePartial,
                     fieldIndex: Int,
                     values: Array[Int],
                     valueToAdd: Int): Array[Int] = {
    var _valueToAdd:Int = valueToAdd
    var _values: Array[Int] = values
    if (_valueToAdd == 0) {
      return _values
    }
    var nextField: DateTimeField = null
    
    while (_valueToAdd > 0) {
      var continueFlag = true
      val max = getMaximumValue(instant, _values)
      val proposed = _values(fieldIndex) + _valueToAdd
      if (proposed <= max) {
        _values(fieldIndex) = proposed.toInt
        break()
      }
      if (nextField == null) {
        if (fieldIndex == 0) {
          _valueToAdd -= (max + 1) - _values(fieldIndex)
          _values(fieldIndex) = getMinimumValue(instant, _values)
          continueFlag = false
        }
        if (continueFlag) {
          nextField = instant.getField(fieldIndex - 1)
          if (getRangeDurationField.getType != nextField.getDurationField.getType) {
            throw new IllegalArgumentException("Fields invalid for add")
          }
        }
      }
      if (continueFlag) {
        _valueToAdd -= (max + 1) - _values(fieldIndex)
        _values = nextField.addWrapPartial(instant, fieldIndex - 1, _values, 1)
        _values(fieldIndex) = getMinimumValue(instant, _values)
      }
    }
    
    while (_valueToAdd < 0) {
      var continueFlag = true
      val min = getMinimumValue(instant, _values)
      val proposed = _values(fieldIndex) + _valueToAdd
      if (proposed >= min) {
        _values(fieldIndex) = proposed.toInt
        break()
      }
      if (nextField == null) {
        if (fieldIndex == 0) {
          _valueToAdd -= (min - 1) - _values(fieldIndex)
          _values(fieldIndex) = getMaximumValue(instant, _values)
          continueFlag = false
        }
        if (continueFlag) {
          nextField = instant.getField(fieldIndex - 1)
          if (getRangeDurationField.getType != nextField.getDurationField.getType) {
            throw new IllegalArgumentException("Fields invalid for add")
          }
        }
      }
      _valueToAdd -= (min - 1) - _values(fieldIndex)
      _values = nextField.addWrapPartial(instant, fieldIndex - 1, _values, -1)
      _values(fieldIndex) = getMaximumValue(instant, _values)
    }

    set(instant, fieldIndex, _values, _values(fieldIndex))
  }

  def addWrapField(instant: Long, value: Int): Long = {
    val current = get(instant)
    val wrapped = FieldUtils.getWrappedValue(current, value, getMinimumValue(instant), getMaximumValue(instant))
    set(instant, wrapped)
  }

  def addWrapField(instant: ReadablePartial,
                   fieldIndex: Int,
                   values: Array[Int],
                   valueToAdd: Int): Array[Int] = {
    val current = values(fieldIndex)
    val wrapped = FieldUtils.getWrappedValue(current, valueToAdd, getMinimumValue(instant), getMaximumValue(instant))
    set(instant, fieldIndex, values, wrapped)
  }

  def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = {
    getDurationField.getDifference(minuendInstant, subtrahendInstant)
  }

  def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = {
    getDurationField.getDifferenceAsLong(minuendInstant, subtrahendInstant)
  }

  def set(instant: Long, value: Int): Long

  def set(partial: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          newValue: Int): Array[Int] = {
    FieldUtils.verifyValueBounds(this, newValue, getMinimumValue(partial, values), getMaximumValue(partial,
      values))
    values(fieldIndex) = newValue
    for (i <- fieldIndex + 1 until partial.size) {
      val field = partial.getField(i)
      if (values(i) > field.getMaximumValue(partial, values)) {
        values(i) = field.getMaximumValue(partial, values)
      }
      if (values(i) < field.getMinimumValue(partial, values)) {
        values(i) = field.getMinimumValue(partial, values)
      }
    }
    values
  }

  def set(instant: Long, text: String, locale: Locale): Long = {
    val value = convertText(text, locale)
    set(instant, value)
  }

  def set(instant: Long, text: String): Long = set(instant, text, null)

  def set(instant: ReadablePartial,
          fieldIndex: Int,
          values: Array[Int],
          text: String,
          locale: Locale): Array[Int] = {
    val value = convertText(text, locale)
    set(instant, fieldIndex, values, value)
  }

  protected def convertText(text: String, locale: Locale): Int = Integer.parseInt(text)

  def getDurationField(): DurationField

  def getRangeDurationField(): DurationField

  def isLeap(instant: Long): Boolean = false

  def getLeapAmount(instant: Long): Int = 0

  def getLeapDurationField(): DurationField = null

  def getMinimumValue(): Int

  def getMinimumValue(instant: Long): Int = getMinimumValue

  def getMinimumValue(instant: ReadablePartial): Int = getMinimumValue

  def getMinimumValue(instant: ReadablePartial, values: Array[Int]): Int = getMinimumValue(instant)

  def getMaximumValue(): Int

  def getMaximumValue(instant: Long): Int = getMaximumValue

  def getMaximumValue(instant: ReadablePartial): Int = getMaximumValue

  def getMaximumValue(instant: ReadablePartial, values: Array[Int]): Int = getMaximumValue(instant)

  def getMaximumTextLength(locale: Locale): Int = {
    val max = getMaximumValue
    if (max >= 0) {
      if (max < 10) {
        return 1
      } else if (max < 100) {
        return 2
      } else if (max < 1000) {
        return 3
      }
    }
    Integer.toString(max).length
  }

  def getMaximumShortTextLength(locale: Locale): Int = getMaximumTextLength(locale)

  def roundFloor(instant: Long): Long

  def roundCeiling(instant: Long): Long = {
    var _instant:Long = instant
    val newInstant = roundFloor(_instant)
    if (newInstant != _instant) {
      _instant = add(newInstant, 1)
    }
    _instant
  }

  def roundHalfFloor(instant: Long): Long = {
    val floor = roundFloor(instant)
    val ceiling = roundCeiling(instant)
    val diffFromFloor = instant - floor
    val diffToCeiling = ceiling - instant
    if (diffFromFloor <= diffToCeiling) {
      floor
    } else {
      ceiling
    }
  }

  def roundHalfCeiling(instant: Long): Long = {
    val floor = roundFloor(instant)
    val ceiling = roundCeiling(instant)
    val diffFromFloor = instant - floor
    val diffToCeiling = ceiling - instant
    if (diffToCeiling <= diffFromFloor) {
      ceiling
    } else {
      floor
    }
  }

  def roundHalfEven(instant: Long): Long = {
    val floor = roundFloor(instant)
    val ceiling = roundCeiling(instant)
    val diffFromFloor = instant - floor
    val diffToCeiling = ceiling - instant
    if (diffFromFloor < diffToCeiling) {
      floor
    } else if (diffToCeiling < diffFromFloor) {
      ceiling
    } else {
      if ((get(ceiling) & 1) == 0) {
        return ceiling
      }
      floor
    }
  }

  def remainder(instant: Long): Long = instant - roundFloor(instant)

  override def toString(): String = "DateTimeField[" + getName + ']'
}
