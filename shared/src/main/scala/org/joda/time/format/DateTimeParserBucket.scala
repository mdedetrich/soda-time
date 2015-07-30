package org.joda.time.format

import java.util.Arrays
import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeField
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeUtils
import org.joda.time.DateTimeZone
import org.joda.time.DurationField
import org.joda.time.DurationFieldType
import org.joda.time.IllegalFieldValueException
import org.joda.time.IllegalInstantException
import DateTimeParserBucket._

object DateTimeParserBucket {

  private def sort(array: Array[SavedField], high: Int) {
    if (high > 10) {
      Arrays.sort(array.map(_.asInstanceOf[AnyRef]), 0, high,array.asInstanceOf[Ordering[Object]])
    } else {
      for (i <- 0 until high) {
        var j = i
        while (j > 0 && array(j - 1).compareTo(array(j)) > 0) {
          val t = array(j)
          array(j) = array(j - 1)
          array(j - 1) = t
          j -= 1
        }
      }
    }
  }

  class SavedField() extends Comparable[SavedField] {

    var iField: DateTimeField = _
    var iValue: Int = _
    var iText: String = null
    var iLocale: Locale = null

    def init(field: DateTimeField, value: Int) {
      iField = field
      iValue = value
      iText = null
      iLocale = null
    }

    def init(field: DateTimeField, text: String, locale: Locale) {
      iField = field
      iValue = 0
      iText = text
      iLocale = locale
    }

    def set(millis: Long, reset: Boolean): Long = {
      var _millis:Long = millis
      _millis = if (iText == null) iField.set(_millis, iValue) else iField.set(_millis, iText, iLocale)
      if (reset) {
        _millis = iField.roundFloor(_millis)
      }
      _millis
    }

    def compareTo(obj: SavedField): Int = {
      val other = obj.iField
      val result = compareReverse(iField.getRangeDurationField, other.getRangeDurationField)
      if (result != 0) {
        return result
      }
      compareReverse(iField.getDurationField, other.getDurationField)
    }
  }

  def compareReverse(a: DurationField, b: DurationField): Int = {
    if (a == null || !a.isSupported) {
      if (b == null || !b.isSupported) {
        return 0
      }
      return -1
    }
    if (b == null || !b.isSupported) {
      return 1
    }
    -a.compareTo(b)
  }
}

class DateTimeParserBucket(private val instantLocal: Long,
                           var chrono: Chronology,
                           locale: Locale,
                           private val pivotYear: Integer,
                           private val defaultYear: Int) {
  
  private var iChrono = chrono.withUTC()
  private var iLocale = if (locale == null) Locale.getDefault else locale
  private var iDefaultZone = chrono.getZone
  private var iZone: DateTimeZone = null
  private var iOffset: Integer = null
  private var iSavedFields: Array[SavedField] = new Array[SavedField](8)
  private var iSavedFieldsCount: Int = _
  private var iSavedFieldsShared: Boolean = _
  private var iSavedState: AnyRef = null
  private var iPivotYear: Integer = null
  private var iMillis: Long = _
  private var iDefaultYear: Int = _
  private var iDefaultPivotYear: Int = _

  chrono = DateTimeUtils.getChronology(chrono)

  iMillis = instantLocal
  iDefaultZone = chrono.getZone()
  iChrono = chrono.withUTC()
  iLocale = if (locale == null) Locale.getDefault() else  locale
  iDefaultYear = defaultYear
  iDefaultPivotYear = pivotYear
  // reset
  iZone = iDefaultZone
  iPivotYear = iDefaultPivotYear
  iSavedFields = new Array[SavedField](8)

  @Deprecated
  def this(instantLocal: Long, chrono: Chronology, locale: Locale) {
    this(instantLocal, chrono, locale, null, 2000)
  }

  @Deprecated
  def this(instantLocal: Long,
           chrono: Chronology,
           locale: Locale,
           pivotYear: Integer) {
    this(instantLocal, chrono, locale, pivotYear, 2000)
  }

  def reset() {
    iZone = iDefaultZone
    iOffset = null
    iPivotYear = iDefaultPivotYear
    iSavedFieldsCount = 0
    iSavedFieldsShared = false
    iSavedState = null
  }

  def parseMillis(parser: DateTimeParser, text: CharSequence): Long = {
    reset()
    doParseMillis(DateTimeParserInternalParser.of(parser), text)
  }

  def doParseMillis(parser: InternalParser, text: CharSequence): Long = {
    var newPos = parser.parseInto(this, text, 0)
    if (newPos >= 0) {
      if (newPos >= text.length) {
        return computeMillis(resetFields = true, text)
      }
    } else {
      newPos = ~newPos
    }
    throw new IllegalArgumentException(FormatUtils.createErrorMessage(text.toString, newPos))
  }

  def getChronology(): Chronology = iChrono

  def getLocale(): Locale = iLocale

  def getZone(): DateTimeZone = iZone

  def setZone(zone: DateTimeZone) {
    iSavedState = null
    iZone = zone
  }

  @Deprecated
  def getOffset(): Int = if (iOffset != null) iOffset else 0

  def getOffsetInteger(): Integer = iOffset

  @Deprecated
  def setOffset(offset: Int) {
    iSavedState = null
    iOffset = offset
  }

  def setOffset(offset: Integer) {
    iSavedState = null
    iOffset = offset
  }

  def getPivotYear(): Integer = iPivotYear

  @Deprecated
  def setPivotYear(pivotYear: Integer) {
    iPivotYear = pivotYear
  }

  def saveField(field: DateTimeField, value: Int) {
    obtainSaveField().init(field, value)
  }

  def saveField(fieldType: DateTimeFieldType, value: Int) {
    obtainSaveField().init(fieldType.getField(iChrono), value)
  }

  def saveField(fieldType: DateTimeFieldType, text: String, locale: Locale) {
    obtainSaveField().init(fieldType.getField(iChrono), text, locale)
  }

  private def obtainSaveField(): SavedField = {
    var savedFields:Array[SavedField] = iSavedFields
    val savedFieldsCount = iSavedFieldsCount
    if (savedFieldsCount == savedFields.length || iSavedFieldsShared) {
      val newArray = Array.ofDim[SavedField](if (savedFieldsCount == savedFields.length) savedFieldsCount * 2 else savedFields.length)
      System.arraycopy(savedFields, 0, newArray, 0, savedFieldsCount)
      iSavedFields = newArray 
      savedFields = newArray
      iSavedFieldsShared = false
    }
    iSavedState = null
    var saved = savedFields(savedFieldsCount)
    if (saved == null) {
      val s = new SavedField()
      saved = s
      savedFields(savedFieldsCount) = s
    }
    iSavedFieldsCount = savedFieldsCount + 1
    saved
  }

  def saveState(): AnyRef = {
    if (iSavedState == null) {
      iSavedState = new SavedState()
    }
    iSavedState
  }

  def restoreState(savedState: AnyRef): Boolean = {
    if (savedState.isInstanceOf[SavedState]) {
      if (savedState.asInstanceOf[SavedState].restoreState(this)) {
        iSavedState = savedState
        return true
      }
    }
    false
  }

  def computeMillis(): Long = {
    computeMillis(resetFields = false, null.asInstanceOf[CharSequence])
  }

  def computeMillis(resetFields: Boolean): Long = {
    computeMillis(resetFields, null.asInstanceOf[CharSequence])
  }

  def computeMillis(resetFields: Boolean, text: String): Long = {
    computeMillis(resetFields, text.asInstanceOf[CharSequence])
  }

  def computeMillis(resetFields: Boolean, text: CharSequence): Long = {
    var savedFields = iSavedFields
    val count = iSavedFieldsCount
    if (iSavedFieldsShared) {
      val s = iSavedFields.clone().asInstanceOf[Array[SavedField]]
      iSavedFields = s
      savedFields = s
      iSavedFieldsShared = false
    }
    sort(savedFields, count)
    if (count > 0) {
      val months = DurationFieldType.months().getField(iChrono)
      val days = DurationFieldType.days().getField(iChrono)
      val first = savedFields(0).iField.getDurationField
      if (compareReverse(first, months) >= 0 && compareReverse(first, days) <= 0) {
        saveField(DateTimeFieldType.year(), iDefaultYear)
        return computeMillis(resetFields, text)
      }
    }
    var millis = iMillis
    try {
      for (i <- 0 until count) {
        millis = savedFields(i).set(millis, resetFields)
      }
      if (resetFields) {
        for (i <- 0 until count) {
          millis = savedFields(i).set(millis, i == (count - 1))
        }
      }
    } catch {
      case e: IllegalFieldValueException =>
        if (text != null) {
          e.prependMessage("Cannot parse \"" + text + '"')
        }
        throw e
    }
    if (iOffset != null) {
      millis -= iOffset
    } else if (iZone != null) {
      val offset = iZone.getOffsetFromLocal(millis)
      millis -= offset
      if (offset != iZone.getOffset(millis)) {
        var message = "Illegal instant due to time zone offset transition (" +
          iZone +
          ')'
        if (text != null) {
          message = "Cannot parse \"" + text + "\": " + message
        }
        throw new IllegalInstantException(message)
      }
    }
    millis
  }

  class SavedState() {

    val iZone = DateTimeParserBucket.this.iZone

    val iOffset = DateTimeParserBucket.this.iOffset

    val iSavedFields = DateTimeParserBucket.this.iSavedFields

    val iSavedFieldsCount = DateTimeParserBucket.this.iSavedFieldsCount

    def restoreState(enclosing: DateTimeParserBucket): Boolean = {
      if (enclosing != DateTimeParserBucket.this) {
        return false
      }
      enclosing.iZone = this.iZone
      enclosing.iOffset = this.iOffset
      enclosing.iSavedFields = this.iSavedFields
      if (this.iSavedFieldsCount < enclosing.iSavedFieldsCount) {
        enclosing.iSavedFieldsShared = true
      }
      enclosing.iSavedFieldsCount = this.iSavedFieldsCount
      true
    }
  }
}
