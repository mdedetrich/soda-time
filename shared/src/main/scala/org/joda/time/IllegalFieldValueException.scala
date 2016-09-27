package org.joda.time

object IllegalFieldValueException {

  def create(fieldType: DateTimeFieldType,
             value: Number,
             lowerBound: Number,
             upperBound: Number): IllegalFieldValueException = {
    val error = new IllegalFieldValueException(
      createMessage(fieldType.getName, value, lowerBound, upperBound, null))
    error.auxConstructor(fieldType, value, lowerBound, upperBound)
    error
  }

  def create(fieldType: DateTimeFieldType,
             value: Number,
             explain: String): IllegalFieldValueException = {
    val error = new IllegalFieldValueException(
      createMessage(fieldType.getName(), value, null, null, explain))
    error.auxConstructor(fieldType, value, explain)
    error
  }

  def create(fieldType: DurationFieldType,
             value: Number,
             lowerBound: Number,
             upperBound: Number): IllegalFieldValueException = {
    val error = new IllegalFieldValueException(
      createMessage(fieldType.getName(), value, lowerBound, upperBound, null))
    error.auxConstructor(fieldType, value, lowerBound, upperBound)
    error
  }

  def create(fieldName: String,
             value: Number,
             lowerBound: Number,
             upperBound: Number): IllegalFieldValueException = {
    val error = new IllegalFieldValueException(
      createMessage(fieldName, value, lowerBound, upperBound, null))
    error.auxConstructor(fieldName, value, lowerBound, upperBound)
    error
  }

  def create(fieldType: DateTimeFieldType,
             value: String): IllegalFieldValueException = {
    val error = new IllegalFieldValueException(
      createMessage(fieldType.getName(), value))
    error.auxConstructor(fieldType, value)
    error
  }

  def create(fieldType: DurationFieldType,
             value: String): IllegalFieldValueException = {
    val error = new IllegalFieldValueException(
      createMessage(fieldType.getName(), value))
    error.auxConstructor(fieldType, value)
    error
  }

  def create(fieldName: String, value: String): IllegalFieldValueException = {
    val error = new IllegalFieldValueException(createMessage(fieldName, value))
    error.auxConstructor(fieldName, value)
    error
  }

  private def createMessage(fieldName: String,
                            value: Number,
                            lowerBound: Number,
                            upperBound: Number,
                            explain: String): String = {
    val buf = new StringBuilder()
      .append("Value ")
      .append(value)
      .append(" for ")
      .append(fieldName)
      .append(' ')
    if (lowerBound == null) {
      if (upperBound == null) {
        buf.append("is not supported")
      } else {
        buf.append("must not be larger than ").append(upperBound)
      }
    } else if (upperBound == null) {
      buf.append("must not be smaller than ").append(lowerBound)
    } else {
      buf
        .append("must be in the range [")
        .append(lowerBound)
        .append(',')
        .append(upperBound)
        .append(']')
    }
    if (explain != null) {
      buf.append(": ").append(explain)
    }
    buf.toString
  }

  private def createMessage(fieldName: String, value: String): String = {
    val buf = new StringBuffer().append("Value ")
    if (value == null) {
      buf.append("null")
    } else {
      buf.append('"')
      buf.append(value)
      buf.append('"')
    }
    buf
      .append(" for ")
      .append(fieldName)
      .append(' ')
      .append("is not supported")
    buf.toString
  }
}

@SerialVersionUID(6305711765985447737L)
class IllegalFieldValueException(message: String)
    extends IllegalArgumentException(message) {

  private var iNumberValue: Number = null
  private var iLowerBound: Number = null
  private var iUpperBound: Number = null
  private var iDateTimeFieldType: DateTimeFieldType = null
  private var iDurationFieldType: DurationFieldType = null
  private var iFieldName: String = null
  private var iStringValue: String = null
  private var iMessage: String = message

  def auxConstructor(fieldType: DateTimeFieldType,
                     value: Number,
                     lowerBound: Number,
                     upperBound: Number): Unit = {
    iDateTimeFieldType = fieldType
    iDurationFieldType = null
    iFieldName = fieldType.getName()
    iNumberValue = value
    iStringValue = null
    iLowerBound = lowerBound
    iUpperBound = upperBound
    iMessage = super.getMessage()
  }

  def auxConstructor(fieldType: DateTimeFieldType,
                     value: Number,
                     explain: String): Unit = {
    iDateTimeFieldType = fieldType
    iDurationFieldType = null
    iFieldName = fieldType.getName
    iNumberValue = value
    iStringValue = null
    iLowerBound = null
    iUpperBound = null
  }

  def auxConstructor(fieldType: DurationFieldType,
                     value: Number,
                     lowerBound: Number,
                     upperBound: Number): Unit = {
    iDateTimeFieldType = null
    iDurationFieldType = fieldType
    iFieldName = fieldType.getName
    iNumberValue = value
    iStringValue = null
    iLowerBound = lowerBound
    iUpperBound = upperBound
  }

  def auxConstructor(fieldName: String,
                     value: Number,
                     lowerBound: Number,
                     upperBound: Number): Unit = {
    iDateTimeFieldType = null
    iDurationFieldType = null
    iFieldName = fieldName
    iNumberValue = value
    iStringValue = null
    iLowerBound = lowerBound
    iUpperBound = upperBound
  }

  def auxConstructor(fieldType: DateTimeFieldType, value: String): Unit = {
    iDateTimeFieldType = fieldType
    iDurationFieldType = null
    iFieldName = fieldType.getName
    iStringValue = value
    iNumberValue = null
    iLowerBound = null
    iUpperBound = null
  }

  def auxConstructor(fieldType: DurationFieldType, value: String): Unit = {
    iDateTimeFieldType = null
    iDurationFieldType = fieldType
    iFieldName = fieldType.getName
    iStringValue = value
    iNumberValue = null
    iLowerBound = null
    iUpperBound = null
  }

  def auxConstructor(fieldName: String, value: String): Unit = {
    iDateTimeFieldType = null
    iDurationFieldType = null
    iFieldName = fieldName
    iStringValue = value
    iNumberValue = null
    iLowerBound = null
    iUpperBound = null
  }

  def getDateTimeFieldType(): DateTimeFieldType = iDateTimeFieldType

  def getDurationFieldType(): DurationFieldType = iDurationFieldType

  def getFieldName(): String = iFieldName

  def getIllegalNumberValue(): Number = iNumberValue

  def getIllegalStringValue(): String = iStringValue

  def getIllegalValueAsString(): String = {
    var value: String = iStringValue
    if (value == null) {
      value = String.valueOf(iNumberValue)
    }
    value
  }

  def getLowerBound(): Number = iLowerBound

  def getUpperBound(): Number = iUpperBound

  override def getMessage(): String = iMessage

  def prependMessage(message: String) {
    if (iMessage == null) {
      iMessage = message
    } else if (message != null) {
      iMessage = message + ": " + iMessage
    }
  }
}
