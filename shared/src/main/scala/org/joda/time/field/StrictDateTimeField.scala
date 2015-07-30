package org.joda.time.field

import org.joda.time.DateTimeField

object StrictDateTimeField {

  def getInstance(field: DateTimeField): DateTimeField = {
    var _field: DateTimeField = field
    if (_field == null) {
      return null
    }
    if (_field.isInstanceOf[LenientDateTimeField]) {
      _field = _field.asInstanceOf[LenientDateTimeField].getWrappedField
    }
    if (!_field.isLenient) {
      return _field
    }
    new StrictDateTimeField(_field)
  }
}

@SerialVersionUID(3154803964207950910L)
class StrictDateTimeField protected (field: DateTimeField) extends DelegatedDateTimeField(field) {

  override def isLenient(): Boolean = false

  override def set(instant: Long, value: Int): Long = {
    FieldUtils.verifyValueBounds(this, value, getMinimumValue(instant), getMaximumValue(instant))
    super.set(instant, value)
  }
}
