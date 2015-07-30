package org.joda.time.field

import org.joda.time.Chronology
import org.joda.time.DateTimeField

object LenientDateTimeField {

  def getInstance(field: DateTimeField, base: Chronology): DateTimeField = {
    var _field: DateTimeField = field
    if (_field == null) {
      return null
    }
    if (_field.isInstanceOf[StrictDateTimeField]) {
      _field = _field.asInstanceOf[StrictDateTimeField].getWrappedField
    }
    if (_field.isLenient) {
      return _field
    }
    new LenientDateTimeField(_field, base)
  }
}

@SerialVersionUID(8714085824173290599L)
class LenientDateTimeField protected (field: DateTimeField, private val iBase: Chronology)
  extends DelegatedDateTimeField(field) {

  override def isLenient(): Boolean = true

  override def set(instant: Long, value: Int): Long = {
    var localInstant = iBase.getZone.convertUTCToLocal(instant)
    val difference = FieldUtils.safeSubtract(value, get(instant))
    localInstant = getType.getField(iBase.withUTC()).add(localInstant, difference)
    iBase.getZone.convertLocalToUTC(localInstant, strict = false, instant)
  }
}
