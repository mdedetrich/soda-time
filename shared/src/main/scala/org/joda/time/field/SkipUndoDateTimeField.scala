package org.joda.time.field

import org.joda.time.Chronology
import org.joda.time.DateTimeField

@SerialVersionUID(-5875876968979L)
class SkipUndoDateTimeField(private val chronology: Chronology, field: DateTimeField, private val skip: Int)
  extends DelegatedDateTimeField(field) {
  
  private var iChronology: Chronology = null
  private var iSkip: Int = _

  iChronology = chronology
  val min = super.getMinimumValue
  @transient private val iMinValue: Int = if (min < skip) min + 1 else if (min == skip + 1) skip else min
  iSkip = skip

  def this(chronology: Chronology, field: DateTimeField) {
    this(chronology, field, 0)
  }

  override def get(millis: Long): Int = {
    var value = super.get(millis)
    if (value < iSkip) {
      value += 1
    }
    value
  }

  override def set(millis: Long, value: Int): Long = {
    var _value: Int = value
    FieldUtils.verifyValueBounds(this, _value, iMinValue, getMaximumValue)
    if (_value <= iSkip) {
      _value -= 1
    }
    super.set(millis, _value)
  }

  override def getMinimumValue(): Int = iMinValue

  private def readResolve(): AnyRef = getType.getField(iChronology)
}
