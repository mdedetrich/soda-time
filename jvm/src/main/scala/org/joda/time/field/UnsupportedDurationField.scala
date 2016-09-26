package org.joda.time.field

import java.io.Serializable
import java.util.HashMap
import org.joda.time.DurationField
import org.joda.time.DurationFieldType
import UnsupportedDurationField._

object UnsupportedDurationField {

  private var cCache: HashMap[DurationFieldType, UnsupportedDurationField] = _

  def getInstance(`type`: DurationFieldType): UnsupportedDurationField = {
    synchronized {
      var field: UnsupportedDurationField = null
      if (cCache == null) {
        cCache = new HashMap[DurationFieldType, UnsupportedDurationField](7)
        field = null
      } else {
        field = cCache.get(`type`)
      }
      if (field == null) {
        field = new UnsupportedDurationField(`type`)
        cCache.put(`type`, field)
      }
      field
    }
  }
}

@SerialVersionUID(-6390301302770925357L)
class UnsupportedDurationField private (private val iType: DurationFieldType)
  extends DurationField with Serializable {

  def getType(): DurationFieldType = iType

  def getName(): String = iType.getName

  def isSupported(): Boolean = false

  def isPrecise(): Boolean = true

  def getValue(duration: Long): Int = throw unsupported()

  def getValueAsLong(duration: Long): Long = throw unsupported()

  def getValue(duration: Long, instant: Long): Int = throw unsupported()

  def getValueAsLong(duration: Long, instant: Long): Long = throw unsupported()

  def getMillis(value: Int): Long = throw unsupported()

  def getMillis(value: Long): Long = throw unsupported()

  def getMillis(value: Int, instant: Long): Long = throw unsupported()

  def getMillis(value: Long, instant: Long): Long = throw unsupported()

  def add(instant: Long, value: Int): Long = throw unsupported()

  def add(instant: Long, value: Long): Long = throw unsupported()

  def getDifference(minuendInstant: Long, subtrahendInstant: Long): Int = throw unsupported()

  def getDifferenceAsLong(minuendInstant: Long, subtrahendInstant: Long): Long = throw unsupported()

  def getUnitMillis(): Long = 0

  def compareTo(durationField: DurationField): Int = 0

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    } else if (obj.isInstanceOf[UnsupportedDurationField]) {
      val other = obj.asInstanceOf[UnsupportedDurationField]
      if (other.getName == null) {
        return getName == null
      }
      return other.getName == getName
    }
    false
  }

  override def hashCode(): Int = getName.hashCode

  override def toString(): String = {
    "UnsupportedDurationField[" + getName + ']'
  }

  private def readResolve(): AnyRef = getInstance(iType)

  private def unsupported(): UnsupportedOperationException = {
    new UnsupportedOperationException(iType + " field is unsupported")
  }
}
