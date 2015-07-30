package org.joda.time.tz

import org.joda.time.DateTimeZone

@SerialVersionUID(-3513011772763289092L)
class FixedDateTimeZone(id: String,
                        private val iNameKey: String,
                        private val iWallOffset: Int,
                        private val iStandardOffset: Int) extends DateTimeZone(id) {

  def getNameKey(instant: Long): String = iNameKey

  def getOffset(instant: Long): Int = iWallOffset

  def getStandardOffset(instant: Long): Int = iStandardOffset

  override def getOffsetFromLocal(instantLocal: Long): Int = iWallOffset

  def isFixed(): Boolean = true

  def nextTransition(instant: Long): Long = instant

  def previousTransition(instant: Long): Long = instant

  override def toTimeZone(): java.util.TimeZone = {
    val id = getID
    if (id.length == 6 && (id.startsWith("+") || id.startsWith("-"))) {
      return java.util.TimeZone.getTimeZone("GMT" + getID)
    }
    new java.util.SimpleTimeZone(iWallOffset, getID)
  }

  override def equals(obj: Any): Boolean = {
    if (this == obj) {
      return true
    }
    if (obj.isInstanceOf[FixedDateTimeZone]) {
      val other = obj.asInstanceOf[FixedDateTimeZone]
      return getID == other.getID && iStandardOffset == other.iStandardOffset &&
        iWallOffset == other.iWallOffset
    }
    false
  }

  override def hashCode(): Int = {
    getID.hashCode + 37 * iStandardOffset + 31 * iWallOffset
  }
}
