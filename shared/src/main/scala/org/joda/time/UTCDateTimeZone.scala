package org.joda.time

object UTCDateTimeZone {
  val INSTANCE = new UTCDateTimeZone()
}

@SerialVersionUID(-3513011772763289092L)
class UTCDateTimeZone extends DateTimeZone("UTC") {

  override def getNameKey(instant: Long): String = "UTC"

  override def getOffset(instant: Long): Int = 0

  override def getStandardOffset(instant: Long): Int = 0

  override def getOffsetFromLocal(instantLocal: Long): Int = 0

  override def isFixed(): Boolean = true

  override def nextTransition(instant: Long): Long = instant

  override def previousTransition(instant: Long): Long = instant

  override def toTimeZone(): java.util.TimeZone = new java.util.SimpleTimeZone(0, getID)

  override def equals(obj: Any): Boolean = obj.isInstanceOf[UTCDateTimeZone]

  override def hashCode(): Int = getID.hashCode
}
