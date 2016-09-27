package org.joda.time

import org.joda.time.format.DateTimeFormat

object IllegalInstantException {

  def create(message: String) = new IllegalInstantException(message)
  def create(instantLocal: Long, zoneId: String) =
    new IllegalInstantException(createMessage(instantLocal, zoneId))

  private def createMessage(instantLocal: Long, zoneId: String): String = {
    val localDateTime = DateTimeFormat
      .forPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
      .print(new Instant(instantLocal))
    val zone = if (zoneId != null) " (" + zoneId + ")" else ""
    "Illegal instant due to time zone offset transition (daylight savings time 'gap'): " +
      localDateTime +
      zone
  }

  def isIllegalInstant(ex: Throwable): Boolean = {
    if (ex.isInstanceOf[IllegalInstantException]) {
      return true
    }
    while (ex.getCause != null && ex.getCause != ex) {
      return isIllegalInstant(ex.getCause)
    }
    false
  }
}

@SerialVersionUID(2858712538216L)
class IllegalInstantException(message: String)
    extends IllegalArgumentException(message)
