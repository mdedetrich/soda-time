package org.joda.time.tz

import java.util.Collections
import java.util.Set
import org.joda.time.DateTimeZone
import UTCProvider._

object UTCProvider {

  private val AVAILABLE_IDS = Collections.singleton("UTC")
}

class UTCProvider extends Provider() {

  def getZone(id: String): DateTimeZone = {
    if ("UTC".equalsIgnoreCase(id)) {
      return DateTimeZone.UTC
    }
    null
  }

  def getAvailableIDs(): Set[String] = AVAILABLE_IDS
}
