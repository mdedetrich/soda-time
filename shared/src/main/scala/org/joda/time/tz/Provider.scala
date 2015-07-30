package org.joda.time.tz

import java.util.Set
import org.joda.time.DateTimeZone

trait Provider {

  def getZone(id: String): DateTimeZone

  def getAvailableIDs(): Set[String]
}
