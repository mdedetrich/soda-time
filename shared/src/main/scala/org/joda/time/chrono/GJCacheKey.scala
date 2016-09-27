package org.joda.time.chrono

import org.joda.time.DateTimeZone
import org.joda.time.Instant

class GJCacheKey(private val zone: DateTimeZone, private val cutoverInstant: Instant, private val minDaysInFirstWeek: Int) {

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result +
      (if ((cutoverInstant == null)) 0 else cutoverInstant.hashCode)
    result = prime * result + minDaysInFirstWeek
    result = prime * result + (if ((zone == null)) 0 else zone.hashCode)
    result
  }

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    }
    if (obj == null) {
      return false
    }
    if (!(obj.isInstanceOf[GJCacheKey])) {
      return false
    }
    val other = obj.asInstanceOf[GJCacheKey]
    if (cutoverInstant == null) {
      if (other.cutoverInstant != null) {
        return false
      }
    } else if (cutoverInstant != other.cutoverInstant) {
      return false
    }
    if (minDaysInFirstWeek != other.minDaysInFirstWeek) {
      return false
    }
    if (zone == null) {
      if (other.zone != null) {
        return false
      }
    } else if (zone != other.zone) {
      return false
    }
    true
  }
}
