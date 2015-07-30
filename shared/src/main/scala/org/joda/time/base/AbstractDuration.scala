package org.joda.time.base

import org.joda.convert.ToString
import org.joda.time.Duration
import org.joda.time.Period
import org.joda.time.ReadableDuration
import org.joda.time.format.FormatUtils

abstract class AbstractDuration extends ReadableDuration() {

  def toDuration(): Duration = new Duration(getMillis)

  def toPeriod(): Period = new Period(getMillis)

  def compareTo(other: ReadableDuration): Int = {
    val thisMillis = this.getMillis
    val otherMillis = other.getMillis
    if (thisMillis < otherMillis) {
      return -1
    }
    if (thisMillis > otherMillis) {
      return 1
    }
    0
  }

  def isEqual(duration: ReadableDuration): Boolean = {
    var _duration: ReadableDuration = duration
    if (_duration == null) {
      _duration = Duration.ZERO
    }
    compareTo(_duration) == 0
  }

  def isLongerThan(duration: ReadableDuration): Boolean = {
    var _duration: ReadableDuration = duration
    if (_duration == null) {
      _duration = Duration.ZERO
    }
    compareTo(_duration) > 0
  }

  def isShorterThan(duration: ReadableDuration): Boolean = {
    var _duration: ReadableDuration = duration
    if (_duration == null) {
      _duration = Duration.ZERO
    }
    compareTo(_duration) < 0
  }

  override def equals(duration: Any): Boolean = {
    if (this == duration) {
      return true
    }
    if (duration.isInstanceOf[ReadableDuration] == false) {
      return false
    }
    val other = duration.asInstanceOf[ReadableDuration]
    (getMillis == other.getMillis)
  }

  override def hashCode(): Int = {
    val len = getMillis
    (len ^ (len >>> 32)).toInt
  }

  @ToString
  override def toString(): String = {
    val millis = getMillis
    val buf = new StringBuffer()
    buf.append("PT")
    val negative = (millis < 0)
    FormatUtils.appendUnpaddedInteger(buf, millis)
    while (buf.length < (if (negative) 7 else 6)) {
      buf.insert(if (negative) 3 else 2, "0")
    }
    if ((millis / 1000) * 1000 == millis) {
      buf.setLength(buf.length - 3)
    } else {
      buf.insert(buf.length - 3, ".")
    }
    buf.append('S')
    buf.toString
  }
}
