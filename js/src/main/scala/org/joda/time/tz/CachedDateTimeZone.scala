package org.joda.time.tz

import org.joda.time.DateTimeZone
import CachedDateTimeZone._
import scala.util.control.Breaks._

object CachedDateTimeZone {
  var cacheSize: Int = 0

  private val cInfoCacheMask = cacheSize - 1

  var i: Integer = null

  try {
    i = Integer.getInteger("org.joda.time.tz.CachedDateTimeZone.size")
  } catch {
    case e: SecurityException => i = null
  }

  if (i == null) {
    cacheSize = 512
  } else {
    cacheSize = i.intValue()
    cacheSize -= 1
    var shift = 0
    while (cacheSize > 0) {
      shift += 1
      cacheSize >>= 1
    }
    cacheSize = 1 << shift
  }

  def forZone(zone: DateTimeZone): CachedDateTimeZone = {
    if (zone.isInstanceOf[CachedDateTimeZone]) {
      return zone.asInstanceOf[CachedDateTimeZone]
    }
    new CachedDateTimeZone(zone)
  }

  private class Info(val iZoneRef: DateTimeZone, val iPeriodStart: Long) {

    var iNextInfo: Info = null

    private var iNameKey: String = null
    private var iOffset: Int = Integer.MIN_VALUE
    private var iStandardOffset: Int = Integer.MIN_VALUE

    def getNameKey(millis: Long): String = {
      if (iNextInfo == null || millis < iNextInfo.iPeriodStart) {
        if (iNameKey == null) {
          iNameKey = iZoneRef.getNameKey(iPeriodStart)
        }
        return iNameKey
      }
      iNextInfo.getNameKey(millis)
    }

    def getOffset(millis: Long): Int = {
      if (iNextInfo == null || millis < iNextInfo.iPeriodStart) {
        if (iOffset == Integer.MIN_VALUE) {
          iOffset = iZoneRef.getOffset(iPeriodStart)
        }
        return iOffset
      }
      iNextInfo.getOffset(millis)
    }

    def getStandardOffset(millis: Long): Int = {
      if (iNextInfo == null || millis < iNextInfo.iPeriodStart) {
        if (iStandardOffset == Integer.MIN_VALUE) {
          iStandardOffset = iZoneRef.getStandardOffset(iPeriodStart)
        }
        return iStandardOffset
      }
      iNextInfo.getStandardOffset(millis)
    }
  }
}

@SerialVersionUID(5472298452022250685L)
class CachedDateTimeZone private (private val zone: DateTimeZone)
    extends DateTimeZone(zone.getID) {
  import scala.scalajs.js

  var iZone: DateTimeZone = null

  iZone = zone

  @transient private val iInfoCache = new js.Array[Info](cInfoCacheMask + 1)

  def getUncachedZone(): DateTimeZone = iZone

  def getNameKey(instant: Long): String = getInfo(instant).getNameKey(instant)

  def getOffset(instant: Long): Int = getInfo(instant).getOffset(instant)

  def getStandardOffset(instant: Long): Int = {
    getInfo(instant).getStandardOffset(instant)
  }

  def isFixed(): Boolean = iZone.isFixed

  def nextTransition(instant: Long): Long = iZone.nextTransition(instant)

  def previousTransition(instant: Long): Long =
    iZone.previousTransition(instant)

  override def hashCode(): Int = iZone.hashCode

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    }
    if (obj.isInstanceOf[CachedDateTimeZone]) {
      return iZone == obj.asInstanceOf[CachedDateTimeZone].iZone
    }
    false
  }

  private def getInfo(millis: Long): Info = {
    val period = (millis >> 32).toInt
    val cache = iInfoCache
    val index = period & cInfoCacheMask
    var info = cache(index)
    if (info == null || (info.iPeriodStart >> 32).toInt != period) {
      info = createInfo(millis)
      cache(index) = info
    }
    info
  }

  private def createInfo(millis: Long): Info = {
    var periodStart = millis & (0xffffffffL << 32)
    val info = new Info(iZone, periodStart)
    val end = periodStart | 0xffffffffL
    var chain = info
    while (true) {
      val next = iZone.nextTransition(periodStart)
      if (next == periodStart || next > end) {
        break()
      }
      periodStart = next
      val newInfo = new Info(iZone, periodStart)
      chain = newInfo
      chain.iNextInfo = newInfo
    }
    info
  }
}
