package org.joda.time

import java.text.DateFormatSymbols
import java.util.Collections
import java.util.HashMap
import java.util.LinkedHashMap
import java.util.Locale
import java.util.Map
import java.util.concurrent.atomic.AtomicReference
import org.joda.time.chrono.ISOChronology

object DateTimeUtils {

  private val SYSTEM_MILLIS_PROVIDER = new SystemMillisProvider()

  @volatile private var cMillisProvider: MillisProvider =
    SYSTEM_MILLIS_PROVIDER

  private val cZoneNames = new AtomicReference[Map[String, DateTimeZone]]()

  def currentTimeMillis(): Long = cMillisProvider.getMillis

  def setCurrentMillisSystem() {
    checkPermission()
    cMillisProvider = SYSTEM_MILLIS_PROVIDER
  }

  def setCurrentMillisFixed(fixedMillis: Long) {
    checkPermission()
    cMillisProvider = new FixedMillisProvider(fixedMillis)
  }

  def setCurrentMillisOffset(offsetMillis: Long) {
    checkPermission()
    cMillisProvider =
      if (offsetMillis == 0) SYSTEM_MILLIS_PROVIDER
      else new OffsetMillisProvider(offsetMillis)
  }

  def setCurrentMillisProvider(millisProvider: MillisProvider) {
    if (millisProvider == null) {
      throw new IllegalArgumentException("The MillisProvider must not be null")
    }
    checkPermission()
    cMillisProvider = millisProvider
  }

  private def checkPermission() {
    val sm = System.getSecurityManager
    if (sm != null) {
      sm.checkPermission(new JodaTimePermission("CurrentTime.setProvider"))
    }
  }

  def getInstantMillis(instant: ReadableInstant): Long = {
    if (instant == null) {
      return DateTimeUtils.currentTimeMillis()
    }
    instant.getMillis
  }

  def getInstantChronology(instant: ReadableInstant): Chronology = {
    if (instant == null) {
      return ISOChronology.getInstance
    }
    val chrono = instant.getChronology
    if (chrono == null) {
      return ISOChronology.getInstance
    }
    chrono
  }

  def getIntervalChronology(start: ReadableInstant,
                            end: ReadableInstant): Chronology = {
    var chrono: Chronology = null
    if (start != null) {
      chrono = start.getChronology
    } else if (end != null) {
      chrono = end.getChronology
    }
    if (chrono == null) {
      chrono = ISOChronology.getInstance
    }
    chrono
  }

  def getIntervalChronology(interval: ReadableInterval): Chronology = {
    if (interval == null) {
      return ISOChronology.getInstance
    }
    val chrono = interval.getChronology
    if (chrono == null) {
      return ISOChronology.getInstance
    }
    chrono
  }

  def getReadableInterval(interval: ReadableInterval): ReadableInterval = {
    var _interval: ReadableInterval = interval
    if (_interval == null) {
      val now = DateTimeUtils.currentTimeMillis()
      _interval = new Interval(now, now)
    }
    _interval
  }

  def getChronology(chrono: Chronology): Chronology = {
    if (chrono == null) {
      return ISOChronology.getInstance
    }
    chrono
  }

  def getZone(zone: DateTimeZone): DateTimeZone = {
    if (zone == null) {
      return DateTimeZone.getDefault
    }
    zone
  }

  def getPeriodType(`type`: PeriodType): PeriodType = {
    if (`type` == null) {
      return PeriodType.standard()
    }
    `type`
  }

  def getDurationMillis(duration: ReadableDuration): Long = {
    if (duration == null) {
      return 0L
    }
    duration.getMillis
  }

  def isContiguous(partial: ReadablePartial): Boolean = {
    if (partial == null) {
      throw new IllegalArgumentException("Partial must not be null")
    }
    var lastType: DurationFieldType = null
    for (i <- 0 until partial.size) {
      val loopField = partial.getField(i)
      if (i > 0) {
        if (loopField.getRangeDurationField == null ||
            loopField.getRangeDurationField.getType != lastType) {
          return false
        }
      }
      lastType = loopField.getDurationField.getType
    }
    true
  }

  def getDateFormatSymbols(locale: Locale): DateFormatSymbols = {
    try {
      DateFormatSymbols.getInstance(locale)
    } catch {
      case ex: Exception => new DateFormatSymbols(locale)
    }
  }

  def getDefaultTimeZoneNames(): Map[String, DateTimeZone] = {
    var names = cZoneNames.get
    if (names == null) {
      names = buildDefaultTimeZoneNames()
      if (!cZoneNames.compareAndSet(null, names)) {
        names = cZoneNames.get
      }
    }
    names
  }

  def setDefaultTimeZoneNames(names: Map[String, DateTimeZone]) {
    cZoneNames.set(
      Collections.unmodifiableMap(new HashMap[String, DateTimeZone](names)))
  }

  private def buildDefaultTimeZoneNames(): Map[String, DateTimeZone] = {
    val map = new LinkedHashMap[String, DateTimeZone]()
    map.put("UT", DateTimeZone.UTC)
    map.put("UTC", DateTimeZone.UTC)
    map.put("GMT", DateTimeZone.UTC)
    put(map, "EST", "America/New_York")
    put(map, "EDT", "America/New_York")
    put(map, "CST", "America/Chicago")
    put(map, "CDT", "America/Chicago")
    put(map, "MST", "America/Denver")
    put(map, "MDT", "America/Denver")
    put(map, "PST", "America/Los_Angeles")
    put(map, "PDT", "America/Los_Angeles")
    Collections.unmodifiableMap(map)
  }

  private def put(map: Map[String, DateTimeZone], name: String, id: String) {
    try {
      map.put(name, DateTimeZone.forID(id))
    } catch {
      case ex: RuntimeException =>
    }
  }

  def toJulianDay(epochMillis: Long): Double = {
    val epochDay = epochMillis / 86400000d
    epochDay + 2440587.5d
  }

  def toJulianDayNumber(epochMillis: Long): Long = {
    Math.floor(toJulianDay(epochMillis) + 0.5d).toLong
  }

  def fromJulianDay(julianDay: Double): Long = {
    val epochDay = julianDay - 2440587.5d
    (epochDay * 86400000d).toLong
  }

  trait MillisProvider {
    def getMillis(): Long
  }

  class SystemMillisProvider extends MillisProvider {
    def getMillis(): Long = System.currentTimeMillis()
  }

  class FixedMillisProvider(private val iMillis: Long) extends MillisProvider {
    def getMillis(): Long = iMillis
  }

  class OffsetMillisProvider(private val iMillis: Long)
      extends MillisProvider {
    def getMillis(): Long = System.currentTimeMillis() + iMillis
  }
}

class DateTimeUtils protected ()
