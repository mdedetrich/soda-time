package org.joda.time

import java.io.File
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import java.util.Collections
import java.util.HashMap
import java.util.Locale
import java.util.Map
import java.util.Set
import java.util.TimeZone
import java.util.concurrent.atomic.AtomicReference
import org.joda.convert.{ToString, FromString}
import org.joda.time.chrono.BaseChronology
import org.joda.time.field.FieldUtils
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.DateTimeFormatterBuilder
import org.joda.time.format.FormatUtils
import org.joda.time.tz.DefaultNameProvider
import org.joda.time.tz.FixedDateTimeZone
import org.joda.time.tz.NameProvider
import org.joda.time.tz.Provider
import org.joda.time.tz.UTCProvider
import org.joda.time.tz.ZoneInfoProvider
import DateTimeZone._

object DateTimeZone {

  val UTC = UTCDateTimeZone.INSTANCE

  private val MAX_MILLIS = (86400 * 1000) - 1
  private val cProvider = new AtomicReference[Provider]()
  private val cNameProvider = new AtomicReference[NameProvider]()
  private val cDefault = new AtomicReference[DateTimeZone]()

  def getDefault(): DateTimeZone = {
    var zone = cDefault.get
    if (zone == null) {
      try {
        try {
          val id = System.getProperty("user.timezone")
          if (id != null) {
            zone = forID(id)
          }
        } catch {
          case ex: RuntimeException =>
        }
        if (zone == null) {
          zone = forTimeZone(TimeZone.getDefault)
        }
      } catch {
        case ex: IllegalArgumentException =>
      }
      if (zone == null) {
        zone = UTC
      }
      if (!cDefault.compareAndSet(null, zone)) {
        zone = cDefault.get
      }
    }
    zone
  }

  def setDefault(zone: DateTimeZone) {
    val sm = System.getSecurityManager
    if (sm != null) {
      sm.checkPermission(new JodaTimePermission("DateTimeZone.setDefault"))
    }
    if (zone == null) {
      throw new IllegalArgumentException("The datetime zone must not be null")
    }
    cDefault.set(zone)
  }

  @FromString
  def forID(id: String): DateTimeZone = {
    var _id: String = id
    if (_id == null) {
      return getDefault
    }
    if (_id == "UTC") {
      return DateTimeZone.UTC
    }
    val zone = getProvider.getZone(_id)
    if (zone != null) {
      return zone
    }
    if (_id.startsWith("+") || _id.startsWith("-")) {
      val offset = parseOffset(_id)
      if (offset == 0L) {
        return DateTimeZone.UTC
      } else {
        _id = printOffset(offset)
        return fixedOffsetZone(_id, offset)
      }
    }
    throw new IllegalArgumentException("The datetime zone id '" + _id + "' is not recognised")
  }

  def forOffsetHours(hoursOffset: Int): DateTimeZone = forOffsetHoursMinutes(hoursOffset, 0)

  def forOffsetHoursMinutes(hoursOffset: Int, minutesOffset: Int): DateTimeZone = {
    var _minutesOffset: Int = minutesOffset
    if (hoursOffset == 0 && _minutesOffset == 0) {
      return DateTimeZone.UTC
    }
    if (hoursOffset < -23 || hoursOffset > 23) {
      throw new IllegalArgumentException("Hours out of range: " + hoursOffset)
    }
    if (_minutesOffset < -59 || _minutesOffset > 59) {
      throw new IllegalArgumentException("Minutes out of range: " + _minutesOffset)
    }
    if (hoursOffset > 0 && _minutesOffset < 0) {
      throw new IllegalArgumentException("Positive hours must not have negative minutes: " + _minutesOffset)
    }
    var offset = 0
    val hoursInMinutes = hoursOffset * 60
    _minutesOffset = if (hoursInMinutes < 0) hoursInMinutes - Math.abs(_minutesOffset) else hoursInMinutes + _minutesOffset
    offset = FieldUtils.safeMultiply(_minutesOffset, DateTimeConstants.MILLIS_PER_MINUTE)
    forOffsetMillis(offset)
  }

  def forOffsetMillis(millisOffset: Int): DateTimeZone = {
    if (millisOffset < -MAX_MILLIS || millisOffset > MAX_MILLIS) {
      throw new IllegalArgumentException("Millis out of range: " + millisOffset)
    }
    val id = printOffset(millisOffset)
    fixedOffsetZone(id, millisOffset)
  }

  def forTimeZone(zone: TimeZone): DateTimeZone = {
    if (zone == null) {
      return getDefault
    }
    val id = zone.getID
    if (id == null) {
      throw new IllegalArgumentException("The TimeZone id must not be null")
    }
    if (id == "UTC") {
      return DateTimeZone.UTC
    }
    var dtz: DateTimeZone = null
    var convId = getConvertedId(id)
    val provider = getProvider
    if (convId != null) {
      dtz = provider.getZone(convId)
    }
    if (dtz == null) {
      dtz = provider.getZone(id)
    }
    if (dtz != null) {
      return dtz
    }
    if (convId == null) {
      convId = id
      if (convId.startsWith("GMT+") || convId.startsWith("GMT-")) {
        convId = convId.substring(3)
        val offset = parseOffset(convId)
        if (offset == 0L) {
          return DateTimeZone.UTC
        } else {
          convId = printOffset(offset)
          return fixedOffsetZone(convId, offset)
        }
      }
    }
    throw new IllegalArgumentException("The datetime zone id '" + id + "' is not recognised")
  }

  private def fixedOffsetZone(id: String, offset: Int): DateTimeZone = {
    if (offset == 0) {
      return DateTimeZone.UTC
    }
    new FixedDateTimeZone(id, null, offset, offset)
  }

  def getAvailableIDs(): Set[String] = getProvider.getAvailableIDs

  def getProvider(): Provider = {
    var provider = cProvider.get
    if (provider == null) {
      provider = getDefaultProvider
      if (!cProvider.compareAndSet(null, provider)) {
        provider = cProvider.get
      }
    }
    provider
  }

  def setProvider(provider: Provider) {
    var _provider: Provider = provider
    val sm = System.getSecurityManager
    if (sm != null) {
      sm.checkPermission(new JodaTimePermission("DateTimeZone.setProvider"))
    }
    if (_provider == null) {
      _provider = getDefaultProvider
    } else {
      validateProvider(_provider)
    }
    cProvider.set(_provider)
  }

  private def validateProvider(provider: Provider): Provider = {
    val ids = provider.getAvailableIDs
    if (ids == null || ids.size == 0) {
      throw new IllegalArgumentException("The provider doesn't have any available ids")
    }
    if (!ids.contains("UTC")) {
      throw new IllegalArgumentException("The provider doesn't support UTC")
    }
    if (UTC != provider.getZone("UTC")) {
      throw new IllegalArgumentException("Invalid UTC zone provided")
    }
    provider
  }

  private def getDefaultProvider(): Provider = {
    try {
      val providerClass = System.getProperty("org.joda.time.DateTimeZone.Provider")
      if (providerClass != null) {
        val provider = Class.forName(providerClass).newInstance().asInstanceOf[Provider]
        return validateProvider(provider)
      }
    } catch {
      case ex: SecurityException =>
    }
    try {
      val dataFolder = System.getProperty("org.joda.time.DateTimeZone.Folder")
      if (dataFolder != null) {
        val provider = new ZoneInfoProvider(new File(dataFolder))
        return validateProvider(provider)
      }
    } catch {
      case ex: SecurityException =>
    }
    try {
      val provider = new ZoneInfoProvider("org/joda/time/tz/data")
      return validateProvider(provider)
    } catch {
      case ex: Exception => ex.printStackTrace()
    }
    new UTCProvider()
  }

  def getNameProvider(): NameProvider = {
    var nameProvider = cNameProvider.get
    if (nameProvider == null) {
      nameProvider = getDefaultNameProvider
      if (!cNameProvider.compareAndSet(null, nameProvider)) {
        nameProvider = cNameProvider.get
      }
    }
    nameProvider
  }

  def setNameProvider(nameProvider: NameProvider) {
    var _nameProvider: NameProvider = nameProvider
    val sm = System.getSecurityManager
    if (sm != null) {
      sm.checkPermission(new JodaTimePermission("DateTimeZone.setNameProvider"))
    }
    if (_nameProvider == null) {
      _nameProvider = getDefaultNameProvider
    }
    cNameProvider.set(_nameProvider)
  }

  private def getDefaultNameProvider(): NameProvider = {
    var nameProvider: NameProvider = null
    try {
      val providerClass = System.getProperty("org.joda.time.DateTimeZone.NameProvider")
      if (providerClass != null) {
        nameProvider = Class.forName(providerClass).newInstance().asInstanceOf[NameProvider]
      }
    } catch {
      case ex: SecurityException =>
    }
    if (nameProvider == null) {
      nameProvider = new DefaultNameProvider()
    }
    nameProvider
  }

  private def getConvertedId(id: String): String = LazyInit.CONVERSION_MAP.get(id)

  private def parseOffset(str: String): Int = {
    -LazyInit.OFFSET_FORMATTER.parseMillis(str).toInt
  }

  private def printOffset(offset: Int): String = {
    var _offset: Int = offset
    val buf = new StringBuffer()
    if (_offset >= 0) {
      buf.append('+')
    } else {
      buf.append('-')
      _offset = -_offset
    }
    val hours = _offset / DateTimeConstants.MILLIS_PER_HOUR
    FormatUtils.appendPaddedInteger(buf, hours, 2)
    _offset -= hours * DateTimeConstants.MILLIS_PER_HOUR.toInt
    val minutes = _offset / DateTimeConstants.MILLIS_PER_MINUTE
    buf.append(':')
    FormatUtils.appendPaddedInteger(buf, minutes, 2)
    _offset -= minutes * DateTimeConstants.MILLIS_PER_MINUTE
    if (_offset == 0) {
      return buf.toString
    }
    val seconds = _offset / DateTimeConstants.MILLIS_PER_SECOND
    buf.append(':')
    FormatUtils.appendPaddedInteger(buf, seconds, 2)
    _offset -= seconds * DateTimeConstants.MILLIS_PER_SECOND
    if (_offset == 0) {
      return buf.toString
    }
    buf.append('.')
    FormatUtils.appendPaddedInteger(buf, _offset, 3)
    buf.toString
  }

  @SerialVersionUID(-6471952376487863581L)
  private class Stub(@transient private var iID: String) extends Serializable {

    private def writeObject(out: ObjectOutputStream) {
      out.writeUTF(iID)
    }

    private def readObject(in: ObjectInputStream) {
      iID = in.readUTF()
    }

    private def readResolve(): AnyRef = forID(iID)
  }

  object LazyInit {

    val CONVERSION_MAP = buildMap()

    val OFFSET_FORMATTER = buildFormatter()

    private def buildFormatter(): DateTimeFormatter = {
      @SerialVersionUID(-3128740902654445468L)
      val chrono = new BaseChronology() {

        def getZone(): DateTimeZone = null

        def withUTC(): Chronology = this

        def withZone(zone: DateTimeZone): Chronology = this

        override def toString(): String = getClass.getName
      }
      new DateTimeFormatterBuilder().appendTimeZoneOffset(null, showSeparators = true, 2, 4)
        .toFormatter()
        .withChronology(chrono)
    }

    private def buildMap(): Map[String, String] = {
      val map = new HashMap[String, String]()
      map.put("GMT", "UTC")
      map.put("WET", "WET")
      map.put("CET", "CET")
      map.put("MET", "CET")
      map.put("ECT", "CET")
      map.put("EET", "EET")
      map.put("MIT", "Pacific/Apia")
      map.put("HST", "Pacific/Honolulu")
      map.put("AST", "America/Anchorage")
      map.put("PST", "America/Los_Angeles")
      map.put("MST", "America/Denver")
      map.put("PNT", "America/Phoenix")
      map.put("CST", "America/Chicago")
      map.put("EST", "America/New_York")
      map.put("IET", "America/Indiana/Indianapolis")
      map.put("PRT", "America/Puerto_Rico")
      map.put("CNT", "America/St_Johns")
      map.put("AGT", "America/Argentina/Buenos_Aires")
      map.put("BET", "America/Sao_Paulo")
      map.put("ART", "Africa/Cairo")
      map.put("CAT", "Africa/Harare")
      map.put("EAT", "Africa/Addis_Ababa")
      map.put("NET", "Asia/Yerevan")
      map.put("PLT", "Asia/Karachi")
      map.put("IST", "Asia/Kolkata")
      map.put("BST", "Asia/Dhaka")
      map.put("VST", "Asia/Ho_Chi_Minh")
      map.put("CTT", "Asia/Shanghai")
      map.put("JST", "Asia/Tokyo")
      map.put("ACT", "Australia/Darwin")
      map.put("AET", "Australia/Sydney")
      map.put("SST", "Pacific/Guadalcanal")
      map.put("NST", "Pacific/Auckland")
      Collections.unmodifiableMap(map)
    }
  }
}

@SerialVersionUID(5546345482340108586L)
abstract class DateTimeZone protected (private val id: String) extends Serializable {
  private var iID: String = null

  if (id == null) {
    throw new IllegalArgumentException("Id must not be null")
  }

  iID = id
  
  @ToString
  def getID(): String = iID

  def getNameKey(instant: Long): String

  def getShortName(instant: Long): String = getShortName(instant, null)

  def getShortName(instant: Long, locale: Locale): String = {
    var _locale: Locale = locale
    if (_locale == null) {
      _locale = Locale.getDefault
    }
    val nameKey = getNameKey(instant)
    if (nameKey == null) {
      return iID
    }
    var name: String = null
    val np = getNameProvider
    name = if (np.isInstanceOf[DefaultNameProvider]) np.asInstanceOf[DefaultNameProvider].getShortName(_locale,
      iID, nameKey, isStandardOffset(instant)) else np.getShortName(_locale, iID, nameKey)
    if (name != null) {
      return name
    }
    printOffset(getOffset(instant))
  }

  def getName(instant: Long): String = getName(instant, null)

  def getName(instant: Long, locale: Locale): String = {
    var _locale: Locale = locale
    if (_locale == null) {
      _locale = Locale.getDefault
    }
    val nameKey = getNameKey(instant)
    if (nameKey == null) {
      return iID
    }
    var name: String = null
    val np = getNameProvider
    name = if (np.isInstanceOf[DefaultNameProvider]) np.asInstanceOf[DefaultNameProvider].getName(_locale,
      iID, nameKey, isStandardOffset(instant)) else np.getName(_locale, iID, nameKey)
    if (name != null) {
      return name
    }
    printOffset(getOffset(instant))
  }

  def getOffset(instant: Long): Int

  def getOffset(instant: ReadableInstant): Int = {
    if (instant == null) {
      return getOffset(DateTimeUtils.currentTimeMillis())
    }
    getOffset(instant.getMillis)
  }

  def getStandardOffset(instant: Long): Int

  def isStandardOffset(instant: Long): Boolean = {
    getOffset(instant) == getStandardOffset(instant)
  }

  def getOffsetFromLocal(instantLocal: Long): Int = {
    val offsetLocal = getOffset(instantLocal)
    val instantAdjusted = instantLocal - offsetLocal
    val offsetAdjusted = getOffset(instantAdjusted)
    if (offsetLocal != offsetAdjusted) {
      if ((offsetLocal - offsetAdjusted) < 0) {
        var nextLocal = nextTransition(instantAdjusted)
        if (nextLocal == (instantLocal - offsetLocal)) {
          nextLocal = Long.MaxValue
        }
        var nextAdjusted = nextTransition(instantLocal - offsetAdjusted)
        if (nextAdjusted == (instantLocal - offsetAdjusted)) {
          nextAdjusted = Long.MaxValue
        }
        if (nextLocal != nextAdjusted) {
          return offsetLocal
        }
      }
    } else if (offsetLocal >= 0) {
      val prev = previousTransition(instantAdjusted)
      if (prev < instantAdjusted) {
        val offsetPrev = getOffset(prev)
        val diff = offsetPrev - offsetLocal
        if (instantAdjusted - prev <= diff) {
          return offsetPrev
        }
      }
    }
    offsetAdjusted
  }

  def convertUTCToLocal(instantUTC: Long): Long = {
    val offset = getOffset(instantUTC)
    val instantLocal = instantUTC + offset
    if ((instantUTC ^ instantLocal) < 0 && (instantUTC ^ offset) >= 0) {
      throw new ArithmeticException("Adding time zone offset caused overflow")
    }
    instantLocal
  }

  def convertLocalToUTC(instantLocal: Long, strict: Boolean, originalInstantUTC: Long): Long = {
    val offsetOriginal = getOffset(originalInstantUTC)
    val instantUTC = instantLocal - offsetOriginal
    val offsetLocalFromOriginal = getOffset(instantUTC)
    if (offsetLocalFromOriginal == offsetOriginal) {
      return instantUTC
    }
    convertLocalToUTC(instantLocal, strict)
  }

  def convertLocalToUTC(instantLocal: Long, strict: Boolean): Long = {
    val offsetLocal = getOffset(instantLocal)
    var offset = getOffset(instantLocal - offsetLocal)
    if (offsetLocal != offset) {
      if (strict || offsetLocal < 0) {
        var nextLocal = nextTransition(instantLocal - offsetLocal)
        if (nextLocal == (instantLocal - offsetLocal)) {
          nextLocal = Long.MaxValue
        }
        var nextAdjusted = nextTransition(instantLocal - offset)
        if (nextAdjusted == (instantLocal - offset)) {
          nextAdjusted = Long.MaxValue
        }
        if (nextLocal != nextAdjusted) {
          if (strict) {
            throw IllegalInstantException.create(instantLocal,getID)
          } else {
            offset = offsetLocal
          }
        }
      }
    }
    val instantUTC = instantLocal - offset
    if ((instantLocal ^ instantUTC) < 0 && (instantLocal ^ offset) < 0) {
      throw new ArithmeticException("Subtracting time zone offset caused overflow")
    }
    instantUTC
  }

  def getMillisKeepLocal(newZone: DateTimeZone, oldInstant: Long): Long = {
    var _newZone: DateTimeZone = newZone
    if (_newZone == null) {
      _newZone = DateTimeZone.getDefault
    }
    if (_newZone == this) {
      return oldInstant
    }
    val instantLocal = convertUTCToLocal(oldInstant)
    _newZone.convertLocalToUTC(instantLocal, strict = false, oldInstant)
  }

  def isLocalDateTimeGap(localDateTime: LocalDateTime): Boolean = {
    if (isFixed) {
      return false
    }
    try {
      localDateTime.toDateTime(this)
      false
    } catch {
      case ex: IllegalInstantException => true
    }
  }

  def adjustOffset(instant: Long, earlierOrLater: Boolean): Long = {
    val instantBefore = instant - 3 * DateTimeConstants.MILLIS_PER_HOUR
    val instantAfter = instant + 3 * DateTimeConstants.MILLIS_PER_HOUR
    val offsetBefore = getOffset(instantBefore)
    val offsetAfter = getOffset(instantAfter)
    if (offsetBefore <= offsetAfter) {
      return instant
    }
    val diff = offsetBefore - offsetAfter
    val transition = nextTransition(instantBefore)
    val overlapStart = transition - diff
    val overlapEnd = transition + diff
    if (instant < overlapStart || instant >= overlapEnd) {
      return instant
    }
    val afterStart = instant - overlapStart
    if (afterStart >= diff) {
      if (earlierOrLater) instant else instant - diff
    } else {
      if (earlierOrLater) instant + diff else instant
    }
  }

  def isFixed(): Boolean

  def nextTransition(instant: Long): Long

  def previousTransition(instant: Long): Long

  def toTimeZone(): java.util.TimeZone = TimeZone.getTimeZone(iID)

  override def equals(`object`: Any): Boolean

  override def hashCode(): Int = 57 + getID.hashCode

  override def toString(): String = getID

  protected def writeReplace(): AnyRef = new Stub(iID)
}
