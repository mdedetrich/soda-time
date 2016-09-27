package org.joda.time.format

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReferenceArray
import org.joda.time.Chronology
import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.ReadablePartial
import scala.util.control.Breaks._

object DateTimeFormat {

  val FULL = 0
  val LONG = 1
  val MEDIUM = 2
  val SHORT = 3
  val NONE = 4
  val DATE = 0
  val TIME = 1
  val DATETIME = 2

  private val PATTERN_CACHE_SIZE = 500

  private val cPatternCache =
    new ConcurrentHashMap[String, DateTimeFormatter]()

  private val cStyleCache = new AtomicReferenceArray[DateTimeFormatter](25)

  def forPattern(pattern: String): DateTimeFormatter =
    createFormatterForPattern(pattern)

  def forStyle(style: String): DateTimeFormatter =
    createFormatterForStyle(style)

  def patternForStyle(style: String, locale: Locale): String = {
    var _locale: Locale = locale
    val formatter = createFormatterForStyle(style)
    if (_locale == null) {
      _locale = Locale.getDefault
    }
    formatter.getPrinter0.asInstanceOf[StyleFormatter].getPattern(_locale)
  }

  def shortDate(): DateTimeFormatter = {
    createFormatterForStyleIndex(SHORT, NONE)
  }

  def shortTime(): DateTimeFormatter = {
    createFormatterForStyleIndex(NONE, SHORT)
  }

  def shortDateTime(): DateTimeFormatter = {
    createFormatterForStyleIndex(SHORT, SHORT)
  }

  def mediumDate(): DateTimeFormatter = {
    createFormatterForStyleIndex(MEDIUM, NONE)
  }

  def mediumTime(): DateTimeFormatter = {
    createFormatterForStyleIndex(NONE, MEDIUM)
  }

  def mediumDateTime(): DateTimeFormatter = {
    createFormatterForStyleIndex(MEDIUM, MEDIUM)
  }

  def longDate(): DateTimeFormatter = {
    createFormatterForStyleIndex(LONG, NONE)
  }

  def longTime(): DateTimeFormatter = {
    createFormatterForStyleIndex(NONE, LONG)
  }

  def longDateTime(): DateTimeFormatter = {
    createFormatterForStyleIndex(LONG, LONG)
  }

  def fullDate(): DateTimeFormatter = {
    createFormatterForStyleIndex(FULL, NONE)
  }

  def fullTime(): DateTimeFormatter = {
    createFormatterForStyleIndex(NONE, FULL)
  }

  def fullDateTime(): DateTimeFormatter = {
    createFormatterForStyleIndex(FULL, FULL)
  }

  def appendPatternTo(builder: DateTimeFormatterBuilder, pattern: String) {
    parsePatternTo(builder, pattern)
  }

  private def parsePatternTo(builder: DateTimeFormatterBuilder,
                             pattern: String) {
    val length = pattern.length
    val indexRef = Array.ofDim[Int](1)

    var i = 0
    while (i < length) {
      indexRef(0) = i
      val token = parseToken(pattern, indexRef)
      i = indexRef(0)
      val tokenLen = token.length
      if (tokenLen == 0) {
        break()
      }
      val c = token.charAt(0)
      c match {
        case 'G' => builder.appendEraText()
        case 'C' => builder.appendCenturyOfEra(tokenLen, tokenLen)
        case 'x' | 'y' | 'Y' =>
          if (tokenLen == 2) {
            var lenientParse = true
            if (i + 1 < length) {
              indexRef(0) += 1
              if (isNumericToken(parseToken(pattern, indexRef))) {
                lenientParse = false
              }
              indexRef(0) -= 1
            }
            c match {
              case 'x' =>
                builder.appendTwoDigitWeekyear(new DateTime().getWeekyear - 30,
                                               lenientParse)
              case 'y' | 'Y' | _ =>
                builder.appendTwoDigitYear(new DateTime().getYear - 30,
                                           lenientParse)
            }
          } else {
            var maxDigits = 9
            if (i + 1 < length) {
              indexRef(0) += 1
              if (isNumericToken(parseToken(pattern, indexRef))) {
                maxDigits = tokenLen
              }
              indexRef(0) -= 1
            }
            c match {
              case 'x' => builder.appendWeekyear(tokenLen, maxDigits)
              case 'y' => builder.appendYear(tokenLen, maxDigits)
              case 'Y' => builder.appendYearOfEra(tokenLen, maxDigits)
            }
          }
        case 'M' =>
          if (tokenLen >= 3) {
            if (tokenLen >= 4) {
              builder.appendMonthOfYearText()
            } else {
              builder.appendMonthOfYearShortText()
            }
          } else {
            builder.appendMonthOfYear(tokenLen)
          }
        case 'd' => builder.appendDayOfMonth(tokenLen)
        case 'a' => builder.appendHalfdayOfDayText()
        case 'h' => builder.appendClockhourOfHalfday(tokenLen)
        case 'H' => builder.appendHourOfDay(tokenLen)
        case 'k' => builder.appendClockhourOfDay(tokenLen)
        case 'K' => builder.appendHourOfHalfday(tokenLen)
        case 'm' => builder.appendMinuteOfHour(tokenLen)
        case 's' => builder.appendSecondOfMinute(tokenLen)
        case 'S' => builder.appendFractionOfSecond(tokenLen, tokenLen)
        case 'e' => builder.appendDayOfWeek(tokenLen)
        case 'E' =>
          if (tokenLen >= 4) {
            builder.appendDayOfWeekText()
          } else {
            builder.appendDayOfWeekShortText()
          }
        case 'D' => builder.appendDayOfYear(tokenLen)
        case 'w' => builder.appendWeekOfWeekyear(tokenLen)
        case 'z' =>
          if (tokenLen >= 4) {
            builder.appendTimeZoneName()
          } else {
            builder.appendTimeZoneShortName(null)
          }
        case 'Z' =>
          if (tokenLen == 1) {
            builder
              .appendTimeZoneOffset(null, "Z", showSeparators = false, 2, 2)
          } else if (tokenLen == 2) {
            builder
              .appendTimeZoneOffset(null, "Z", showSeparators = true, 2, 2)
          } else {
            builder.appendTimeZoneId()
          }
        case '\'' =>
          var sub = token.substring(1)
          if (sub.length == 1) {
            builder.appendLiteral(sub.charAt(0))
          } else {
            builder.appendLiteral(new String(sub))
          }

        case _ =>
          throw new IllegalArgumentException(
            "Illegal pattern component: " + token)
      }
      i = i + 1
    }
  }

  private def parseToken(pattern: String, indexRef: Array[Int]): String = {
    val buf = new StringBuilder()
    var i = indexRef(0)
    val length = pattern.length
    var c = pattern.charAt(i)
    if (c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z') {
      buf.append(c)
      while (i + 1 < length) {
        val peek = pattern.charAt(i + 1)
        if (peek == c) {
          buf.append(c)
          i += 1
        } else {
          break()
        }
      }
    } else {
      buf.append('\'')
      var inLiteral = false
      while (i < length) {
        c = pattern.charAt(i)
        if (c == '\'') {
          if (i + 1 < length && pattern.charAt(i + 1) == '\'') {
            i += 1
            buf.append(c)
          } else {
            inLiteral = !inLiteral
          }
        } else if (!inLiteral && (c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z')) {
          i -= 1
          break()
        } else {
          buf.append(c)
        }
        i += 1
      }
    }
    indexRef(0) = i
    buf.toString
  }

  private def isNumericToken(token: String): Boolean = {
    val tokenLen = token.length
    if (tokenLen > 0) {
      val c = token.charAt(0)
      c match {
        case 'c' | 'C' | 'x' | 'y' | 'Y' | 'd' | 'h' | 'H' | 'm' | 's' | 'S' |
            'e' | 'D' | 'F' | 'w' | 'W' | 'k' | 'K' =>
          return true
        case 'M' =>
          if (tokenLen <= 2) {
            return true
          }
      }
    }
    false
  }

  private def createFormatterForPattern(pattern: String): DateTimeFormatter = {
    if (pattern == null || pattern.length == 0) {
      throw new IllegalArgumentException("Invalid pattern specification")
    }
    var formatter = cPatternCache.get(pattern)
    if (formatter == null) {
      val builder = new DateTimeFormatterBuilder()
      parsePatternTo(builder, pattern)
      formatter = builder.toFormatter()
      if (cPatternCache.size < PATTERN_CACHE_SIZE) {
        val oldFormatter = cPatternCache.putIfAbsent(pattern, formatter)
        if (oldFormatter != null) {
          formatter = oldFormatter
        }
      }
    }
    formatter
  }

  private def createFormatterForStyle(style: String): DateTimeFormatter = {
    if (style == null || style.length != 2) {
      throw new IllegalArgumentException(
        "Invalid style specification: " + style)
    }
    val dateStyle = selectStyle(style.charAt(0))
    val timeStyle = selectStyle(style.charAt(1))
    if (dateStyle == NONE && timeStyle == NONE) {
      throw new IllegalArgumentException("Style '--' is invalid")
    }
    createFormatterForStyleIndex(dateStyle, timeStyle)
  }

  private def createFormatterForStyleIndex(
      dateStyle: Int,
      timeStyle: Int): DateTimeFormatter = {
    val index = ((dateStyle << 2) + dateStyle) + timeStyle
    if (index >= cStyleCache.length) {
      return createDateTimeFormatter(dateStyle, timeStyle)
    }
    var f = cStyleCache.get(index)
    if (f == null) {
      f = createDateTimeFormatter(dateStyle, timeStyle)
      if (cStyleCache.compareAndSet(index, null, f) == false) {
        f = cStyleCache.get(index)
      }
    }
    f
  }

  private def createDateTimeFormatter(dateStyle: Int,
                                      timeStyle: Int): DateTimeFormatter = {
    var `type` = DATETIME
    if (dateStyle == NONE) {
      `type` = TIME
    } else if (timeStyle == NONE) {
      `type` = DATE
    }
    val llf = new StyleFormatter(dateStyle, timeStyle, `type`)
    new DateTimeFormatter(llf, llf)
  }

  private def selectStyle(ch: Char): Int = ch match {
    case 'S' => SHORT
    case 'M' => MEDIUM
    case 'L' => LONG
    case 'F' => FULL
    case '-' => NONE
    case _ =>
      throw new IllegalArgumentException("Invalid style character: " + ch)
  }

  object StyleFormatter {

    private val cCache =
      new ConcurrentHashMap[StyleFormatterCacheKey, DateTimeFormatter]()
  }

  class StyleFormatter(private val iDateStyle: Int,
                       private val iTimeStyle: Int,
                       private val iType: Int)
      extends InternalPrinter()
      with InternalParser {
    import StyleFormatter._

    def estimatePrintedLength(): Int = 40

    def printTo(appenadble: Appendable,
                instant: Long,
                chrono: Chronology,
                displayOffset: Int,
                displayZone: DateTimeZone,
                locale: Locale) {
      val p = getFormatter(locale).getPrinter0
      p.printTo(appenadble,
                instant,
                chrono,
                displayOffset,
                displayZone,
                locale)
    }

    def printTo(appendable: Appendable,
                partial: ReadablePartial,
                locale: Locale) {
      val p = getFormatter(locale).getPrinter0
      p.printTo(appendable, partial, locale)
    }

    def estimateParsedLength(): Int = 40

    def parseInto(bucket: DateTimeParserBucket,
                  text: CharSequence,
                  position: Int): Int = {
      val p = getFormatter(bucket.getLocale).getParser0
      p.parseInto(bucket, text, position)
    }

    private def getFormatter(locale: Locale): DateTimeFormatter = {
      var _locale = locale
      _locale = (if (_locale == null) Locale.getDefault else _locale)
      val key =
        new StyleFormatterCacheKey(iType, iDateStyle, iTimeStyle, _locale)
      var f = cCache.get(key)
      if (f == null) {
        f = DateTimeFormat.forPattern(getPattern(_locale))
        val oldFormatter = cCache.putIfAbsent(key, f)
        if (oldFormatter != null) {
          f = oldFormatter
        }
      }
      f
    }

    def getPattern(locale: Locale): String = {
      var f: DateFormat = null
      iType match {
        case DATE => f = DateFormat.getDateInstance(iDateStyle, locale)
        case TIME => f = DateFormat.getTimeInstance(iTimeStyle, locale)
        case DATETIME =>
          f = DateFormat.getDateTimeInstance(iDateStyle, iTimeStyle, locale)
      }
      if (f.isInstanceOf[SimpleDateFormat] == false) {
        throw new IllegalArgumentException(
          "No datetime pattern for locale: " + locale)
      }
      f.asInstanceOf[SimpleDateFormat].toPattern()
    }
  }

  class StyleFormatterCacheKey(iType: Int,
                               iDateStyle: Int,
                               iTimeStyle: Int,
                               private val locale: Locale) {

    private val combinedTypeAndStyle = iType + (iDateStyle << 4) + (iTimeStyle << 8)

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + combinedTypeAndStyle
      result = prime * result + (if ((locale == null)) 0 else locale.hashCode)
      result
    }

    override def equals(obj: Any): Boolean = {
      if (super.equals(obj)) {
        return true
      }
      if (obj == null) {
        return false
      }
      if (!(obj.isInstanceOf[StyleFormatterCacheKey])) {
        return false
      }
      val other = obj.asInstanceOf[StyleFormatterCacheKey]
      if (combinedTypeAndStyle != other.combinedTypeAndStyle) {
        return false
      }
      if (locale == null) {
        if (other.locale != null) {
          return false
        }
      } else if (locale != other.locale) {
        return false
      }
      true
    }
  }
}

class DateTimeFormat protected ()
