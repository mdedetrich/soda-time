package org.joda.time.tz

import java.io.BufferedReader
import java.io.DataOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FileReader
import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import java.util.Locale
import java.util.Map
import java.util.StringTokenizer
import java.util.TreeMap
import java.util.Map.Entry
import org.joda.time.Chronology
import org.joda.time.DateTime
import org.joda.time.DateTimeField
import org.joda.time.DateTimeZone
import org.joda.time.LocalDate
import org.joda.time.MutableDateTime
import org.joda.time.chrono.ISOChronology
import org.joda.time.chrono.LenientChronology
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.ISODateTimeFormat
import ZoneInfoCompiler._
import scala.util.control.Breaks._

object ZoneInfoCompiler {

  var cStartOfYear: DateTimeOfYear = null
  var cLenientISO: Chronology = null

  var cVerbose: ThreadLocal[Boolean] = new ThreadLocal[Boolean]() {
    override protected def initialValue(): Boolean = false
  }

  def verbose(): Boolean = cVerbose.get

  def main(args: Array[String]) {
    if (args.length == 0) {
      printUsage()
      return
    }
    var inputDir: File = null
    var outputDir: File = null
    var verbose = false
    var i: Int = 0
    i = 0
    while (i < args.length) {
      try {
        if ("-src" == args(i)) {
          inputDir = new File(args(i))
        } else if ("-dst" == args(i)) {
          outputDir = new File(args(i))
        } else if ("-verbose" == args(i)) {
          verbose = true
        } else if ("-?" == args(i)) {
          printUsage()
          return
        } else {
          break()
        }
      } catch {
        case e: IndexOutOfBoundsException => {
          printUsage()
          return
        }
      }
      i += 1
    }
    if (i >= args.length) {
      printUsage()
      return
    }
    val sources = Array.ofDim[File](args.length - i)
    var j = 0
    while (i < args.length) {
      sources(j) =
        if (inputDir == null) new File(args(i))
        else new File(inputDir, args(i))
      i += 1
      j += 1
    }
    cVerbose.set(verbose)
    val zic = new ZoneInfoCompiler()
    zic.compile(outputDir, sources)
  }

  private def printUsage() {
    println(
      "Usage: java org.joda.time.tz.ZoneInfoCompiler <options> <source files>")
    println("where possible options include:")
    println("  -src <directory>    Specify where to read source files")
    println("  -dst <directory>    Specify where to write generated files")
    println("  -verbose            Output verbosely (default false)")
  }

  def getStartOfYear(): DateTimeOfYear = {
    if (cStartOfYear == null) {
      cStartOfYear = new DateTimeOfYear()
    }
    cStartOfYear
  }

  def getLenientISOChronology(): Chronology = {
    if (cLenientISO == null) {
      cLenientISO = LenientChronology.getInstance(ISOChronology.getInstanceUTC)
    }
    cLenientISO
  }

  def writeZoneInfoMap(dout: DataOutputStream,
                       zimap: Map[String, DateTimeZone]) {
    val idToIndex = new HashMap[String, Short](zimap.size)
    val indexToId = new TreeMap[Short, String]()
    var count: Short = 0
    import scala.collection.JavaConversions._
    for (entry <- zimap.entrySet) {
      var id: String = entry.getKey
      if (!idToIndex.containsKey(id)) {
        val index: Short = Short.box(count)
        idToIndex.put(id, index)
        indexToId.put(index, id)
        if ({
          count = (count + 1).toShort
          count
        } == 0) {
          throw new InternalError("Too many time zone ids")
        }
      }
      id = entry.getValue.getID
      if (!idToIndex.containsKey(id)) {
        val index: Short = Short.box(count)
        idToIndex.put(id, index)
        indexToId.put(index, id)
        if ({
          count = (count + 1).toShort
          count
        } == 0) {
          throw new InternalError("Too many time zone ids")
        }
      }
    }

    dout.writeShort(indexToId.size)
    for (id <- indexToId.values) {
      dout.writeUTF(id)
    }
    dout.writeShort(zimap.size)
    for ((key, value) <- zimap) {
      var id = key
      dout.writeShort(idToIndex.get(id).shortValue())
      id = value.getID
      dout.writeShort(idToIndex.get(id).shortValue())
    }
  }

  def parseYear(str: String, `def`: Int): Int = {
    var _str: String = str
    _str = _str.toLowerCase()
    if (_str == "minimum" || _str == "min") {
      return Integer.MIN_VALUE
    } else if (_str == "maximum" || _str == "max") {
      return Integer.MAX_VALUE
    } else if (_str == "only") {
      return `def`
    }
    Integer.parseInt(_str)
  }

  def parseMonth(str: String): Int = {
    val field = ISOChronology.getInstanceUTC.monthOfYear()
    field.get(field.set(0, str, Locale.ENGLISH))
  }

  def parseDayOfWeek(str: String): Int = {
    val field = ISOChronology.getInstanceUTC.dayOfWeek()
    field.get(field.set(0, str, Locale.ENGLISH))
  }

  def parseOptional(str: String): String = if (str == "-") null else str

  def parseTime(str: String): Int = {
    val p = ISODateTimeFormat.hourMinuteSecondFraction()
    val mdt = new MutableDateTime(0, getLenientISOChronology)
    var pos = 0
    if (str.startsWith("-")) {
      pos = 1
    }
    val newPos = p.parseInto(mdt, str, pos)
    if (newPos == ~pos) {
      throw new IllegalArgumentException(str)
    }
    var millis = mdt.getMillis.toInt
    if (pos == 1) {
      millis = -millis
    }
    millis
  }

  def parseZoneChar(c: Char): Char = c match {
    case 's' | 'S' => 's'
    case 'u' | 'U' | 'g' | 'G' | 'z' | 'Z' => 'u'
    case 'w' | 'W' => 'w'
    case _ => 'w'
  }

  def test(id: String, tz: DateTimeZone): Boolean = {
    if (id != tz.getID) {
      return true
    }
    var millis = ISOChronology.getInstanceUTC.year().set(0, 1850)
    var end = ISOChronology.getInstanceUTC.year().set(0, 2050)
    var offset = tz.getOffset(millis)
    var key = tz.getNameKey(millis)
    val transitions = new ArrayList[Long]()
    while (true) {
      val next = tz.nextTransition(millis)
      if (next == millis || next > end) {
        break()
      }
      millis = next
      val nextOffset = tz.getOffset(millis)
      val nextKey = tz.getNameKey(millis)
      if (offset == nextOffset && key == nextKey) {
        println(
          "*d* Error in " + tz.getID + " " + new DateTime(
            millis,
            ISOChronology.getInstanceUTC))
        return false
      }
      if (nextKey == null || (nextKey.length < 3 && "??" != nextKey)) {
        println(
          "*s* Error in " + tz.getID + " " + new DateTime(
            millis,
            ISOChronology.getInstanceUTC) +
            ", nameKey=" +
            nextKey)
        return false
      }
      transitions.add(Long.box(millis))
      offset = nextOffset
      key = nextKey
    }
    millis = ISOChronology.getInstanceUTC.year().set(0, 2050)
    end = ISOChronology.getInstanceUTC.year().set(0, 1850)
    val i = transitions.size
    while (i >= 0) {
      val prev = tz.previousTransition(millis)
      if (prev == millis || prev < end) {
        break()
      }
      millis = prev
      val trans = transitions.get(i).longValue()
      if (trans - 1 != millis) {
        println(
          "*r* Error in " + tz.getID + " " + new DateTime(
            millis,
            ISOChronology.getInstanceUTC) +
            " != " +
            new DateTime(trans - 1, ISOChronology.getInstanceUTC))
        return false
      }
    }
    true
  }

  class DateTimeOfYear() {

    var iMonthOfYear = 1
    var iDayOfMonth = 1
    var iDayOfWeek = 0
    var iAdvanceDayOfWeek = false
    var iMillisOfDay = 0
    var iZoneChar = 'w'

    def this(st: StringTokenizer) {
      this()
      var month = 1
      var day = 1
      var dayOfWeek = 0
      var millis = 0
      var advance = false
      var zoneChar = 'w'
      if (st.hasMoreTokens()) {
        month = parseMonth(st.nextToken())
        if (st.hasMoreTokens()) {
          var str = st.nextToken()
          if (str.startsWith("last")) {
            day = -1
            dayOfWeek = parseDayOfWeek(str.substring(4))
            advance = false
          } else {
            try {
              day = Integer.parseInt(str)
              dayOfWeek = 0
              advance = false
            } catch {
              case e: NumberFormatException => {
                var index = str.indexOf(">=")
                if (index > 0) {
                  day = Integer.parseInt(str.substring(index + 2))
                  dayOfWeek = parseDayOfWeek(str.substring(0, index))
                  advance = true
                } else {
                  index = str.indexOf("<=")
                  if (index > 0) {
                    day = Integer.parseInt(str.substring(index + 2))
                    dayOfWeek = parseDayOfWeek(str.substring(0, index))
                    advance = false
                  } else {
                    throw new IllegalArgumentException(str)
                  }
                }
              }
            }
          }
          if (st.hasMoreTokens()) {
            str = st.nextToken()
            zoneChar = parseZoneChar(str.charAt(str.length - 1))
            if (str == "24:00") {
              if (month == 12 && day == 31) {
                millis = parseTime("23:59:59.999")
              } else {
                val date =
                  if (day == -1) new LocalDate(2001, month, 1).plusMonths(1)
                  else new LocalDate(2001, month, day).plusDays(1)
                advance = day != -1 && dayOfWeek != 0
                month = date.getMonthOfYear
                day = date.getDayOfMonth
                if (dayOfWeek != 0) {
                  dayOfWeek = ((dayOfWeek - 1 + 1) % 7) + 1
                }
              }
            } else {
              millis = parseTime(str)
            }
          }
        }
      }
      iMonthOfYear = month
      iDayOfMonth = day
      iDayOfWeek = dayOfWeek
      iAdvanceDayOfWeek = advance
      iMillisOfDay = millis
      iZoneChar = zoneChar
    }

    def addRecurring(builder: DateTimeZoneBuilder,
                     nameKey: String,
                     saveMillis: Int,
                     fromYear: Int,
                     toYear: Int) {
      builder.addRecurringSavings(nameKey,
                                  saveMillis,
                                  fromYear,
                                  toYear,
                                  iZoneChar,
                                  iMonthOfYear,
                                  iDayOfMonth,
                                  iDayOfWeek,
                                  iAdvanceDayOfWeek,
                                  iMillisOfDay)
    }

    def addCutover(builder: DateTimeZoneBuilder, year: Int) {
      builder.addCutover(year,
                         iZoneChar,
                         iMonthOfYear,
                         iDayOfMonth,
                         iDayOfWeek,
                         iAdvanceDayOfWeek,
                         iMillisOfDay)
    }

    override def toString(): String = {
      "MonthOfYear: " + iMonthOfYear + "\n" + "DayOfMonth: " +
        iDayOfMonth +
        "\n" +
        "DayOfWeek: " +
        iDayOfWeek +
        "\n" +
        "AdvanceDayOfWeek: " +
        iAdvanceDayOfWeek +
        "\n" +
        "MillisOfDay: " +
        iMillisOfDay +
        "\n" +
        "ZoneChar: " +
        iZoneChar +
        "\n"
    }
  }

  private class Rule(st: StringTokenizer) {

    val iName = st.nextToken().intern()

    val iFromYear = parseYear(st.nextToken(), 0)

    val iToYear = parseYear(st.nextToken(), iFromYear)

    val iType = parseOptional(st.nextToken())

    val iDateTimeOfYear = new DateTimeOfYear(st)

    val iSaveMillis = parseTime(st.nextToken())

    val iLetterS = parseOptional(st.nextToken())

    if (iToYear < iFromYear) {
      throw new IllegalArgumentException()
    }

    def addRecurring(builder: DateTimeZoneBuilder, nameFormat: String) {
      val nameKey = formatName(nameFormat)
      iDateTimeOfYear
        .addRecurring(builder, nameKey, iSaveMillis, iFromYear, iToYear)
    }

    private def formatName(nameFormat: String): String = {
      var index = nameFormat.indexOf('/')
      if (index > 0) {
        if (iSaveMillis == 0) {
          return nameFormat.substring(0, index).intern()
        } else {
          return nameFormat.substring(index + 1).intern()
        }
      }
      index = nameFormat.indexOf("%s")
      if (index < 0) {
        return nameFormat
      }
      val left = nameFormat.substring(0, index)
      val right = nameFormat.substring(index + 2)
      var name: String = null
      name =
        if (iLetterS == null) left.concat(right) else left + iLetterS + right
      name.intern()
    }

    override def toString(): String = {
      "[Rule]\n" + "Name: " + iName + "\n" + "FromYear: " +
        iFromYear +
        "\n" +
        "ToYear: " +
        iToYear +
        "\n" +
        "Type: " +
        iType +
        "\n" +
        iDateTimeOfYear +
        "SaveMillis: " +
        iSaveMillis +
        "\n" +
        "LetterS: " +
        iLetterS +
        "\n"
    }
  }

  private class RuleSet(rule: Rule) {

    private val iRules: List[Rule] = new ArrayList[Rule]()

    iRules.add(rule)

    def addRule(rule: Rule) {
      if (!(rule.iName == iRules.get(0).iName)) {
        throw new IllegalArgumentException("Rule name mismatch")
      }
      iRules.add(rule)
    }

    def addRecurring(builder: DateTimeZoneBuilder, nameFormat: String) {
      for (i <- 0 until iRules.size) {
        val rule = iRules.get(i)
        rule.addRecurring(builder, nameFormat)
      }
    }
  }

  object Zone {

    private def addToBuilder(zone: Zone,
                             builder: DateTimeZoneBuilder,
                             ruleSets: Map[String, RuleSet]) {
      var _zone: Zone = zone
      while (_zone != null) {
        builder.setStandardOffset(_zone.iOffsetMillis)
        if (_zone.iRules == null) {
          builder.setFixedSavings(_zone.iFormat, 0)
        } else {
          try {
            val saveMillis = parseTime(_zone.iRules)
            builder.setFixedSavings(_zone.iFormat, saveMillis)
          } catch {
            case e: Exception => {
              val rs = ruleSets.get(_zone.iRules)
              if (rs == null) {
                throw new IllegalArgumentException(
                  "Rules not found: " + _zone.iRules)
              }
              rs.addRecurring(builder, _zone.iFormat)
            }
          }
        }
        if (_zone.iUntilYear == Integer.MAX_VALUE) {
          break()
        }
        _zone.iUntilDateTimeOfYear.addCutover(builder, _zone.iUntilYear)
        _zone = _zone.iNext
      }
    }
  }

  private class Zone private (name: String, st: StringTokenizer) {

    var iName = name.intern()
    var iOffsetMillis = parseTime(st.nextToken())
    var iRules = parseOptional(st.nextToken())
    var iFormat = st.nextToken().intern()
    var iUntilYear: Int = _
    var iUntilDateTimeOfYear: DateTimeOfYear = null
    private var iNext: Zone = null

    var year = Integer.MAX_VALUE

    var dtOfYear = getStartOfYear

    if (st.hasMoreTokens()) {
      year = Integer.parseInt(st.nextToken())
      if (st.hasMoreTokens()) {
        dtOfYear = new DateTimeOfYear(st)
      }
    }

    def this(st: StringTokenizer) {
      this(st.nextToken(), st)
    }

    def chain(st: StringTokenizer) {
      if (iNext != null) {
        iNext.chain(st)
      } else {
        iNext = new Zone(iName, st)
      }
    }

    def addToBuilder(builder: DateTimeZoneBuilder,
                     ruleSets: Map[String, RuleSet]) {
      Zone.addToBuilder(this, builder, ruleSets)
    }

    override def toString(): String = {
      val str = "[Zone]\n" + "Name: " + iName + "\n" + "OffsetMillis: " +
          iOffsetMillis +
          "\n" +
          "Rules: " +
          iRules +
          "\n" +
          "Format: " +
          iFormat +
          "\n" +
          "UntilYear: " +
          iUntilYear +
          "\n" +
          iUntilDateTimeOfYear
      if (iNext == null) {
        return str
      }
      str + "...\n" + iNext.toString
    }
  }
}

class ZoneInfoCompiler {

  private val iRuleSets: Map[String, RuleSet] = new HashMap[String, RuleSet]()
  private val iZones: List[Zone] = new ArrayList[Zone]()
  private val iLinks: List[String] = new ArrayList[String]()

  def compile(outputDir: File,
              sources: Array[File]): Map[String, DateTimeZone] = {
    if (sources != null) {
      for (i <- 0 until sources.length) {
        val in = new BufferedReader(new FileReader(sources(i)))
        parseDataFile(in)
        in.close()
      }
    }
    if (outputDir != null) {
      if (!outputDir.exists()) {
        if (!outputDir.mkdirs()) {
          throw new IOException(
            "Destination directory doesn't exist and cannot be created: " +
              outputDir)
        }
      }
      if (!outputDir.isDirectory) {
        throw new IOException("Destination is not a directory: " + outputDir)
      }
    }
    val map = new TreeMap[String, DateTimeZone]()
    println("Writing zoneinfo files")
    for (i <- 0 until iZones.size) {
      val zone = iZones.get(i)
      val builder = new DateTimeZoneBuilder()
      zone.addToBuilder(builder, iRuleSets)
      val original = builder.toDateTimeZone(zone.iName, true)
      val tz = original
      if (test(tz.getID, tz)) {
        map.put(tz.getID, tz)
        if (outputDir != null) {
          if (ZoneInfoCompiler.verbose()) {
            println("Writing " + tz.getID)
          }
          val file = new File(outputDir, tz.getID)
          if (!file.getParentFile.exists()) {
            file.getParentFile.mkdirs()
          }
          val out = new FileOutputStream(file)
          try {
            builder.writeTo(zone.iName, out)
          } finally {
            out.close()
          }
          val in = new FileInputStream(file)
          val tz2 = DateTimeZoneBuilder.readFrom(in, tz.getID)
          in.close()
          if (original != tz2) {
            println(
              "*e* Error in " + tz.getID + ": Didn't read properly from file")
          }
        }
      }
    }
    for (pass <- 0 until 2) {
      var i = 0
      while (i < iLinks.size) {
        val id = iLinks.get(i)
        val alias = iLinks.get(i + 1)
        val tz = map.get(id)
        if (tz == null) {
          if (pass > 0) {
            println(
              "Cannot find time zone '" + id + "' to link alias '" +
                alias +
                "' to")
          }
        } else {
          map.put(alias, tz)
        }
        i += 2
      }
    }
    if (outputDir != null) {
      println("Writing ZoneInfoMap")
      val file = new File(outputDir, "ZoneInfoMap")
      if (!file.getParentFile.exists()) {
        file.getParentFile.mkdirs()
      }
      val out = new FileOutputStream(file)
      val dout = new DataOutputStream(out)
      try {
        val zimap =
          new TreeMap[String, DateTimeZone](String.CASE_INSENSITIVE_ORDER)
        zimap.putAll(map)
        writeZoneInfoMap(dout, zimap)
      } finally {
        dout.close()
      }
    }
    map
  }

  def parseDataFile(in: BufferedReader) {
    var zone: Zone = null
    var line: String = null
    while ({
      line = in.readLine(); in
    } != null) {
      var continueFlag = true
      val trimmed = line.trim()
      if (trimmed.length == 0 || trimmed.charAt(0) == '#') {
        continueFlag = false
      }
      if (continueFlag) {
        val index = line.indexOf('#')
        if (index >= 0) {
          line = line.substring(0, index)
        }
        val st = new StringTokenizer(line, " \t")
        if (Character.isWhitespace(line.charAt(0)) && st.hasMoreTokens()) {
          if (zone != null) {
            zone.chain(st)
          }
          continueFlag = false
        } else {
          if (continueFlag) {
            if (zone != null) {
              iZones.add(zone)
            }
            zone = null
          }
        }
        if (continueFlag) {
          if (st.hasMoreTokens()) {
            val token = st.nextToken()
            if (token.equalsIgnoreCase("Rule")) {
              val r = new Rule(st)
              var rs = iRuleSets.get(r.iName)
              if (rs == null) {
                rs = new RuleSet(r)
                iRuleSets.put(r.iName, rs)
              } else {
                rs.addRule(r)
              }
            } else if (token.equalsIgnoreCase("Zone")) {
              zone = new Zone(st)
            } else if (token.equalsIgnoreCase("Link")) {
              iLinks.add(st.nextToken())
              iLinks.add(st.nextToken())
            } else {
              println("Unknown line: " + line)
            }
          }
        }
      }
    }
    if (zone != null) {
      iZones.add(zone)
    }
  }
}
