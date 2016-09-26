package org.joda.time.tz

import java.io.DataInput
import java.io.DataInputStream
import java.io.DataOutput
import java.io.DataOutputStream
import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.text.DateFormatSymbols
import java.util.Arrays
import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTime
import org.joda.time.DateTimeUtils
import org.joda.time.DateTimeZone
import org.joda.time.Period
import org.joda.time.PeriodType
import org.joda.time.chrono.ISOChronology
import DateTimeZoneBuilder._
import scala.util.control.Breaks._
import scala.scalajs.js
import js.JSConverters._

object DateTimeZoneBuilder {

  def readFrom(in: InputStream, id: String): DateTimeZone = {
    if (in.isInstanceOf[DataInput]) {
      readFrom(in.asInstanceOf[DataInput], id)
    } else {
      readFrom(new DataInputStream(in).asInstanceOf[DataInput], id)
    }
  }

  def readFrom(in: DataInput, id: String): DateTimeZone = in.readUnsignedByte() match {
    case 'F' =>
      var fixed: DateTimeZone = new FixedDateTimeZone(id, in.readUTF(), readMillis(in).toInt, readMillis(in).toInt)
      if (fixed == DateTimeZone.UTC) {
        fixed = DateTimeZone.UTC
      }
      fixed

    case 'C' => CachedDateTimeZone.forZone(PrecalculatedZone.readFrom(in, id))
    case 'P' => PrecalculatedZone.readFrom(in, id)
    case _ => throw new IOException("Invalid encoding")
  }

  def writeMillis(out: DataOutput, millis: Long) {
    if (millis % (30 * 60000L) == 0) {
      val units = millis / (30 * 60000L)
      if (((units << (64 - 6)) >> (64 - 6)) == units) {
        out.writeByte((units & 0x3f).toInt)
        return
      }
    }
    if (millis % 60000L == 0) {
      val minutes = millis / 60000L
      if (((minutes << (64 - 30)) >> (64 - 30)) == minutes) {
        out.writeInt(0x40000000 | (minutes & 0x3fffffff).toInt)
        return
      }
    }
    if (millis % 1000L == 0) {
      val seconds = millis / 1000L
      if (((seconds << (64 - 38)) >> (64 - 38)) == seconds) {
        out.writeByte(0x80 | ((seconds >> 32) & 0x3f).toInt)
        out.writeInt((seconds & 0xffffffff).toInt)
        return
      }
    }
    out.writeByte(if (millis < 0) 0xff else 0xc0)
    out.writeLong(millis)
  }

  def readMillis(in: DataInput): Long = {
    var v = in.readUnsignedByte()
    v >> 6 match {
      case 1 =>
        v = (v << (32 - 6)) >> (32 - 30)
        v |= (in.readUnsignedByte()) << 16
        v |= (in.readUnsignedByte()) << 8
        v |= (in.readUnsignedByte())
        v * 60000L

      case 2 =>
        var w = (v.toLong << (64 - 6)) >> (64 - 38)
        w |= (in.readUnsignedByte()) << 24
        w |= (in.readUnsignedByte()) << 16
        w |= (in.readUnsignedByte()) << 8
        w |= (in.readUnsignedByte())
        w * 1000L

      case 3 => in.readLong()

      case 0 | _ =>
        v = (v << (32 - 6)) >> (32 - 6)
        v * (30 * 60000L)

    }
  }

  private def buildFixedZone(id: String,
                             nameKey: String,
                             wallOffset: Int,
                             standardOffset: Int): DateTimeZone = {
    if ("UTC" == id && id == nameKey && wallOffset == 0 && standardOffset == 0) {
      return DateTimeZone.UTC
    }
    new FixedDateTimeZone(id, nameKey, wallOffset, standardOffset)
  }

  object OfYear {

    def readFrom(in: DataInput): OfYear = {
      new OfYear(in.readUnsignedByte().toChar, in.readUnsignedByte().toInt, in.readByte().toInt, in.readUnsignedByte().toInt,
        in.readBoolean(), readMillis(in).toInt)
    }
  }

  class OfYear(val mode: Char,
               val monthOfYear: Int,
               val dayOfMonth: Int,
               val dayOfWeek: Int,
               val advanceDayOfWeek: Boolean,
               val millisOfDay: Int) {


    private var iMode: Char = _
    private var iMonthOfYear: Int = _
    private var iDayOfMonth: Int = _
    private var iDayOfWeek: Int = _
    private var iAdvance: Boolean = _
    private var iMillisOfDay: Int = _

    if (mode != 'u' && mode != 'w' && mode != 's') {
      throw new IllegalArgumentException("Unknown mode: " + mode)
    }

    iMode = mode
    iMonthOfYear = monthOfYear
    iDayOfMonth = dayOfMonth
    iDayOfWeek = dayOfWeek
    iAdvance = advanceDayOfWeek
    iMillisOfDay = millisOfDay

    def setInstant(year: Int, standardOffset: Int, saveMillis: Int): Long = {
      var offset: Int = 0
      offset = if (iMode == 'w') standardOffset + saveMillis else if (iMode == 's') standardOffset else 0
      val chrono = ISOChronology.getInstanceUTC
      var millis = chrono.year().set(0, year)
      millis = chrono.monthOfYear().set(millis, iMonthOfYear)
      millis = chrono.millisOfDay().set(millis, iMillisOfDay)
      millis = setDayOfMonth(chrono, millis)
      if (iDayOfWeek != 0) {
        millis = setDayOfWeek(chrono, millis)
      }
      millis - offset
    }

    def next(instant: Long, standardOffset: Int, saveMillis: Int): Long = {
      var _instant: Long = instant
      var offset: Int = 0
      offset = if (iMode == 'w') standardOffset + saveMillis else if (iMode == 's') standardOffset else 0
      _instant += offset
      val chrono = ISOChronology.getInstanceUTC
      var next = chrono.monthOfYear().set(_instant, iMonthOfYear)
      next = chrono.millisOfDay().set(next, 0)
      next = chrono.millisOfDay().add(next, iMillisOfDay)
      next = setDayOfMonthNext(chrono, next)
      if (iDayOfWeek == 0) {
        if (next <= _instant) {
          next = chrono.year().add(next, 1)
          next = setDayOfMonthNext(chrono, next)
        }
      } else {
        next = setDayOfWeek(chrono, next)
        if (next <= _instant) {
          next = chrono.year().add(next, 1)
          next = chrono.monthOfYear().set(next, iMonthOfYear)
          next = setDayOfMonthNext(chrono, next)
          next = setDayOfWeek(chrono, next)
        }
      }
      next - offset
    }

    def previous(instant: Long, standardOffset: Int, saveMillis: Int): Long = {
      var _instant: Long = instant
      var offset: Int = 0
      offset = if (iMode == 'w') standardOffset + saveMillis else if (iMode == 's') standardOffset else 0
      _instant += offset
      val chrono = ISOChronology.getInstanceUTC
      var prev = chrono.monthOfYear().set(_instant, iMonthOfYear)
      prev = chrono.millisOfDay().set(prev, 0)
      prev = chrono.millisOfDay().add(prev, iMillisOfDay)
      prev = setDayOfMonthPrevious(chrono, prev)
      if (iDayOfWeek == 0) {
        if (prev >= _instant) {
          prev = chrono.year().add(prev, -1)
          prev = setDayOfMonthPrevious(chrono, prev)
        }
      } else {
        prev = setDayOfWeek(chrono, prev)
        if (prev >= _instant) {
          prev = chrono.year().add(prev, -1)
          prev = chrono.monthOfYear().set(prev, iMonthOfYear)
          prev = setDayOfMonthPrevious(chrono, prev)
          prev = setDayOfWeek(chrono, prev)
        }
      }
      prev - offset
    }

    override def equals(obj: Any): Boolean = {
      if (super.equals(obj)) {
        return true
      }
      if (obj.isInstanceOf[OfYear]) {
        val other = obj.asInstanceOf[OfYear]
        return iMode == other.iMode && iMonthOfYear == other.iMonthOfYear &&
          iDayOfMonth == other.iDayOfMonth &&
          iDayOfWeek == other.iDayOfWeek &&
          iAdvance == other.iAdvance &&
          iMillisOfDay == other.iMillisOfDay
      }
      false
    }

    def writeTo(out: DataOutput) {
      out.writeByte(iMode)
      out.writeByte(iMonthOfYear)
      out.writeByte(iDayOfMonth)
      out.writeByte(iDayOfWeek)
      out.writeBoolean(iAdvance)
      writeMillis(out, iMillisOfDay)
    }

    private def setDayOfMonthNext(chrono: Chronology, next: Long): Long = {
      var _next: Long = next
      try {
        _next = setDayOfMonth(chrono, _next)
      } catch {
        case e: IllegalArgumentException => if (iMonthOfYear == 2 && iDayOfMonth == 29) {
          while (chrono.year().isLeap(_next) == false) {
            _next = chrono.year().add(_next, 1)
          }
          _next = setDayOfMonth(chrono, _next)
        } else {
          throw e
        }
      }
      _next
    }

    private def setDayOfMonthPrevious(chrono: Chronology, prev: Long): Long = {
      var _prev: Long = prev
      try {
        _prev = setDayOfMonth(chrono, _prev)
      } catch {
        case e: IllegalArgumentException => if (iMonthOfYear == 2 && iDayOfMonth == 29) {
          while (chrono.year().isLeap(_prev) == false) {
            _prev = chrono.year().add(_prev, -1)
          }
          _prev = setDayOfMonth(chrono, _prev)
        } else {
          throw e
        }
      }
      _prev
    }

    private def setDayOfMonth(chrono: Chronology, instant: Long): Long = {
      var _instant: Long = instant
      if (iDayOfMonth >= 0) {
        _instant = chrono.dayOfMonth().set(_instant, iDayOfMonth)
      } else {
        _instant = chrono.dayOfMonth().set(_instant, 1)
        _instant = chrono.monthOfYear().add(_instant, 1)
        _instant = chrono.dayOfMonth().add(_instant, iDayOfMonth)
      }
      _instant
    }

    private def setDayOfWeek(chrono: Chronology, instant: Long): Long = {
      var _instant: Long = instant
      val dayOfWeek = chrono.dayOfWeek().get(_instant)
      var daysToAdd = iDayOfWeek - dayOfWeek
      if (daysToAdd != 0) {
        if (iAdvance) {
          if (daysToAdd < 0) {
            daysToAdd += 7
          }
        } else {
          if (daysToAdd > 0) {
            daysToAdd -= 7
          }
        }
        _instant = chrono.dayOfWeek().add(_instant, daysToAdd)
      }
      _instant
    }
  }

  object Recurrence {

    def readFrom(in: DataInput): Recurrence = {
      new Recurrence(OfYear.readFrom(in), in.readUTF(), readMillis(in).toInt)
    }
  }

  class Recurrence(val iOfYear: OfYear, val iNameKey: String, val iSaveMillis: Int)
  {

    def getOfYear(): OfYear = iOfYear

    def next(instant: Long, standardOffset: Int, saveMillis: Int): Long = {
      iOfYear.next(instant, standardOffset, saveMillis)
    }

    def previous(instant: Long, standardOffset: Int, saveMillis: Int): Long = {
      iOfYear.previous(instant, standardOffset, saveMillis)
    }

    def getNameKey(): String = iNameKey

    def getSaveMillis(): Int = iSaveMillis

    override def equals(obj: Any): Boolean = {
      if (super.equals(obj)) {
        return true
      }
      if (obj.isInstanceOf[Recurrence]) {
        val other = obj.asInstanceOf[Recurrence]
        return iSaveMillis == other.iSaveMillis && iNameKey == other.iNameKey &&
          iOfYear == other.iOfYear
      }
      false
    }

    def writeTo(out: DataOutput) {
      iOfYear.writeTo(out)
      out.writeUTF(iNameKey)
      writeMillis(out, iSaveMillis)
    }

    def rename(nameKey: String): Recurrence = {
      new Recurrence(iOfYear, nameKey, iSaveMillis)
    }

    def renameAppend(appendNameKey: String): Recurrence = {
      rename((iNameKey + appendNameKey).intern())
    }
  }

  private class Rule(val iRecurrence: Recurrence, val iFromYear: Int, val iToYear: Int)
  {

    def getFromYear(): Int = iFromYear

    def getToYear(): Int = iToYear

    def getOfYear(): OfYear = iRecurrence.getOfYear

    def getNameKey(): String = iRecurrence.getNameKey

    def getSaveMillis(): Int = iRecurrence.getSaveMillis

    def next(instant: Long, standardOffset: Int, saveMillis: Int): Long = {
      val chrono = ISOChronology.getInstanceUTC
      val wallOffset = standardOffset + saveMillis
      var testInstant = instant
      var year: Int = 0
      year = if (instant == Long.MinValue) Integer.MIN_VALUE else chrono.year().get(instant + wallOffset)
      if (year < iFromYear) {
        testInstant = chrono.year().set(0, iFromYear) - wallOffset
        testInstant -= 1
      }
      var next = iRecurrence.next(testInstant, standardOffset, saveMillis)
      if (next > instant) {
        year = chrono.year().get(next + wallOffset)
        if (year > iToYear) {
          next = instant
        }
      }
      next
    }
  }

  private class Transition {

    private var iNameKey: String = null
    private var iWallOffset: Int = _
    private var iStandardOffset: Int = _
    private var iMillis: Long = _

    def this(millis: Long, tr: DateTimeZoneBuilder.Transition) {
      this()
      iMillis = millis
      iNameKey = tr.iNameKey
      iWallOffset = tr.iWallOffset
      iStandardOffset = tr.iStandardOffset
    }

    def this(millis: Long, rule: Rule, standardOffset: Int) {
      this()
      iMillis = millis
      iNameKey = rule.getNameKey
      iWallOffset = standardOffset + rule.getSaveMillis
      iStandardOffset = standardOffset
    }

    def this(millis: Long,
             nameKey: String,
             wallOffset: Int,
             standardOffset: Int) {
      this()
      iMillis = millis
      iNameKey = nameKey
      iWallOffset = wallOffset
      iStandardOffset = standardOffset
    }

    def getMillis(): Long = iMillis

    def getNameKey(): String = iNameKey

    def getWallOffset(): Int = iWallOffset

    def getStandardOffset(): Int = iStandardOffset

    def getSaveMillis(): Int = iWallOffset - iStandardOffset

    def isTransitionFrom(other: Transition): Boolean = {
      if (other == null) {
        return true
      }
      iMillis > other.iMillis &&
        (iWallOffset != other.iWallOffset || !(iNameKey == other.iNameKey))
    }
  }

  object RuleSet {
    val now = DateTimeUtils.currentTimeMillis()
    private val YEAR_LIMIT = ISOChronology.getInstanceUTC.year().get(now) + 100
  }

  private class RuleSet() {

    private var iStandardOffset: Int = _
    private var iRules: collection.mutable.ListBuffer[Rule] = new collection.mutable.ListBuffer[Rule]()
    private var iInitialNameKey: String = _
    private var iInitialSaveMillis: Int = _
    private var iUpperYear: Int = Integer.MAX_VALUE
    private var iUpperOfYear: OfYear = _

    def this(rs: RuleSet) {
      this()
      iStandardOffset = rs.iStandardOffset
      iRules = rs.iRules.to[collection.mutable.ListBuffer]
      iInitialNameKey = rs.iInitialNameKey
      iInitialSaveMillis = rs.iInitialSaveMillis
      iUpperYear = rs.iUpperYear
      iUpperOfYear = rs.iUpperOfYear
    }

    def getStandardOffset(): Int = iStandardOffset

    def setStandardOffset(standardOffset: Int) {
      iStandardOffset = standardOffset
    }

    def setFixedSavings(nameKey: String, saveMillis: Int) {
      iInitialNameKey = nameKey
      iInitialSaveMillis = saveMillis
    }

    def addRule(rule: Rule) {
      if (!iRules.contains(rule)) {
        iRules += rule
      }
    }

    def setUpperLimit(year: Int, ofYear: OfYear) {
      iUpperYear = year
      iUpperOfYear = ofYear
    }

    def firstTransition(firstMillis: Long): Transition = {
      if (iInitialNameKey != null) {
        return new Transition(firstMillis, iInitialNameKey, iStandardOffset + iInitialSaveMillis, iStandardOffset)
      }
      val copy = iRules.to[collection.mutable.ListBuffer]
      var millis = Long.MinValue
      var saveMillis = 0
      var first: Transition = null
      var next: Transition = null
      while ( {
        next = nextTransition(millis, saveMillis); next
      } != null) {
        millis = next.getMillis
        if (millis == firstMillis) {
          first = new Transition(firstMillis, next)
          break()
        }
        if (millis > firstMillis) {
          if (first == null) {
            import scala.collection.JavaConversions._
            for (rule <- copy) {
              if (rule.getSaveMillis == 0) {
                first = new DateTimeZoneBuilder.Transition(firstMillis, rule, iStandardOffset)
                break()
              }
            }

          }
          if (first == null) {
            first = new Transition(firstMillis, next.getNameKey, iStandardOffset, iStandardOffset)
          }
          break()
        }
        first = new Transition(firstMillis, next)
        saveMillis = next.getSaveMillis
      }
      iRules = copy
      first
    }

    def nextTransition(instant: Long, saveMillis: Int): Transition = {
      val chrono = ISOChronology.getInstanceUTC
      var nextRule: Rule = null
      var nextMillis = Long.MaxValue
      val it = iRules.iterator
      while (it.hasNext) {
        var continueFlag = true
        val rule = it.next()
        val next = rule.next(instant, iStandardOffset, saveMillis)
        if (next <= instant) {
          val index = iRules.indexOf(rule)
          iRules.remove(index)
          continueFlag = false
        }
        if (continueFlag) {
          if (next <= nextMillis) {
            nextRule = rule
            nextMillis = next
          }
        }
      }
      if (nextRule == null) {
        return null
      }
      if (chrono.year().get(nextMillis) >= RuleSet.YEAR_LIMIT) {
        return null
      }
      if (iUpperYear < Integer.MAX_VALUE) {
        val upperMillis = iUpperOfYear.setInstant(iUpperYear, iStandardOffset, saveMillis)
        if (nextMillis >= upperMillis) {
          return null
        }
      }
      new Transition(nextMillis, nextRule, iStandardOffset)
    }

    def getUpperLimit(saveMillis: Int): Long = {
      if (iUpperYear == Integer.MAX_VALUE) {
        return Long.MaxValue
      }
      iUpperOfYear.setInstant(iUpperYear, iStandardOffset, saveMillis)
    }

    def buildTailZone(id: String): DSTZone = {
      if (iRules.size == 2) {
        val startRule = iRules(0)
        val endRule = iRules(1)
        if (startRule.getToYear == Integer.MAX_VALUE && endRule.getToYear == Integer.MAX_VALUE) {
          return new DSTZone(id, iStandardOffset, startRule.iRecurrence, endRule.iRecurrence)
        }
      }
      null
    }
  }

  object DSTZone {

    def readFrom(in: DataInput, id: String): DSTZone = {
      new DSTZone(id, readMillis(in).toInt, Recurrence.readFrom(in), Recurrence.readFrom(in))
    }
  }

  @SerialVersionUID(6941492635554961361L)
  class DSTZone(id: String,
                val iStandardOffset: Int,
                val iStartRecurrence: Recurrence,
                val iEndRecurrence: Recurrence) extends DateTimeZone(id) {

    def getNameKey(instant: Long): String = {
      findMatchingRecurrence(instant).getNameKey
    }

    def getOffset(instant: Long): Int = {
      iStandardOffset + findMatchingRecurrence(instant).getSaveMillis
    }

    def getStandardOffset(instant: Long): Int = iStandardOffset

    def isFixed(): Boolean = false

    def nextTransition(instant: Long): Long = {
      val standardOffset = iStandardOffset
      val startRecurrence = iStartRecurrence
      val endRecurrence = iEndRecurrence
      var start: Long = 0l
      var end: Long = 0l
      try {
        start = startRecurrence.next(instant, standardOffset, endRecurrence.getSaveMillis)
        if (instant > 0 && start < 0) {
          start = instant
        }
      } catch {
        case e: IllegalArgumentException => start = instant
        case e: ArithmeticException => start = instant
      }
      try {
        end = endRecurrence.next(instant, standardOffset, startRecurrence.getSaveMillis)
        if (instant > 0 && end < 0) {
          end = instant
        }
      } catch {
        case e: IllegalArgumentException => end = instant
        case e: ArithmeticException => end = instant
      }
      if (start > end) end else start
    }

    def previousTransition(instant: Long): Long = {
      var _instant: Long = instant
      _instant += 1
      val standardOffset = iStandardOffset
      val startRecurrence = iStartRecurrence
      val endRecurrence = iEndRecurrence
      var start: Long = 0l
      var end: Long = 0l
      try {
        start = startRecurrence.previous(_instant, standardOffset, endRecurrence.getSaveMillis)
        if (_instant < 0 && start > 0) {
          start = _instant
        }
      } catch {
        case e: IllegalArgumentException => start = _instant
        case e: ArithmeticException => start = _instant
      }
      try {
        end = endRecurrence.previous(_instant, standardOffset, startRecurrence.getSaveMillis)
        if (_instant < 0 && end > 0) {
          end = _instant
        }
      } catch {
        case e: IllegalArgumentException => end = _instant
        case e: ArithmeticException => end = _instant
      }
      (if (start > end) start else end) - 1
    }

    override def equals(obj: Any): Boolean = {
      if (super.equals(obj)) {
        return true
      }
      if (obj.isInstanceOf[DSTZone]) {
        val other = obj.asInstanceOf[DSTZone]
        return getID == other.getID && iStandardOffset == other.iStandardOffset &&
          iStartRecurrence == other.iStartRecurrence &&
          iEndRecurrence == other.iEndRecurrence
      }
      false
    }

    def writeTo(out: DataOutput) {
      writeMillis(out, iStandardOffset)
      iStartRecurrence.writeTo(out)
      iEndRecurrence.writeTo(out)
    }

    private def findMatchingRecurrence(instant: Long): Recurrence = {
      val standardOffset = iStandardOffset
      val startRecurrence = iStartRecurrence
      val endRecurrence = iEndRecurrence
      var start: Long = 0l
      var end: Long = 0l
      try {
        start = startRecurrence.next(instant, standardOffset, endRecurrence.getSaveMillis)
      } catch {
        case e: IllegalArgumentException => start = instant
        case e: ArithmeticException => start = instant
      }
      try {
        end = endRecurrence.next(instant, standardOffset, startRecurrence.getSaveMillis)
      } catch {
        case e: IllegalArgumentException => end = instant
        case e: ArithmeticException => end = instant
      }
      if (start > end) startRecurrence else endRecurrence
    }
  }

  object PrecalculatedZone {

    def readFrom(in: DataInput, id: String): PrecalculatedZone = {
      val poolSize = in.readUnsignedShort()
      val pool = js.Array[String]()
      for (i <- 0 until poolSize) {
        pool(i) = in.readUTF()
      }
      val size = in.readInt()
      val transitions = js.Array[Long]()
      val wallOffsets = js.Array[Int]()
      val standardOffsets = js.Array[Int]()
      val nameKeys = js.Array[String]()
      for (i <- 0 until size) {
        transitions(i) = readMillis(in)
        wallOffsets(i) = readMillis(in).toInt
        standardOffsets(i) = readMillis(in).toInt
        var index: Int = 0
        index = if (poolSize < 256) in.readUnsignedByte() else in.readUnsignedShort()
        nameKeys(i) = pool(index)
      }
      var tailZone: DSTZone = null
      if (in.readBoolean()) {
        tailZone = DSTZone.readFrom(in, id)
      }
      new PrecalculatedZone(id, transitions, wallOffsets, standardOffsets, nameKeys, tailZone)
    }

    def create(id: String,
               outputID: Boolean,
               transitions: collection.mutable.ListBuffer[Transition],
               tailZone: DSTZone): PrecalculatedZone = {
      var _tailZone: DSTZone = tailZone
      val size = transitions.size
      if (size == 0) {
        throw new IllegalArgumentException()
      }
      val trans = js.Array[Long]()
      val wallOffsets = js.Array[Int]()
      val standardOffsets = js.Array[Int]()
      val nameKeys = js.Array[String]()
      var last: Transition = null
      for (i <- 0 until size) {
        val tr = transitions(i)
        if (!tr.isTransitionFrom(last)) {
          throw new IllegalArgumentException(id)
        }
        trans(i) = tr.getMillis
        wallOffsets(i) = tr.getWallOffset
        standardOffsets(i) = tr.getStandardOffset
        nameKeys(i) = tr.getNameKey
        last = tr
      }
      var zoneNameData = js.Array[String]()
      val zoneStrings = new DateFormatSymbols(Locale.ENGLISH).getZoneStrings
      for (j <- 0 until zoneStrings.length) {
        val set = zoneStrings(j)
        if (set != null && set.length == 5 && id == set(0)) {
          zoneNameData = set.toJSArray
        }
      }
      val chrono = ISOChronology.getInstanceUTC

      var i = 0
      while ( i < nameKeys.length - 1) {
        val curNameKey = nameKeys(i)
        val nextNameKey = nameKeys(i + 1)
        val curOffset = wallOffsets(i)
        val nextOffset = wallOffsets(i + 1)
        val curStdOffset = standardOffsets(i)
        val nextStdOffset = standardOffsets(i + 1)
        val p = new Period(trans(i), trans(i + 1), PeriodType.yearMonthDay(), chrono)
        if (curOffset != nextOffset && curStdOffset == nextStdOffset &&
          curNameKey == nextNameKey &&
          p.getYears == 0 &&
          p.getMonths > 4 &&
          p.getMonths < 8 &&
          curNameKey == zoneNameData(2) &&
          curNameKey == zoneNameData(4)) {
          if (ZoneInfoCompiler.verbose()) {
            println("Fixing duplicate name key - " + nextNameKey)
            println("     - " + new DateTime(trans(i), chrono) + " - " + new DateTime(trans(i + 1), chrono))
          }
          if (curOffset > nextOffset) {
            nameKeys(i) = (curNameKey + "-Summer").intern()
          } else if (curOffset < nextOffset) {
            nameKeys(i + 1) = (nextNameKey + "-Summer").intern()
            i += 1
          }
        }
        i += 1
      }

      if (_tailZone != null) {
        if (_tailZone.iStartRecurrence.getNameKey == _tailZone.iEndRecurrence.getNameKey) {
          if (ZoneInfoCompiler.verbose()) {
            println("Fixing duplicate recurrent name key - " + _tailZone.iStartRecurrence.getNameKey)
          }
          _tailZone = if (_tailZone.iStartRecurrence.getSaveMillis > 0) new DSTZone(_tailZone.getID, _tailZone.iStandardOffset,
            _tailZone.iStartRecurrence.renameAppend("-Summer"), _tailZone.iEndRecurrence) else new DSTZone(_tailZone.getID,
            _tailZone.iStandardOffset, _tailZone.iStartRecurrence, _tailZone.iEndRecurrence.renameAppend("-Summer"))
        }
      }
      new PrecalculatedZone(if (outputID) id else "", trans, wallOffsets, standardOffsets, nameKeys,
        _tailZone)
    }
  }

  @SerialVersionUID(7811976468055766265L)
  class PrecalculatedZone private (id: String,
                                   private val iTransitions: js.Array[Long],
                                   private val iWallOffsets: js.Array[Int],
                                   private val iStandardOffsets: js.Array[Int],
                                   private val iNameKeys: js.Array[String],
                                   private val iTailZone: DSTZone) extends DateTimeZone(id) {

    def getNameKey(instant: Long): String = {
      val transitions = iTransitions
      var i = Arrays.binarySearch(transitions.toArray, instant)
      if (i >= 0) {
        return iNameKeys(i)
      }
      i = ~i
      if (i < transitions.length) {
        if (i > 0) {
          return iNameKeys(i - 1)
        }
        return "UTC"
      }
      if (iTailZone == null) {
        return iNameKeys(i - 1)
      }
      iTailZone.getNameKey(instant)
    }

    def getOffset(instant: Long): Int = {
      val transitions = iTransitions
      var i = Arrays.binarySearch(transitions.toArray, instant)
      if (i >= 0) {
        return iWallOffsets(i)
      }
      i = ~i
      if (i < transitions.length) {
        if (i > 0) {
          return iWallOffsets(i - 1)
        }
        return 0
      }
      if (iTailZone == null) {
        return iWallOffsets(i - 1)
      }
      iTailZone.getOffset(instant)
    }

    def getStandardOffset(instant: Long): Int = {
      val transitions = iTransitions
      var i = Arrays.binarySearch(transitions.toArray, instant)
      if (i >= 0) {
        return iStandardOffsets(i)
      }
      i = ~i
      if (i < transitions.length) {
        if (i > 0) {
          return iStandardOffsets(i - 1)
        }
        return 0
      }
      if (iTailZone == null) {
        return iStandardOffsets(i - 1)
      }
      iTailZone.getStandardOffset(instant)
    }

    def isFixed(): Boolean = false

    def nextTransition(instant: Long): Long = {
      var _instant: Long = instant
      val transitions = iTransitions
      var i = Arrays.binarySearch(transitions.toArray, _instant)
      i = if (i >= 0) (i + 1) else ~i
      if (i < transitions.length) {
        return transitions(i)
      }
      if (iTailZone == null) {
        return _instant
      }
      val end = transitions(transitions.length - 1)
      if (_instant < end) {
        _instant = end
      }
      iTailZone.nextTransition(_instant)
    }

    def previousTransition(instant: Long): Long = {
      val transitions = iTransitions
      var i = Arrays.binarySearch(transitions.toArray, instant)
      if (i >= 0) {
        if (instant > Long.MinValue) {
          return instant - 1
        }
        return instant
      }
      i = ~i
      if (i < transitions.length) {
        if (i > 0) {
          val prev = transitions(i - 1)
          if (prev > Long.MinValue) {
            return prev - 1
          }
        }
        return instant
      }
      if (iTailZone != null) {
        val prev = iTailZone.previousTransition(instant)
        if (prev < instant) {
          return prev
        }
      }
      val prev = transitions(i - 1)
      if (prev > Long.MinValue) {
        return prev - 1
      }
      instant
    }

    override def equals(obj: Any): Boolean = {
      if (super.equals(obj)) {
        return true
      }
      if (obj.isInstanceOf[PrecalculatedZone]) {
        val other = obj.asInstanceOf[PrecalculatedZone]
        return getID == other.getID && iTransitions.sameElements(other.iTransitions) &&
          iNameKeys.sameElements(other.iNameKeys) &&
          iWallOffsets.sameElements(other.iWallOffsets) &&
          iStandardOffsets.sameElements(other.iStandardOffsets) &&
          (if (iTailZone == null) null == other.iTailZone else iTailZone == other.iTailZone)
      }
      false
    }

    def writeTo(out: DataOutput) {
      val size = iTransitions.length
      val poolSet = new collection.mutable.HashSet[String]()
      for (i <- 0 until size) {
        poolSet.add(iNameKeys(i))
      }
      val poolSize = poolSet.size
      if (poolSize > 65535) {
        throw new UnsupportedOperationException("String pool is too large")
      }
      val pool = js.Array[String]()
      val it = poolSet.iterator
      var i = 0
      while (it.hasNext) {
        pool(i) = it.next()
        i += 1
      }
      out.writeShort(poolSize)
      for (i <- 0 until poolSize) {
        out.writeUTF(pool(i))
      }
      out.writeInt(size)
      for (i <- 0 until size) {
        writeMillis(out, iTransitions(i))
        writeMillis(out, iWallOffsets(i))
        writeMillis(out, iStandardOffsets(i))
        val nameKey = iNameKeys(i)
        for (j <- 0 until poolSize if pool(j) == nameKey) {
          if (poolSize < 256) {
            out.writeByte(j)
          } else {
            out.writeShort(j)
          }
          break()
        }
      }
      out.writeBoolean(iTailZone != null)
      if (iTailZone != null) {
        iTailZone.writeTo(out)
      }
    }

    def isCachable(): Boolean = {
      if (iTailZone != null) {
        return true
      }
      val transitions = iTransitions
      if (transitions.length <= 1) {
        return false
      }
      var distances:Double = 0
      var count = 0
      for (i <- 1 until transitions.length) {
        val diff:Double = transitions(i) - transitions(i - 1)
        if (diff < ((366L + 365) * 24 * 60 * 60 * 1000)) {
          distances += diff.toDouble
          count += 1
        }
      }
      if (count > 0) {
        var avg = distances / count
        avg /= 24 * 60 * 60 * 1000
        if (avg >= 25) {
          return true
        }
      }
      false
    }
  }
}

class DateTimeZoneBuilder {

  private val iRuleSets = new collection.mutable.ListBuffer[RuleSet]()

  def addCutover(year: Int,
                 mode: Char,
                 monthOfYear: Int,
                 dayOfMonth: Int,
                 dayOfWeek: Int,
                 advanceDayOfWeek: Boolean,
                 millisOfDay: Int): DateTimeZoneBuilder = {
    if (iRuleSets.nonEmpty) {
      val ofYear = new OfYear(mode, monthOfYear, dayOfMonth, dayOfWeek, advanceDayOfWeek, millisOfDay)
      val lastRuleSet = iRuleSets.last
      lastRuleSet.setUpperLimit(year, ofYear)
    }
    iRuleSets += new RuleSet()
    this
  }

  def setStandardOffset(standardOffset: Int): DateTimeZoneBuilder = {
    getLastRuleSet.setStandardOffset(standardOffset)
    this
  }

  def setFixedSavings(nameKey: String, saveMillis: Int): DateTimeZoneBuilder = {
    getLastRuleSet.setFixedSavings(nameKey, saveMillis)
    this
  }

  def addRecurringSavings(nameKey: String,
                          saveMillis: Int,
                          fromYear: Int,
                          toYear: Int,
                          mode: Char,
                          monthOfYear: Int,
                          dayOfMonth: Int,
                          dayOfWeek: Int,
                          advanceDayOfWeek: Boolean,
                          millisOfDay: Int): DateTimeZoneBuilder = {
    if (fromYear <= toYear) {
      val ofYear = new OfYear(mode, monthOfYear, dayOfMonth, dayOfWeek, advanceDayOfWeek, millisOfDay)
      val recurrence = new Recurrence(ofYear, nameKey, saveMillis)
      val rule = new Rule(recurrence, fromYear, toYear)
      getLastRuleSet.addRule(rule)
    }
    this
  }

  private def getLastRuleSet(): RuleSet = {
    if (iRuleSets.isEmpty) {
      addCutover(Integer.MIN_VALUE, 'w', 1, 1, 0, advanceDayOfWeek = false, 0)
    }
    iRuleSets.last
  }

  def toDateTimeZone(id: String, outputID: Boolean): DateTimeZone = {
    if (id == null) {
      throw new IllegalArgumentException()
    }
    val transitions = new collection.mutable.ListBuffer[Transition]()
    var tailZone: DSTZone = null
    var millis = Long.MinValue
    var saveMillis = 0
    val ruleSetCount = iRuleSets.size
    for (i <- 0 until ruleSetCount) {
      var continueFlag = true
      var rs = iRuleSets(i)
      var next = rs.firstTransition(millis)
      if (next == null) {
        continueFlag = false
      }
      if (continueFlag) {
        addTransition(transitions, next)
        millis = next.getMillis
        saveMillis = next.getSaveMillis
        rs = new RuleSet(rs)
        while ( {
          next = rs.nextTransition(millis, saveMillis);next
        } != null) {
          if (addTransition(transitions, next)) {
            if (tailZone != null) {
              break()
            }
          }
          millis = next.getMillis
          saveMillis = next.getSaveMillis
          if (tailZone == null && i == ruleSetCount - 1) {
            tailZone = rs.buildTailZone(id)
          }
        }
        millis = rs.getUpperLimit(saveMillis)
      }
    }
    if (transitions.isEmpty) {
      if (tailZone != null) {
        return tailZone
      }
      return buildFixedZone(id, "UTC", 0, 0)
    }
    if (transitions.size == 1 && tailZone == null) {
      val tr = transitions.head
      return buildFixedZone(id, tr.getNameKey, tr.getWallOffset, tr.getStandardOffset)
    }
    val zone = PrecalculatedZone.create(id, outputID, transitions, tailZone)
    if (zone.isCachable) {
      return CachedDateTimeZone.forZone(zone)
    }
    zone
  }

  private def addTransition(transitions: collection.mutable.ListBuffer[Transition], tr: Transition): Boolean = {
    val size = transitions.size
    if (size == 0) {
      transitions += tr
      return true
    }
    val last = transitions(size - 1)
    if (!tr.isTransitionFrom(last)) {
      return false
    }
    var offsetForLast = 0
    if (size >= 2) {
      offsetForLast = transitions(size - 2).getWallOffset
    }
    val offsetForNew = last.getWallOffset
    val lastLocal = last.getMillis + offsetForLast
    val newLocal = tr.getMillis + offsetForNew
    if (newLocal != lastLocal) {
      transitions += tr
      return true
    }
    transitions.remove(size - 1)
    addTransition(transitions, tr)
  }

  def writeTo(zoneID: String, out: OutputStream) {
    if (out.isInstanceOf[DataOutput]) {
      writeTo(zoneID, out.asInstanceOf[DataOutput])
    } else {
      writeTo(zoneID, new DataOutputStream(out).asInstanceOf[DataOutput])
    }
  }

  def writeTo(zoneID: String, out: DataOutput) {
    var zone = toDateTimeZone(zoneID, outputID = false)
    if (zone.isInstanceOf[FixedDateTimeZone]) {
      out.writeByte('F')
      out.writeUTF(zone.getNameKey(0))
      writeMillis(out, zone.getOffset(0))
      writeMillis(out, zone.getStandardOffset(0))
    } else {
      if (zone.isInstanceOf[CachedDateTimeZone]) {
        out.writeByte('C')
        zone = zone.asInstanceOf[CachedDateTimeZone].getUncachedZone
      } else {
        out.writeByte('P')
      }
      zone.asInstanceOf[PrecalculatedZone].writeTo(out)
    }
  }
}
