package org.joda.time.base

import java.io.Serializable
import org.joda.time.Chronology
import org.joda.time.DateTimeUtils
import org.joda.time.MutableInterval
import org.joda.time.ReadWritableInterval
import org.joda.time.ReadableDuration
import org.joda.time.ReadableInstant
import org.joda.time.ReadableInterval
import org.joda.time.ReadablePeriod
import org.joda.time.chrono.ISOChronology
import org.joda.time.convert.ConverterManager
import org.joda.time.field.FieldUtils

@SerialVersionUID(576586928732749278L)
abstract class BaseInterval protected ()
    extends AbstractInterval
    with ReadableInterval
    with Serializable {

  @volatile private var iChronology: Chronology = null
  @volatile private var iStartMillis: Long = _
  @volatile private var iEndMillis: Long = _

  protected def this(startInstant: Long,
                     endInstant: Long,
                     chrono: Chronology) {
    this()
    checkInterval(startInstant, endInstant)
    iChronology = DateTimeUtils.getChronology(chrono)
    iStartMillis = startInstant
    iEndMillis = endInstant
  }

  protected def this(start: ReadableInstant, end: ReadableInstant) {
    this()
    if (start == null && end == null) {
      iEndMillis = DateTimeUtils.currentTimeMillis()
      iStartMillis = iEndMillis
      iChronology = ISOChronology.getInstance
    } else {
      iChronology = DateTimeUtils.getInstantChronology(start)
      iStartMillis = DateTimeUtils.getInstantMillis(start)
      iEndMillis = DateTimeUtils.getInstantMillis(end)
      checkInterval(iStartMillis, iEndMillis)
    }
  }

  protected def this(start: ReadableInstant, duration: ReadableDuration) {
    this()
    iChronology = DateTimeUtils.getInstantChronology(start)
    iStartMillis = DateTimeUtils.getInstantMillis(start)
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    iEndMillis = FieldUtils.safeAdd(iStartMillis, durationMillis)
    checkInterval(iStartMillis, iEndMillis)
  }

  protected def this(duration: ReadableDuration, end: ReadableInstant) {
    this()
    iChronology = DateTimeUtils.getInstantChronology(end)
    iEndMillis = DateTimeUtils.getInstantMillis(end)
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    iStartMillis = FieldUtils.safeAdd(iEndMillis, -durationMillis)
    checkInterval(iStartMillis, iEndMillis)
  }

  protected def this(start: ReadableInstant, period: ReadablePeriod) {
    this()
    val chrono = DateTimeUtils.getInstantChronology(start)
    iChronology = chrono
    iStartMillis = DateTimeUtils.getInstantMillis(start)
    iEndMillis =
      if (period == null) iStartMillis else chrono.add(period, iStartMillis, 1)
    checkInterval(iStartMillis, iEndMillis)
  }

  protected def this(period: ReadablePeriod, end: ReadableInstant) {
    this()
    val chrono = DateTimeUtils.getInstantChronology(end)
    iChronology = chrono
    iEndMillis = DateTimeUtils.getInstantMillis(end)
    iStartMillis =
      if (period == null) iEndMillis else chrono.add(period, iEndMillis, -1)
    checkInterval(iStartMillis, iEndMillis)
  }

  protected def this(interval: AnyRef, chrono: Chronology) {
    this()
    val converter = ConverterManager.getInstance.getIntervalConverter(interval)
    if (converter.isReadableInterval(interval, chrono)) {
      val input = interval.asInstanceOf[ReadableInterval]
      iChronology = (if (chrono != null) chrono else input.getChronology)
      iStartMillis = input.getStartMillis
      iEndMillis = input.getEndMillis
    } else if (this.isInstanceOf[ReadWritableInterval]) {
      converter
        .setInto(this.asInstanceOf[ReadWritableInterval], interval, chrono)
    } else {
      val mi = new MutableInterval()
      converter.setInto(mi, interval, chrono)
      iChronology = mi.getChronology
      iStartMillis = mi.getStartMillis
      iEndMillis = mi.getEndMillis
    }
    checkInterval(iStartMillis, iEndMillis)
  }

  protected def auxConstructor(startInstant: Long,
                               endInstant: Long,
                               chrono: Chronology): Unit = {
    checkInterval(startInstant, endInstant)
    iChronology = DateTimeUtils.getChronology(chrono)
    iStartMillis = startInstant
    iEndMillis = endInstant
  }

  protected def auxConstructor(start: ReadableInstant,
                               end: ReadableInstant): Unit = {
    if (start == null && end == null) {
      iEndMillis = DateTimeUtils.currentTimeMillis()
      iStartMillis = iEndMillis
      iChronology = ISOChronology.getInstance
    } else {
      iChronology = DateTimeUtils.getInstantChronology(start)
      iStartMillis = DateTimeUtils.getInstantMillis(start)
      iEndMillis = DateTimeUtils.getInstantMillis(end)
      checkInterval(iStartMillis, iEndMillis)
    }
  }

  protected def auxConstructor(start: ReadableInstant,
                               duration: ReadableDuration): Unit = {
    iChronology = DateTimeUtils.getInstantChronology(start)
    iStartMillis = DateTimeUtils.getInstantMillis(start)
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    iEndMillis = FieldUtils.safeAdd(iStartMillis, durationMillis)
    checkInterval(iStartMillis, iEndMillis)
  }

  protected def auxConstructor(duration: ReadableDuration,
                               end: ReadableInstant): Unit = {
    iChronology = DateTimeUtils.getInstantChronology(end)
    iEndMillis = DateTimeUtils.getInstantMillis(end)
    val durationMillis = DateTimeUtils.getDurationMillis(duration)
    iStartMillis = FieldUtils.safeAdd(iEndMillis, -durationMillis)
    checkInterval(iStartMillis, iEndMillis)
  }

  protected def auxConstructor(start: ReadableInstant,
                               period: ReadablePeriod): Unit = {
    val chrono = DateTimeUtils.getInstantChronology(start)
    iChronology = chrono
    iStartMillis = DateTimeUtils.getInstantMillis(start)
    iEndMillis =
      if (period == null) iStartMillis else chrono.add(period, iStartMillis, 1)
    checkInterval(iStartMillis, iEndMillis)
  }

  protected def auxConstructor(period: ReadablePeriod,
                               end: ReadableInstant): Unit = {
    val chrono = DateTimeUtils.getInstantChronology(end)
    iChronology = chrono
    iEndMillis = DateTimeUtils.getInstantMillis(end)
    iStartMillis =
      if (period == null) iEndMillis else chrono.add(period, iEndMillis, -1)
    checkInterval(iStartMillis, iEndMillis)
  }

  protected def auxConstructor(interval: AnyRef, chrono: Chronology): Unit = {
    val converter = ConverterManager.getInstance.getIntervalConverter(interval)
    if (converter.isReadableInterval(interval, chrono)) {
      val input = interval.asInstanceOf[ReadableInterval]
      iChronology = if (chrono != null) chrono else input.getChronology
      iStartMillis = input.getStartMillis
      iEndMillis = input.getEndMillis
    } else if (this.isInstanceOf[ReadWritableInterval]) {
      converter
        .setInto(this.asInstanceOf[ReadWritableInterval], interval, chrono)
    } else {
      val mi = new MutableInterval()
      converter.setInto(mi, interval, chrono)
      iChronology = mi.getChronology
      iStartMillis = mi.getStartMillis
      iEndMillis = mi.getEndMillis
    }
    checkInterval(iStartMillis, iEndMillis)
  }

  def getChronology(): Chronology = iChronology

  def getStartMillis(): Long = iStartMillis

  def getEndMillis(): Long = iEndMillis

  protected def setInterval(startInstant: Long,
                            endInstant: Long,
                            chrono: Chronology) {
    checkInterval(startInstant, endInstant)
    iStartMillis = startInstant
    iEndMillis = endInstant
    iChronology = DateTimeUtils.getChronology(chrono)
  }
}
