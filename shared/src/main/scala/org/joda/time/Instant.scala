package org.joda.time

import java.io.Serializable
import org.joda.convert.FromString
import org.joda.time.base.AbstractInstant
import org.joda.time.chrono.ISOChronology
import org.joda.time.convert.ConverterManager
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}

object Instant {

  def now(): Instant = new Instant()

  @FromString
  def parse(str: String): Instant = {
    parse(str, ISODateTimeFormat.dateTimeParser())
  }

  def parse(str: String, formatter: DateTimeFormatter): Instant = {
    formatter.parseDateTime(str).toInstant()
  }
}

@SerialVersionUID(3299096530934209741L)
class Instant
    extends AbstractInstant()
    with ReadableInstant
    with Serializable {

  private var iMillis = DateTimeUtils.currentTimeMillis()

  def this(instant: Long) {
    this()
    iMillis = instant
  }

  def this(instant: AnyRef) {
    this()
    val converter = ConverterManager.getInstance.getInstantConverter(instant)
    iMillis = converter.getInstantMillis(instant, ISOChronology.getInstanceUTC)
  }

  override def toInstant(): Instant = this

  def withMillis(newMillis: Long): Instant = {
    if (newMillis == iMillis) this else new Instant(newMillis)
  }

  def withDurationAdded(durationToAdd: Long, scalar: Int): Instant = {
    if (durationToAdd == 0 || scalar == 0) {
      return this
    }
    val instant = getChronology.add(getMillis, durationToAdd, scalar)
    withMillis(instant)
  }

  def withDurationAdded(durationToAdd: ReadableDuration,
                        scalar: Int): Instant = {
    if (durationToAdd == null || scalar == 0) {
      return this
    }
    withDurationAdded(durationToAdd.getMillis, scalar)
  }

  def plus(duration: Long): Instant = withDurationAdded(duration, 1)

  def plus(duration: ReadableDuration): Instant =
    withDurationAdded(duration, 1)

  def minus(duration: Long): Instant = withDurationAdded(duration, -1)

  def minus(duration: ReadableDuration): Instant =
    withDurationAdded(duration, -1)

  def getMillis(): Long = iMillis

  def getChronology(): Chronology = ISOChronology.getInstanceUTC

  override def toDateTime(): DateTime = {
    new DateTime(getMillis, ISOChronology.getInstance)
  }

  @Deprecated
  override def toDateTimeISO(): DateTime = toDateTime()

  override def toMutableDateTime(): MutableDateTime = {
    new MutableDateTime(getMillis, ISOChronology.getInstance)
  }

  @Deprecated
  override def toMutableDateTimeISO(): MutableDateTime = toMutableDateTime()
}
