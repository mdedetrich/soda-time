package org.joda.time.base

import java.io.Serializable
import org.joda.time.Chronology
import org.joda.time.DateTimeUtils
import org.joda.time.Interval
import org.joda.time.Period
import org.joda.time.PeriodType
import org.joda.time.ReadableDuration
import org.joda.time.ReadableInstant
import org.joda.time.convert.ConverterManager
import org.joda.time.field.FieldUtils

@SerialVersionUID(2581698638990L)
abstract class BaseDuration protected (private val millis: Long) extends AbstractDuration() with ReadableDuration with Serializable {

  @volatile private var iMillis: Long = millis
  
  protected def auxConstructor(duration: Long): Unit = {
    iMillis = millis
  }
  
  protected def auxConstructor(duration: AnyRef): Unit = {
    iMillis = ConverterManager.getInstance.getDurationConverter(duration).getDurationMillis(duration)  
  }
  
  protected def auxConstructor(startInstant: Long, endInstant: Long):Unit = {
    iMillis = FieldUtils.safeSubtract(endInstant, startInstant)
  }
  
  protected def auxConstructor(start: ReadableInstant, end: ReadableInstant):Unit = {
    iMillis = if (start == end) {
      0L
    } else {
      val startMillis = DateTimeUtils.getInstantMillis(start)
      val endMillis = DateTimeUtils.getInstantMillis(end)
      FieldUtils.safeSubtract(endMillis, startMillis)
    }
  }
  
  protected def this(duration: AnyRef) {
    this(ConverterManager.getInstance.getDurationConverter(duration).getDurationMillis(duration))
  }

  protected def this(startInstant: Long, endInstant: Long) {
    this(FieldUtils.safeSubtract(endInstant, startInstant))
  }

  protected def this(start: ReadableInstant, end: ReadableInstant) {
    this {
      if (start == end) {
        0L
      } else {
        val startMillis = DateTimeUtils.getInstantMillis(start)
        val endMillis = DateTimeUtils.getInstantMillis(end)
        FieldUtils.safeSubtract(endMillis, startMillis)
      }
    }
  }

  def getMillis(): Long = iMillis

  protected def setMillis(duration: Long) {
    iMillis = duration
  }

  def toPeriod(`type`: PeriodType): Period = new Period(getMillis, `type`)

  def toPeriod(chrono: Chronology): Period = new Period(getMillis, chrono)

  def toPeriod(`type`: PeriodType, chrono: Chronology): Period = new Period(getMillis, `type`, chrono)

  def toPeriodFrom(startInstant: ReadableInstant): Period = new Period(startInstant, this)

  def toPeriodFrom(startInstant: ReadableInstant, `type`: PeriodType): Period = new Period(startInstant, this, `type`)

  def toPeriodTo(endInstant: ReadableInstant): Period = new Period(this, endInstant)

  def toPeriodTo(endInstant: ReadableInstant, `type`: PeriodType): Period = new Period(this, endInstant, `type`)

  def toIntervalFrom(startInstant: ReadableInstant): Interval = new Interval(startInstant, this)

  def toIntervalTo(endInstant: ReadableInstant): Interval = new Interval(this, endInstant)
}
