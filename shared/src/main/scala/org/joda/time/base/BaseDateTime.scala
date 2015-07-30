package org.joda.time.base

import java.io.Serializable
import org.joda.time.Chronology
import org.joda.time.DateTimeUtils
import org.joda.time.DateTimeZone
import org.joda.time.ReadableDateTime
import org.joda.time.chrono.ISOChronology
import org.joda.time.convert.ConverterManager

@SerialVersionUID(-6728882245981L)
abstract class BaseDateTime(instant: Long, chronology: Chronology) extends AbstractDateTime with ReadableDateTime with Serializable {

  @volatile private var iMillis: Long = checkInstant(instant, chronology)
  @volatile private var iChronology: Chronology = checkChronology(chronology)

  if (iChronology.year().isSupported) {
    iChronology.year().set(iMillis, iChronology.year().get(iMillis))
  }
  
  def this() {
    this(DateTimeUtils.currentTimeMillis(), ISOChronology.getInstance)
  }

  def this(zone: DateTimeZone) {
    this(DateTimeUtils.currentTimeMillis(), ISOChronology.getInstance(zone))
  }

  def this(chronology: Chronology) {
    this(DateTimeUtils.currentTimeMillis(), chronology)
  }

  def this(instant: Long) {
    this(instant, ISOChronology.getInstance)
  }

  def this(instant: Long, zone: DateTimeZone) {
    this(instant, ISOChronology.getInstance(zone))
  }

  def this(instant: AnyRef, zone: DateTimeZone) {
    this()
    val converter = ConverterManager.getInstance.getInstantConverter(instant)
    val chrono = checkChronology(converter.getChronology(instant, zone))
    iChronology = chrono
    iMillis = checkInstant(converter.getInstantMillis(instant, chrono), chrono)
  }

  def this(instant: AnyRef, chronology: Chronology) {
    this()
    val converter = ConverterManager.getInstance.getInstantConverter(instant)
    iChronology = checkChronology(converter.getChronology(instant, chronology))
    iMillis = checkInstant(converter.getInstantMillis(instant, chronology), iChronology)
  }


  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int,
           chronology: Chronology) {
    this()
    iChronology = checkChronology(chronology)
    val instant = iChronology.getDateTimeMillis(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour,
      secondOfMinute, millisOfSecond)
    iMillis = checkInstant(instant, iChronology)
  }

  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int) {
    this(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond, ISOChronology.getInstance)
  }


  def this(year: Int,
           monthOfYear: Int,
           dayOfMonth: Int,
           hourOfDay: Int,
           minuteOfHour: Int,
           secondOfMinute: Int,
           millisOfSecond: Int,
           zone: DateTimeZone) {
    this(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond, ISOChronology.getInstance(zone))
  }


  protected def auxConstructor(chronology: Chronology): Unit = {
    iMillis = checkInstant(DateTimeUtils.currentTimeMillis(), chronology)
    iChronology = checkChronology(chronology)
  }

  protected def auxConstructor(zone: DateTimeZone): Unit = {
    val chronology = ISOChronology.getInstance(zone)
    iMillis = checkInstant(DateTimeUtils.currentTimeMillis(), chronology)
    iChronology = checkChronology(chronology)
  }

  protected def auxConstructor(instant: Long): Unit = {
    val chronology = ISOChronology.getInstance
    iMillis = checkInstant(instant, chronology)
    iChronology = checkChronology(chronology)
  }

  protected def auxConstructor(instant: Long, chronology: Chronology): Unit = {
    iMillis = checkInstant(instant, chronology)
    iChronology = checkChronology(chronology)
  }

  protected def auxConstructor(instant: AnyRef): Unit = {
    val converter = ConverterManager.getInstance.getInstantConverter(instant)
    val chrono = checkChronology(converter.getChronology(instant, null:Chronology))
    iChronology = chrono
    iMillis = checkInstant(converter.getInstantMillis(instant, chrono), chrono)
  }

  protected def auxConstructor(instant: AnyRef, zone: DateTimeZone): Unit = {
    val converter = ConverterManager.getInstance.getInstantConverter(instant)
    val chrono = checkChronology(converter.getChronology(instant, zone))
    iChronology = chrono
    iMillis = checkInstant(converter.getInstantMillis(instant, chrono), chrono)
  }

  protected def auxConstructor(instant: AnyRef, chronology: Chronology): Unit = {
    val converter = ConverterManager.getInstance.getInstantConverter(instant)
    iChronology = checkChronology(converter.getChronology(instant, chronology))
    iMillis = checkInstant(converter.getInstantMillis(instant, chronology), iChronology)
  }

  protected def auxConstructor(year: Int,
                               monthOfYear: Int,
                               dayOfMonth: Int,
                               hourOfDay: Int,
                               minuteOfHour: Int,
                               secondOfMinute: Int,
                               millisOfSecond: Int): Unit = {
    val chronology = ISOChronology.getInstance
    iChronology = checkChronology(chronology)
    val instant = iChronology.getDateTimeMillis(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour,
      secondOfMinute, millisOfSecond)
    iMillis = checkInstant(instant, iChronology)
  }

  protected def auxConstructor(year: Int,
                               monthOfYear: Int,
                               dayOfMonth: Int,
                               hourOfDay: Int,
                               minuteOfHour: Int,
                               secondOfMinute: Int,
                               millisOfSecond: Int,
                               chronology: Chronology): Unit = {
    iChronology = checkChronology(chronology)
    val instant = iChronology.getDateTimeMillis(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour,
      secondOfMinute, millisOfSecond)
    iMillis = checkInstant(instant, iChronology)
  }

  protected def auxConstructor(year: Int,
                               monthOfYear: Int,
                               dayOfMonth: Int,
                               hourOfDay: Int,
                               minuteOfHour: Int,
                               secondOfMinute: Int,
                               millisOfSecond: Int,
                               zone: DateTimeZone): Unit = {
    val chronology:Chronology = ISOChronology.getInstance(zone)
    iChronology = checkChronology(chronology)
    val instant = iChronology.getDateTimeMillis(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour,
      secondOfMinute, millisOfSecond)
    iMillis = checkInstant(instant, iChronology)
  }
  
  protected def auxConstructor(instant: Long, zone: DateTimeZone): Unit = {
    val chronology = ISOChronology.getInstance(zone)

    iMillis = checkInstant(instant, iChronology)
    iChronology = checkChronology(chronology)

    if (iChronology.year().isSupported) {
      iChronology.year().set(iMillis, iChronology.year().get(iMillis))
    }
  }
  
  protected def checkChronology(chronology: Chronology): Chronology = DateTimeUtils.getChronology(chronology)

  protected def checkInstant(instant: Long, chronology: Chronology): Long = instant

  def getMillis(): Long = iMillis

  def getChronology(): Chronology = iChronology

  protected def setMillis(instant: Long) {
    iMillis = checkInstant(instant, iChronology)
  }

  protected def setChronology(chronology: Chronology) {
    iChronology = checkChronology(chronology)
  }
}
