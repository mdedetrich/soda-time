package org.joda.time.base

import java.io.Serializable
import java.util.Locale
import org.joda.time.Chronology
import org.joda.time.DateTimeUtils
import org.joda.time.ReadablePartial
import org.joda.time.convert.ConverterManager
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter

@SerialVersionUID(2353678632973660L)
abstract class BasePartial protected (instant: Long, chronology: Chronology)
  extends AbstractPartial with ReadablePartial with Serializable {

  private var iChronology:Chronology = _
  private var iValues:Array[Int] = _
  
  var _chronology:Chronology = chronology

  _chronology = DateTimeUtils.getChronology(_chronology)
  iChronology = chronology.withUTC()
  iValues = _chronology.get(this, instant)

  protected def this() {
    this(DateTimeUtils.currentTimeMillis(), null)
  }

  protected def this(chronology: Chronology) {
    this(DateTimeUtils.currentTimeMillis(), chronology)
  }

  protected def this(instant: Long) {
    this(instant, null)
  }

  protected def this(instant: AnyRef, chronology: Chronology) {
    this()
    var _chronology = chronology
    val converter = ConverterManager.getInstance.getPartialConverter(instant)
    _chronology = converter.getChronology(instant, _chronology)
    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = _chronology.withUTC()
    iValues = converter.getPartialValues(this, instant, _chronology)
  }

  protected def this(instant: AnyRef, chronology: Chronology, parser: DateTimeFormatter) {
    this()
    var _chronology = chronology
    val converter = ConverterManager.getInstance.getPartialConverter(instant)
    _chronology = converter.getChronology(instant, _chronology)
    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = _chronology.withUTC()
    iValues = converter.getPartialValues(this, instant, _chronology, parser)
  }

  protected def this(values: Array[Int], chronology: Chronology) {
    this()
    var _chronology = chronology
    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = _chronology.withUTC()
    _chronology.validate(this, values)
    iValues = values
  }

  protected def this(base: BasePartial, values: Array[Int]) {
    this()
    iChronology = base.iChronology
    iValues = values
  }

  protected def this(base: BasePartial, chrono: Chronology) {
    this()
    iChronology = chrono.withUTC()
    iValues = base.iValues
  }
  
  protected def this(dummyImplicit: DummyImplicit) {
    this()
  }
  
  protected def auxConstructor(chronology: Chronology): Unit = {
    var _chronology:Chronology = chronology

    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = chronology.withUTC()
    iValues = _chronology.get(this, instant)
  }
  
  protected def auxConstructor(instant:Long): Unit = {
    var _chronology:Chronology = null

    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = chronology.withUTC()
    iValues = _chronology.get(this, instant)
  }
  
  protected def auxConstructor(instant:Long,chronology: Chronology): Unit = {
    var _chronology:Chronology = chronology

    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = chronology.withUTC()
    iValues = _chronology.get(this, instant)
  }
  
  protected def auxConstructor(instant: AnyRef, chronology: Chronology, parser: DateTimeFormatter): Unit = {
    var _chronology = chronology
    val converter = ConverterManager.getInstance.getPartialConverter(instant)
    _chronology = converter.getChronology(instant, _chronology)
    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = _chronology.withUTC()
    iValues = converter.getPartialValues(this, instant, _chronology, parser)
  }
  
  protected def auxConstructor(values: Array[Int], chronology: Chronology): Unit = {
    var _chronology = chronology
    _chronology = DateTimeUtils.getChronology(_chronology)
    iChronology = _chronology.withUTC()
    _chronology.validate(this, values)
    iValues = values
  }
  
  protected def auxConstructor(base: BasePartial, values: Array[Int]): Unit = {
    iChronology = base.iChronology
    iValues = values
  }
  
  protected def auxConstructor(base: BasePartial, chrono: Chronology): Unit = {
    iChronology = chrono.withUTC()
    iValues = base.iValues
  }

  def getValue(index: Int): Int = iValues(index)

  override def getValues(): Array[Int] = {
    iValues.clone().asInstanceOf[Array[Int]]
  }

  def getChronology(): Chronology = iChronology

  protected def setValue(index: Int, value: Int) {
    val field = getField(index)
    val values = field.set(this, index, iValues, value)
    System.arraycopy(values, 0, iValues, 0, iValues.length)
  }

  protected def setValues(values: Array[Int]) {
    getChronology.validate(this, values)
    System.arraycopy(values, 0, iValues, 0, iValues.length)
  }

  def toString(pattern: String): String = {
    if (pattern == null) {
      return toString
    }
    DateTimeFormat.forPattern(pattern).print(this)
  }

  def toString(pattern: String, locale: Locale): String = {
    if (pattern == null) {
      return toString
    }
    DateTimeFormat.forPattern(pattern).withLocale(locale)
      .print(this)
  }
}
