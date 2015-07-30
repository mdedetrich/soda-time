package org.joda.time.convert

import org.joda.time.JodaTimePermission

object ConverterManager {

  private var INSTANCE: ConverterManager = null

  def getInstance(): ConverterManager = {
    if (INSTANCE == null) {
      INSTANCE = new ConverterManager()
    }
    INSTANCE
  }
}

class ConverterManager {

  private var iInstantConverters: ConverterSet = new ConverterSet(Array(ReadableInstantConverter.INSTANCE, StringConverter.INSTANCE, CalendarConverter.INSTANCE, DateConverter.INSTANCE, LongConverter.INSTANCE, NullConverter.INSTANCE))
  private var iPartialConverters: ConverterSet = new ConverterSet(Array(ReadablePartialConverter.INSTANCE, ReadableInstantConverter.INSTANCE, StringConverter.INSTANCE, CalendarConverter.INSTANCE, DateConverter.INSTANCE, LongConverter.INSTANCE, NullConverter.INSTANCE))
  private var iDurationConverters: ConverterSet = new ConverterSet(Array(ReadableDurationConverter.INSTANCE, ReadableIntervalConverter.INSTANCE, StringConverter.INSTANCE, LongConverter.INSTANCE, NullConverter.INSTANCE))
  private var iPeriodConverters: ConverterSet = new ConverterSet(Array(ReadableDurationConverter.INSTANCE, ReadablePeriodConverter.INSTANCE, ReadableIntervalConverter.INSTANCE, StringConverter.INSTANCE, NullConverter.INSTANCE))
  private var iIntervalConverters: ConverterSet = new ConverterSet(Array(ReadableIntervalConverter.INSTANCE, StringConverter.INSTANCE, NullConverter.INSTANCE))

  def getInstantConverter(`object`: AnyRef): InstantConverter = {
    val converter = iInstantConverters.select(if (`object` == null) null else `object`.getClass).asInstanceOf[InstantConverter]
    if (converter != null) {
      return converter
    }
    throw new IllegalArgumentException("No instant converter found for type: " +
      (if (`object` == null) "null" else `object`.getClass.getName))
  }

  def getInstantConverters(): Array[InstantConverter] = {
    val set = iInstantConverters
    val converters: Array[InstantConverter] = Array.ofDim[InstantConverter](set.size)
    set.copyInto(converters)
    converters
  }

  def addInstantConverter(converter: InstantConverter): InstantConverter = {
    checkAlterInstantConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[InstantConverter](1)
    iInstantConverters = iInstantConverters.add(converter, removed)
    removed(0)
  }

  def removeInstantConverter(converter: InstantConverter): InstantConverter = {
    checkAlterInstantConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[InstantConverter](1)
    iInstantConverters = iInstantConverters.remove(converter, removed)
    removed(0)
  }

  private def checkAlterInstantConverters() {
    val sm = System.getSecurityManager
    if (sm != null) {
      sm.checkPermission(new JodaTimePermission("ConverterManager.alterInstantConverters"))
    }
  }

  def getPartialConverter(`object`: AnyRef): PartialConverter = {
    val converter = iPartialConverters.select(if (`object` == null) null else `object`.getClass).asInstanceOf[PartialConverter]
    if (converter != null) {
      return converter
    }
    throw new IllegalArgumentException("No partial converter found for type: " +
      (if (`object` == null) "null" else `object`.getClass.getName))
  }

  def getPartialConverters(): Array[PartialConverter] = {
    val set = iPartialConverters
    val converters = Array.ofDim[PartialConverter](set.size)
    set.copyInto(converters)
    converters
  }

  def addPartialConverter(converter: PartialConverter): PartialConverter = {
    checkAlterPartialConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[PartialConverter](1)
    iPartialConverters = iPartialConverters.add(converter, removed)
    removed(0)
  }

  def removePartialConverter(converter: PartialConverter): PartialConverter = {
    checkAlterPartialConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[PartialConverter](1)
    iPartialConverters = iPartialConverters.remove(converter, removed)
    removed(0)
  }

  private def checkAlterPartialConverters() {
    val sm = System.getSecurityManager
    if (sm != null) {
      sm.checkPermission(new JodaTimePermission("ConverterManager.alterPartialConverters"))
    }
  }

  def getDurationConverter(`object`: AnyRef): DurationConverter = {
    val converter = iDurationConverters.select(if (`object` == null) null else `object`.getClass).asInstanceOf[DurationConverter]
    if (converter != null) {
      return converter
    }
    throw new IllegalArgumentException("No duration converter found for type: " +
      (if (`object` == null) "null" else `object`.getClass.getName))
  }

  def getDurationConverters(): Array[DurationConverter] = {
    val set = iDurationConverters
    val converters = Array.ofDim[DurationConverter](set.size)
    set.copyInto(converters)
    converters
  }

  def addDurationConverter(converter: DurationConverter): DurationConverter = {
    checkAlterDurationConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[DurationConverter](1)
    iDurationConverters = iDurationConverters.add(converter, removed)
    removed(0)
  }

  def removeDurationConverter(converter: DurationConverter): DurationConverter = {
    checkAlterDurationConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[DurationConverter](1)
    iDurationConverters = iDurationConverters.remove(converter, removed)
    removed(0)
  }

  private def checkAlterDurationConverters() {
    val sm = System.getSecurityManager
    if (sm != null) {
      sm.checkPermission(new JodaTimePermission("ConverterManager.alterDurationConverters"))
    }
  }

  def getPeriodConverter(`object`: AnyRef): PeriodConverter = {
    val converter = iPeriodConverters.select(if (`object` == null) null else `object`.getClass).asInstanceOf[PeriodConverter]
    if (converter != null) {
      return converter
    }
    throw new IllegalArgumentException("No period converter found for type: " +
      (if (`object` == null) "null" else `object`.getClass.getName))
  }

  def getPeriodConverters(): Array[PeriodConverter] = {
    val set = iPeriodConverters
    val converters = Array.ofDim[PeriodConverter](set.size)
    set.copyInto(converters)
    converters
  }

  def addPeriodConverter(converter: PeriodConverter): PeriodConverter = {
    checkAlterPeriodConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[PeriodConverter](1)
    iPeriodConverters = iPeriodConverters.add(converter, removed)
    removed(0)
  }

  def removePeriodConverter(converter: PeriodConverter): PeriodConverter = {
    checkAlterPeriodConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[PeriodConverter](1)
    iPeriodConverters = iPeriodConverters.remove(converter, removed)
    removed(0)
  }

  private def checkAlterPeriodConverters() {
    val sm = System.getSecurityManager
    if (sm != null) {
      sm.checkPermission(new JodaTimePermission("ConverterManager.alterPeriodConverters"))
    }
  }

  def getIntervalConverter(`object`: AnyRef): IntervalConverter = {
    val converter = iIntervalConverters.select(if (`object` == null) null else `object`.getClass).asInstanceOf[IntervalConverter]
    if (converter != null) {
      return converter
    }
    throw new IllegalArgumentException("No interval converter found for type: " +
      (if (`object` == null) "null" else `object`.getClass.getName))
  }

  def getIntervalConverters(): Array[IntervalConverter] = {
    val set = iIntervalConverters
    val converters = Array.ofDim[IntervalConverter](set.size)
    set.copyInto(converters)
    converters
  }

  def addIntervalConverter(converter: IntervalConverter): IntervalConverter = {
    checkAlterIntervalConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[IntervalConverter](1)
    iIntervalConverters = iIntervalConverters.add(converter, removed)
    removed(0)
  }

  def removeIntervalConverter(converter: IntervalConverter): IntervalConverter = {
    checkAlterIntervalConverters()
    if (converter == null) {
      return null
    }
    val removed = Array.ofDim[IntervalConverter](1)
    iIntervalConverters = iIntervalConverters.remove(converter, removed)
    removed(0)
  }

  private def checkAlterIntervalConverters() {
    val sm = System.getSecurityManager
    if (sm != null) {
      sm.checkPermission(new JodaTimePermission("ConverterManager.alterIntervalConverters"))
    }
  }

  override def toString(): String = {
    "ConverterManager[" + iInstantConverters.size + " instant," +
      iPartialConverters.size +
      " partial," +
      iDurationConverters.size +
      " duration," +
      iPeriodConverters.size +
      " period," +
      iIntervalConverters.size +
      " interval]"
  }
}
