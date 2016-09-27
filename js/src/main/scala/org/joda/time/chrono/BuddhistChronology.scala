package org.joda.time.chrono

import org.joda.time.Chronology
import org.joda.time.DateTime
import org.joda.time.DateTimeConstants
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeZone
import org.joda.time.DurationFieldType
import org.joda.time.chrono.AssembledChronology.Fields
import org.joda.time.field.DelegatedDateTimeField
import org.joda.time.field.DividedDateTimeField
import org.joda.time.field.OffsetDateTimeField
import org.joda.time.field.RemainderDateTimeField
import org.joda.time.field.SkipUndoDateTimeField
import org.joda.time.field.UnsupportedDurationField
import BuddhistChronology._

object BuddhistChronology {

  val BE = DateTimeConstants.CE

  private val ERA_FIELD = new BasicSingleEraDateTimeField("BE")
  private val BUDDHIST_OFFSET = 543
  private val cCache =
    new collection.mutable.HashMap[DateTimeZone, BuddhistChronology]()
  private val INSTANCE_UTC = getInstance(DateTimeZone.UTC)

  def getInstanceUTC(): BuddhistChronology = INSTANCE_UTC

  def getInstance(): BuddhistChronology = getInstance(DateTimeZone.getDefault)

  def getInstance(zone: DateTimeZone): BuddhistChronology = {
    var _zone: DateTimeZone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    var chrono = cCache.get(_zone).orNull
    if (chrono == null) {
      chrono =
        new BuddhistChronology(GJChronology.getInstance(_zone, null), null)
      val lowerLimit = new DateTime(1, 1, 1, 0, 0, 0, 0, chrono)
      chrono = new BuddhistChronology(
        LimitChronology.getInstance(chrono, lowerLimit, null),
        "")
      val oldChrono = if (cCache.get(_zone).isEmpty) {
        cCache(_zone) = chrono
        chrono
      } else {
        chrono
      }

      if (oldChrono != null) {
        chrono = oldChrono
      }
    }
    chrono
  }
}

@SerialVersionUID(-3474595157769370126L)
class BuddhistChronology private (base: Chronology, param: AnyRef)
    extends AssembledChronology(base, param) {

  private def readResolve(): AnyRef = {
    val base = getBase
    if (base == null) getInstanceUTC else getInstance(base.getZone)
  }

  def withUTC(): Chronology = INSTANCE_UTC

  def withZone(zone: DateTimeZone): Chronology = {
    var _zone: DateTimeZone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    if (_zone == getZone) {
      return this
    }
    getInstance(_zone)
  }

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    }
    if (obj.isInstanceOf[BuddhistChronology]) {
      val chrono = obj.asInstanceOf[BuddhistChronology]
      return getZone == chrono.getZone
    }
    false
  }

  override def hashCode(): Int = {
    "Buddhist".hashCode * 11 + getZone.hashCode
  }

  override def toString(): String = {
    var str = "BuddhistChronology"
    val zone = getZone
    if (zone != null) {
      str = str + '[' + zone.getID + ']'
    }
    str
  }

  protected def assemble(fields: Fields) {
    if (getParam == null) {
      fields.eras =
        UnsupportedDurationField.getInstance(DurationFieldType.eras())
      var field = fields.year
      fields.year = new OffsetDateTimeField(
        new SkipUndoDateTimeField(this, field),
        BUDDHIST_OFFSET)
      field = fields.yearOfEra
      fields.yearOfEra = new DelegatedDateTimeField(
        fields.year,
        fields.eras,
        DateTimeFieldType.yearOfEra())
      field = fields.weekyear
      fields.weekyear = new OffsetDateTimeField(
        new SkipUndoDateTimeField(this, field),
        BUDDHIST_OFFSET)
      field = new OffsetDateTimeField(fields.yearOfEra, 99)
      fields.centuryOfEra = new DividedDateTimeField(
        field,
        fields.eras,
        DateTimeFieldType.centuryOfEra(),
        100)
      fields.centuries = fields.centuryOfEra.getDurationField
      field = new RemainderDateTimeField(
        fields.centuryOfEra.asInstanceOf[DividedDateTimeField])
      fields.yearOfCentury =
        new OffsetDateTimeField(field, DateTimeFieldType.yearOfCentury(), 1)
      field = new RemainderDateTimeField(fields.weekyear,
                                         fields.centuries,
                                         DateTimeFieldType.weekyearOfCentury(),
                                         100)
      fields.weekyearOfCentury = new OffsetDateTimeField(
        field,
        DateTimeFieldType.weekyearOfCentury(),
        1)
      fields.era = ERA_FIELD
    }
  }
}
