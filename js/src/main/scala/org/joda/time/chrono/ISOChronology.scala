package org.joda.time.chrono

import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.Serializable
import org.joda.time.Chronology
import org.joda.time.DateTimeFieldType
import org.joda.time.DateTimeZone
import org.joda.time.chrono.AssembledChronology.Fields
import org.joda.time.field.DividedDateTimeField
import org.joda.time.field.RemainderDateTimeField
import ISOChronology._

object ISOChronology {

  private val INSTANCE_UTC = new ISOChronology(GregorianChronology.getInstanceUTC)

  private val cCache = new collection.mutable.HashMap[DateTimeZone, ISOChronology]()

  cCache.put(DateTimeZone.UTC, INSTANCE_UTC)

  def getInstanceUTC(): ISOChronology = INSTANCE_UTC

  def getInstance(): ISOChronology = getInstance(DateTimeZone.getDefault)

  def getInstance(zone: DateTimeZone): ISOChronology = {
    var _zone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    var chrono = cCache.get(_zone).orNull
    if (chrono == null) {
      chrono = new ISOChronology(ZonedChronology.getInstance(INSTANCE_UTC, _zone))
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

  @SerialVersionUID(-6212696554273812441L)
  private class Stub(@transient private var iZone: DateTimeZone) extends Serializable {

    private def readResolve(): AnyRef = ISOChronology.getInstance(iZone)

    private def writeObject(out: ObjectOutputStream) {
      out.writeObject(iZone)
    }

    private def readObject(in: ObjectInputStream) {
      iZone = in.readObject().asInstanceOf[DateTimeZone]
    }
  }
}

@SerialVersionUID(-6212696554273812441L)
class ISOChronology private (base: Chronology) extends AssembledChronology(base, null) {

  def withUTC(): Chronology = INSTANCE_UTC

  def withZone(zone: DateTimeZone): Chronology = {
    var _zone = zone
    if (_zone == null) {
      _zone = DateTimeZone.getDefault
    }
    if (_zone == getZone) {
      return this
    }
    getInstance(_zone)
  }

  override def toString(): String = {
    var str = "ISOChronology"
    val zone = getZone
    if (zone != null) {
      str = str + '[' + zone.getID + ']'
    }
    str
  }

  protected def assemble(fields: Fields) {
    if (getBase.getZone == DateTimeZone.UTC) {
      fields.centuryOfEra = new DividedDateTimeField(ISOYearOfEraDateTimeField.INSTANCE, DateTimeFieldType.centuryOfEra(),
        100)
      fields.centuries = fields.centuryOfEra.getDurationField
      fields.yearOfCentury = new RemainderDateTimeField(fields.centuryOfEra.asInstanceOf[DividedDateTimeField],
        DateTimeFieldType.yearOfCentury())
      fields.weekyearOfCentury = new RemainderDateTimeField(fields.centuryOfEra.asInstanceOf[DividedDateTimeField],
        fields.weekyears, DateTimeFieldType.weekyearOfCentury())
    }
  }

  override def equals(obj: Any): Boolean = {
    if (super.equals(obj)) {
      return true
    }
    if (obj.isInstanceOf[ISOChronology]) {
      val chrono = obj.asInstanceOf[ISOChronology]
      return getZone == chrono.getZone
    }
    false
  }

  override def hashCode(): Int = "ISO".hashCode * 11 + getZone.hashCode

  private def writeReplace(): AnyRef = new Stub(getZone)
}
