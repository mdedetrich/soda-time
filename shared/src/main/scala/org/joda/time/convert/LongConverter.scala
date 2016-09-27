package org.joda.time.convert
import org.joda.time.Chronology

object LongConverter {

  val INSTANCE = new LongConverter()
}

class LongConverter
    extends AbstractConverter()
    with InstantConverter
    with PartialConverter
    with DurationConverter {

  override def getInstantMillis(`object`: AnyRef, chrono: Chronology): Long = {
    `object`.asInstanceOf[Long].longValue()
  }

  def getDurationMillis(`object`: AnyRef): Long = {
    `object`.asInstanceOf[Long].longValue()
  }

  def getSupportedType(): Class[_] = classOf[Long]
}
