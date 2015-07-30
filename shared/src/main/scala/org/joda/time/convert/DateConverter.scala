package org.joda.time.convert

import java.util.Date
import org.joda.time.Chronology

object DateConverter {
  val INSTANCE = new DateConverter()
}

class DateConverter extends AbstractConverter() with InstantConverter with PartialConverter {

  override def getInstantMillis(`object`: AnyRef, chrono: Chronology): Long = {
    val date = `object`.asInstanceOf[Date]
    date.getTime
  }

  def getSupportedType(): Class[_] = classOf[Date]
}
