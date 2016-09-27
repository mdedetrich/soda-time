package org.joda.time.format

import scala.beans.BeanProperty

object DateTimeParserInternalParser {

  def of(underlying: DateTimeParser): InternalParser = {
    if (underlying.isInstanceOf[InternalParserDateTimeParser]) {
      return underlying.asInstanceOf[InternalParser]
    }
    if (underlying == null) {
      return null
    }
    new DateTimeParserInternalParser(underlying)
  }
}

class DateTimeParserInternalParser private (
    @BeanProperty val underlying: DateTimeParser)
    extends InternalParser {

  def estimateParsedLength(): Int = underlying.estimateParsedLength()

  def parseInto(bucket: DateTimeParserBucket,
                text: CharSequence,
                position: Int): Int = {
    underlying.parseInto(bucket, text.toString, position)
  }
}
