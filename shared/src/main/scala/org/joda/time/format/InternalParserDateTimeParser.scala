package org.joda.time.format

object InternalParserDateTimeParser {

  def of(underlying: InternalParser): DateTimeParser = {
    if (underlying.isInstanceOf[DateTimeParserInternalParser]) {
      return underlying
        .asInstanceOf[DateTimeParserInternalParser]
        .getUnderlying
    }
    if (underlying.isInstanceOf[DateTimeParser]) {
      return underlying.asInstanceOf[DateTimeParser]
    }
    if (underlying == null) {
      return null
    }
    new InternalParserDateTimeParser(underlying)
  }
}

class InternalParserDateTimeParser private (
    private val underlying: InternalParser)
    extends DateTimeParser
    with InternalParser {

  def estimateParsedLength(): Int = underlying.estimateParsedLength()

  def parseInto(bucket: DateTimeParserBucket,
                text: CharSequence,
                position: Int): Int = {
    underlying.parseInto(bucket, text, position)
  }

  def parseInto(bucket: DateTimeParserBucket,
                text: String,
                position: Int): Int = {
    underlying.parseInto(bucket, text, position)
  }

  override def equals(obj: Any): Boolean = {
    if (obj == this) {
      return true
    }
    if (obj.isInstanceOf[InternalParserDateTimeParser]) {
      val other = obj.asInstanceOf[InternalParserDateTimeParser]
      return underlying == other.underlying
    }
    false
  }
}
