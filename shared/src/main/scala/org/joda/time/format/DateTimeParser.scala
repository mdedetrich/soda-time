package org.joda.time.format

trait DateTimeParser {

  def estimateParsedLength(): Int

  def parseInto(bucket: DateTimeParserBucket, text: String, position: Int): Int
}
