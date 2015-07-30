package org.joda.time.format

trait InternalParser {

  def estimateParsedLength(): Int

  def parseInto(bucket: DateTimeParserBucket, text: CharSequence, position: Int): Int
}
