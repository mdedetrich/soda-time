package org.joda.time.convert

trait DurationConverter extends Converter {
  def getDurationMillis(`object`: AnyRef): Long
}
