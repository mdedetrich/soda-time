package org.joda.time.convert

trait Converter {

  def getSupportedType(): Class[_]
}
