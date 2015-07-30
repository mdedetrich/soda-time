package org.joda.time.tz

import java.util.Locale

trait NameProvider {

  def getShortName(locale: Locale, id: String, nameKey: String): String

  def getName(locale: Locale, id: String, nameKey: String): String
}
