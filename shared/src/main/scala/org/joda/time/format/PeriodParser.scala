package org.joda.time.format

import java.util.Locale
import org.joda.time.ReadWritablePeriod

trait PeriodParser {
  def parseInto(period: ReadWritablePeriod,
                periodStr: String,
                position: Int,
                locale: Locale): Int
}
