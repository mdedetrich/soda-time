package org.joda.time.tz

import java.util.HashMap
import java.util.Locale
import java.util.Map
import org.joda.time.DateTimeUtils
import scala.util.control.Breaks._

class DefaultNameProvider extends NameProvider {

  private val iByLocaleCache: HashMap[Locale, Map[String, Map[String, Any]]] = new HashMap(7)

  private val iByLocaleCache2: HashMap[Locale, Map[String, Map[Boolean, Any]]] = new HashMap(7)

  def getShortName(locale: Locale, id: String, nameKey: String): String = {
    val nameSet = getNameSet(locale, id, nameKey)
    if (nameSet == null) null else nameSet(0)
  }

  def getName(locale: Locale, id: String, nameKey: String): String = {
    val nameSet = getNameSet(locale, id, nameKey)
    if (nameSet == null) null else nameSet(1)
  }

  private def getNameSet(locale: Locale, id: String, nameKey: String): Array[String] = {
    synchronized {
      if (locale == null || id == null || nameKey == null) {
        return null
      }
      var byIdCache = iByLocaleCache.get(locale)
      if (byIdCache == null) {
        byIdCache = new HashMap(7)
        iByLocaleCache.put(locale, byIdCache)
      }
      var byNameKeyCache = byIdCache.get(id)
      if (byNameKeyCache == null) {
        byNameKeyCache = new HashMap(7)
        byIdCache.put(id,byNameKeyCache)
        val zoneStringsEn = DateTimeUtils.getDateFormatSymbols(Locale.ENGLISH).getZoneStrings
        var setEn: Array[String] = null
        for (strings <- zoneStringsEn if strings != null && strings.length == 5 && id == strings(0)) {
          setEn = strings
          break()
        }
        val zoneStringsLoc = DateTimeUtils.getDateFormatSymbols(locale).getZoneStrings
        var setLoc: Array[String] = null
        for (strings <- zoneStringsLoc if strings != null && strings.length == 5 && id == strings(0)) {
          setLoc = strings
          break()
        }
        if (setEn != null && setLoc != null) {
          byNameKeyCache.put(setEn(2), Array(setLoc(2), setLoc(1)))
          if (setEn(2) == setEn(4)) {
            byNameKeyCache.put(setEn(4) + "-Summer", Array(setLoc(4), setLoc(3)))
          } else {
            byNameKeyCache.put(setEn(4), Array(setLoc(4), setLoc(3)))
          }
        }
      }
      byNameKeyCache.get(nameKey).asInstanceOf[Array[String]]
    }
  }

  def getShortName(locale: Locale,
                   id: String,
                   nameKey: String,
                   standardTime: Boolean): String = {
    val nameSet = getNameSet(locale, id, nameKey, standardTime)
    if (nameSet == null) null else nameSet(0)
  }

  def getName(locale: Locale,
              id: String,
              nameKey: String,
              standardTime: Boolean): String = {
    val nameSet = getNameSet(locale, id, nameKey, standardTime)
    if (nameSet == null) null else nameSet(1)
  }

  private def getNameSet(locale: Locale,
                         id: String,
                         nameKey: String,
                         standardTime: Boolean): Array[String] = {
    synchronized {
      if (locale == null || id == null || nameKey == null) {
        return null
      }
      var byIdCache = iByLocaleCache2.get(locale)
      if (byIdCache == null) {
        byIdCache = new HashMap(7)
        iByLocaleCache2.put(locale, byIdCache)
      }
      var byNameKeyCache = byIdCache.get(id)
      if (byNameKeyCache == null) {
        byNameKeyCache = new HashMap(7)
        byIdCache.put(id, byNameKeyCache)
        val zoneStringsEn = DateTimeUtils.getDateFormatSymbols(Locale.ENGLISH).getZoneStrings
        var setEn: Array[String] = null
        for (strings <- zoneStringsEn if strings != null && strings.length == 5 && id == strings(0)) {
          setEn = strings
          break()
        }
        val zoneStringsLoc = DateTimeUtils.getDateFormatSymbols(locale).getZoneStrings
        var setLoc: Array[String] = null
        for (strings <- zoneStringsLoc if strings != null && strings.length == 5 && id == strings(0)) {
          setLoc = strings
          break()
        }
        if (setEn != null && setLoc != null) {
          byNameKeyCache.put(true, Array(setLoc(2), setLoc(1)))
          byNameKeyCache.put(false, Array(setLoc(4), setLoc(3)))
        }
      }
      byNameKeyCache.get(Boolean.box(standardTime)).asInstanceOf[Array[String]]
    }
  }

}
