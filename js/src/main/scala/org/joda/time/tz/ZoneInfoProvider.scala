package org.joda.time.tz

import java.io.DataInputStream
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import java.lang.ref.SoftReference
import java.util.Set
import org.joda.time.DateTimeZone
import ZoneInfoProvider._
import scala.scalajs.js
import scala.collection.JavaConversions._

object ZoneInfoProvider {
  
  private def loadZoneInfoMap(in:InputStream):collection.mutable.Map[String,AnyRef] = {
    val map = new collection.mutable.HashMap[String,AnyRef]()
    val din = new DataInputStream(in)
    try {
      readZoneInfoMap(din, map)
      map("UTC") = new SoftReference[DateTimeZone](DateTimeZone.UTC)
      map
    } finally {
      try {
        din.close()
      } catch {
        case ex: IOException =>
      }
    }
  }
  

  private def readZoneInfoMap(din: DataInputStream, zimap: collection.mutable.Map[String, AnyRef]) {
    var size = din.readUnsignedShort()
    val pool = js.Array[String]()
    for (i <- 0 until size) {
      pool(i) = din.readUTF().intern()
    }
    size = din.readUnsignedShort()
    for (i <- 0 until size) {
      zimap(pool(din.readUnsignedShort())) = pool(din.readUnsignedShort())
    }
  }
}

class ZoneInfoProvider extends Provider {

  private var iResourcePath: String = null
  private var iLoader: ClassLoader = null
  private var iZoneInfoMap: collection.mutable.Map[String,AnyRef] = loadZoneInfoMap(openResource("ZoneInfoMap"))
  private var iZoneInfoKeys = iZoneInfoMap.keySet.to[collection.immutable.TreeSet]
  private var iFileDir: File = null

  def this(fileDir:File) {
    this()
    if (fileDir == null) {
      throw new IllegalArgumentException("No file directory provided")
    }

    if (!fileDir.exists()) {
      throw new IOException("File directory doesn't exist: " + fileDir)
    }

    if (!fileDir.isDirectory) {
      throw new IOException("File doesn't refer to a directory: " + fileDir)
    }

    iFileDir = fileDir
    iResourcePath = null
    iLoader = null
  }

  private def this(resourcePath: String, loader: ClassLoader, favorSystemLoader: Boolean) {
    this()
    var _loader: ClassLoader = loader
    var _resourcePath: String = resourcePath
    if (_resourcePath == null) {
      throw new IllegalArgumentException("No resource path provided")
    }
    if (!_resourcePath.endsWith("/")) {
      _resourcePath += '/'
    }
    iFileDir = null
    iResourcePath = _resourcePath
    if (_loader == null && !favorSystemLoader) {
      _loader = getClass.getClassLoader
    }
    iLoader = _loader
    iZoneInfoMap = loadZoneInfoMap(openResource("ZoneInfoMap"))
    iZoneInfoKeys = iZoneInfoMap.keySet.to[collection.immutable.TreeSet]
  }

  def this(resourcePath: String) {
    this(resourcePath, null, false)
  }

  def this(resourcePath: String, loader: ClassLoader) {
    this(resourcePath, loader, true)
  }

  def getZone(id: String): DateTimeZone = {
    if (id == null) {
      return null
    }
    val obj = iZoneInfoMap(id)
    if (obj == null) {
      return null
    }
    if (obj.isInstanceOf[SoftReference[_]]) {
      val ref = obj.asInstanceOf[SoftReference[DateTimeZone]]
      val tz = ref.get
      if (tz != null) {
        return tz
      }
      return loadZoneData(id)
    } else if (id == obj) {
      return loadZoneData(id)
    }
    getZone(obj.asInstanceOf[String])
  }

  def getAvailableIDs(): Set[String] = iZoneInfoKeys

  protected def uncaughtException(ex: Exception) {
    ex.printStackTrace()
  }

  private def openResource(name: String): InputStream = {
    var in: InputStream = null
    if (iFileDir != null) {
      in = new FileInputStream(new File(iFileDir, name))
    } else {
      val path = iResourcePath.concat(name)
      in = if (iLoader != null) iLoader.getResourceAsStream(path) else ClassLoader.getSystemResourceAsStream(path)
      if (in == null) {
        val buf = new StringBuilder(40).append("Resource not found: \"")
          .append(path)
          .append("\" ClassLoader: ")
          .append(if (iLoader != null) iLoader.toString else "system")
        throw new IOException(buf.toString)
      }
    }
    in
  }

  private def loadZoneData(id: String): DateTimeZone = {
    var in: InputStream = null
    try {
      in = openResource(id)
      val tz = DateTimeZoneBuilder.readFrom(in, id)
      iZoneInfoMap.put(id, new SoftReference[DateTimeZone](tz))
      tz
    } catch {
      case ex: IOException =>
        uncaughtException(ex)
        iZoneInfoMap.remove(id)
        null
    } finally {
      try {
        if (in != null) {
          in.close()
        }
      } catch {
        case ex: IOException =>
      }
    }
  }
}
