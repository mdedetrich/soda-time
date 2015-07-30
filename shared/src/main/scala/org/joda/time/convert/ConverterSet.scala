package org.joda.time.convert
import org.joda.time.convert.ConverterSet._

object ConverterSet {

  private def selectSlow(set: ConverterSet, `type`: Class[_]): Converter = {
    var _set: ConverterSet = set
    var converters = _set.iConverters
    var length = converters.length
    var converter: Converter = null
    
    {
      val i = length
      while (i >= 0) {
        converter = converters(i)
        val supportedType = converter.getSupportedType
        if (supportedType == `type`) {
          return converter
        }
        if (supportedType == null ||
          (`type` != null && !supportedType.isAssignableFrom(`type`))) {
          _set = _set.remove(i, null)
          converters = _set.iConverters
          length = converters.length
        }
      }
    }

    if (`type` == null || length == 0) {
      return null
    }
    if (length == 1) {
      return converters(0)
    }
    {
      var i = length
      while (i >= 0) {
        converter = converters(i)
        val supportedType = converter.getSupportedType
        val j = length
        while (j >= 0) {
          if (j != i &&
            converters(j).getSupportedType.isAssignableFrom(supportedType)) {
            _set = _set.remove(j, null)
            converters = _set.iConverters
            length = converters.length
            i = length - 1
          }
        }
      }
    }

    if (length == 1) {
      return converters(0)
    }
    val msg = new StringBuilder()
    msg.append("Unable to find best converter for type \"")
    msg.append(`type`.getName)
    msg.append("\" from remaining set: ")
    for (i <- 0 until length) {
      converter = converters(i)
      val supportedType = converter.getSupportedType
      msg.append(converter.getClass.getName)
      msg.append('[')
      msg.append(if (supportedType == null) null else supportedType.getName)
      msg.append("], ")
    }
    throw new IllegalStateException(msg.toString)
  }

  class Entry(val iType: Class[_], val iConverter: Converter)
}

class ConverterSet(private val iConverters: Array[Converter]) {

  private var iSelectEntries: Array[Entry] = new Array[Entry](1 << 4)

  def select(`type`: Class[_]): Converter = {
    var _type: Class[_] = `type`
    var entries = iSelectEntries
    val length = entries.length
    var index = if (_type == null) 0 else _type.hashCode & (length - 1)
    var e: Entry = null
    while ( {
      e = entries(index); e
    } != null) {
      if (e.iType == _type) {
        return e.iConverter
      }
      if (index >= length) {
        index = 0
      }
    }
    val converter = selectSlow(this, _type)
    e = new Entry(_type, converter)
    entries = entries.clone().asInstanceOf[Array[Entry]]
    entries(index) = e
    for (i <- 0 until length if entries(i) == null) {
      iSelectEntries = entries
      return converter
    }
    val newLength = length << 1
    val newEntries = Array.ofDim[Entry](newLength)
    for (i <- 0 until length) {
      e = entries(i)
      _type = e.iType
      index = if (_type == null) 0 else _type.hashCode & (newLength - 1)
      while (newEntries(index) != null) {
        if (index >= newLength) {
          index = 0
        }
      }
      newEntries(index) = e
    }
    iSelectEntries = newEntries
    converter
  }

  def size(): Int = iConverters.length

  def copyInto[A <: Converter](converters: Array[A]) {
    System.arraycopy(iConverters, 0, converters, 0, iConverters.length)
  }

  def add[A <: Converter](converter: Converter, removed: Array[A]): ConverterSet = {
    val converters = iConverters
    val length = converters.length
    for (i <- 0 until length) {
      val existing:A = converters(i).asInstanceOf[A]
      if (converter == existing) {
        if (removed != null) {
          removed(0) = null.asInstanceOf[A]
        }
        return this
      }
      if (converter.getSupportedType == existing.getSupportedType) {
        val copy = Array.ofDim[Converter](length)
        for (j <- 0 until length) {
          copy(j) = if (j != i) converters(j) else converter
        }
        if (removed != null) {
          removed(0) = existing
        }
        return new ConverterSet(copy)
      }
    }
    val copy = Array.ofDim[Converter](length + 1)
    System.arraycopy(converters, 0, copy, 0, length)
    copy(length) = converter
    if (removed != null) {
      removed(0) = null.asInstanceOf[A]
    }
    new ConverterSet(copy)
  }

  def remove[A <: Converter](converter: A, removed: Array[A]): ConverterSet = {
    val converters = iConverters
    val length = converters.length
    for (i <- 0 until length if converter == converters(i)) {
      return remove(i, removed)
    }
    if (removed != null) {
      removed(0) = null.asInstanceOf[A]
    }
    this
  }

  def remove[A <: Converter](index: Int, removed: Array[A]): ConverterSet = {
    val converters = iConverters
    val length = converters.length
    if (index >= length) {
      throw new IndexOutOfBoundsException()
    }
    if (removed != null) {
      removed(0) = converters(index).asInstanceOf[A]
    }
    val copy = Array.ofDim[Converter](length - 1)
    var j = 0
    for (i <- 0 until length if i != index) {
      j = j + 1
      copy(j) = converters(i)
    }
    new ConverterSet(copy)
  }
}
