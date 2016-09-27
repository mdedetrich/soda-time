package org.joda.time.format

import java.io.IOException
import java.io.Writer

object FormatUtils {

  private val LOG_10 = Math.log(10)

  def appendPaddedInteger(buf: StringBuffer, value: Int, size: Int) {
    try {
      appendPaddedInteger(buf.asInstanceOf[Appendable], value, size)
    } catch {
      case e: IOException =>
    }
  }

  def appendPaddedInteger(appenadble: Appendable, value: Int, size: Int) {
    var _value: Int = value
    var _size: Int = size
    if (_value < 0) {
      appenadble.append('-')
      if (_value != Integer.MIN_VALUE) {
        _value = -_value
      } else {
        while (_size > 10) {
          appenadble.append('0')
          _size -= 1
        }
        appenadble.append("" + -Integer.MIN_VALUE.toLong)
        return
      }
    }
    if (_value < 10) {
      while (_size > 1) {
        appenadble.append('0')
        _size -= 1
      }
      appenadble.append((_value + '0').toChar)
    } else if (_value < 100) {
      while (_size > 2) {
        appenadble.append('0')
        _size -= 1
      }
      val d = ((_value + 1) * 13421772) >> 27
      appenadble.append((d + '0').toChar)
      appenadble.append((_value - (d << 3) - (d << 1) + '0').toChar)
    } else {
      var digits: Int = 0
      digits =
        if (_value < 1000) 3
        else if (_value < 10000) 4
        else (Math.log(_value) / LOG_10).toInt + 1
      while (_size > digits) {
        appenadble.append('0')
        _size -= 1
      }
      appenadble.append(Integer.toString(_value))
    }
  }

  def appendPaddedInteger(buf: StringBuffer, value: Long, size: Int) {
    try {
      appendPaddedInteger(buf.asInstanceOf[Appendable], value, size)
    } catch {
      case e: IOException =>
    }
  }

  def appendPaddedInteger(appendable: Appendable, value: Long, size: Int) {
    var _value: Long = value
    var _size: Int = size
    val intValue = _value.toInt
    if (intValue == _value) {
      appendPaddedInteger(appendable, intValue, _size)
    } else if (_size <= 19) {
      appendable.append(_value.toString)
    } else {
      if (_value < 0) {
        appendable.append('-')
        if (_value != Long.MinValue) {
          _value = -_value
        } else {
          while (_size > 19) {
            appendable.append('0')
            _size -= 1
          }
          appendable.append("9223372036854775808")
          return
        }
      }
      val digits = (Math.log(_value) / LOG_10).toInt + 1
      while (_size > digits) {
        appendable.append('0')
        _size -= 1
      }
      appendable.append(_value.toString)
    }
  }

  def writePaddedInteger(out: Writer, value: Int, size: Int) {
    var _value: Int = value
    var _size: Int = size
    if (_value < 0) {
      out.write('-')
      if (_value != Integer.MIN_VALUE) {
        _value = -_value
      } else {
        while (_size > 10) {
          out.write('0')
          _size -= 1
        }
        out.write("" + -Integer.MIN_VALUE.toLong)
        return
      }
    }
    if (_value < 10) {
      while (_size > 1) {
        out.write('0')
        _size -= 1
      }
      out.write(_value + '0')
    } else if (_value < 100) {
      while (_size > 2) {
        out.write('0')
        _size -= 1
      }
      val d = ((_value + 1) * 13421772) >> 27
      out.write(d + '0')
      out.write(_value - (d << 3) - (d << 1) + '0')
    } else {
      var digits: Int = 0
      digits =
        if (_value < 1000) 3
        else if (_value < 10000) 4
        else (Math.log(_value) / LOG_10).toInt + 1
      while (_size > digits) {
        out.write('0')
        _size -= 1
      }
      out.write(Integer.toString(_value))
    }
  }

  def writePaddedInteger(out: Writer, value: Long, size: Int) {
    var _value: Long = value
    var _size: Int = size
    val intValue = _value.toInt
    if (intValue == _value) {
      writePaddedInteger(out, intValue, _size)
    } else if (_size <= 19) {
      out.write(_value.toString)
    } else {
      if (_value < 0) {
        out.write('-')
        if (_value != Long.MinValue) {
          _value = -_value
        } else {
          while (_size > 19) {
            out.write('0')
            _size -= 1
          }
          out.write("9223372036854775808")
          return
        }
      }
      val digits = (Math.log(_value) / LOG_10).toInt + 1
      while (_size > digits) {
        out.write('0')
        _size -= 1
      }
      out.write(_value.toString)
    }
  }

  def appendUnpaddedInteger(buf: StringBuffer, value: Int) {
    try {
      appendUnpaddedInteger(buf.asInstanceOf[Appendable], value)
    } catch {
      case e: IOException =>
    }
  }

  def appendUnpaddedInteger(appendable: Appendable, value: Int) {
    var _value: Int = value
    if (_value < 0) {
      appendable.append('-')
      if (_value != Integer.MIN_VALUE) {
        _value = -_value
      } else {
        appendable.append("" + -Integer.MIN_VALUE.toLong)
        return
      }
    }
    if (_value < 10) {
      appendable.append((_value + '0').toChar)
    } else if (_value < 100) {
      val d = ((_value + 1) * 13421772) >> 27
      appendable.append((d + '0').toChar)
      appendable.append((_value - (d << 3) - (d << 1) + '0').toChar)
    } else {
      appendable.append(Integer.toString(_value))
    }
  }

  def appendUnpaddedInteger(buf: StringBuffer, value: Long) {
    try {
      appendUnpaddedInteger(buf.asInstanceOf[Appendable], value)
    } catch {
      case e: IOException =>
    }
  }

  def appendUnpaddedInteger(appendable: Appendable, value: Long) {
    val intValue = value.toInt
    if (intValue == value) {
      appendUnpaddedInteger(appendable, intValue)
    } else {
      appendable.append(value.toString)
    }
  }

  def writeUnpaddedInteger(out: Writer, value: Int) {
    var _value: Int = value
    if (_value < 0) {
      out.write('-')
      if (_value != Integer.MIN_VALUE) {
        _value = -_value
      } else {
        out.write("" + -Integer.MIN_VALUE.toLong)
        return
      }
    }
    if (_value < 10) {
      out.write(_value + '0')
    } else if (_value < 100) {
      val d = ((_value + 1) * 13421772) >> 27
      out.write(d + '0')
      out.write(_value - (d << 3) - (d << 1) + '0')
    } else {
      out.write(Integer.toString(_value))
    }
  }

  def writeUnpaddedInteger(out: Writer, value: Long) {
    val intValue = value.toInt
    if (intValue == value) {
      writeUnpaddedInteger(out, intValue)
    } else {
      out.write(value.toString)
    }
  }

  def calculateDigitCount(value: Long): Int = {
    if (value < 0) {
      if (value != Long.MinValue) {
        return calculateDigitCount(-value) + 1
      } else {
        return 20
      }
    }
    if (value < 10) 1
    else if (value < 100) 2
    else if (value < 1000) 3
    else if (value < 10000) 4
    else (Math.log(value) / LOG_10).toInt + 1
  }

  def parseTwoDigits(text: CharSequence, position: Int): Int = {
    val value = text.charAt(position) - '0'
    ((value << 3) + (value << 1)) + text.charAt(position + 1) -
      '0'
  }

  def createErrorMessage(text: String, errorPos: Int): String = {
    val sampleLen = errorPos + 32
    var sampleText: String = null
    sampleText =
      if (text.length <= sampleLen + 3) text
      else text.substring(0, sampleLen).concat("...")
    if (errorPos <= 0) {
      return "Invalid format: \"" + sampleText + '"'
    }
    if (errorPos >= text.length) {
      return "Invalid format: \"" + sampleText + "\" is too short"
    }
    "Invalid format: \"" + sampleText + "\" is malformed at \"" +
      sampleText.substring(errorPos) +
      '"'
  }
}
