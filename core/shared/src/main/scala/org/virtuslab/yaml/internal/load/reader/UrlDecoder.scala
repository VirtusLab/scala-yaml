package org.virtuslab.yaml.internal.load.reader

import java.nio.charset.Charset
import java.nio.ByteBuffer
import java.nio.CharBuffer

object UrlDecoder {
  def decode(s: String): String = {
    val charset             = Charset.defaultCharset()
    val len                 = s.length
    lazy val charsetDecoder = charset.newDecoder()

    lazy val byteBuffer = ByteBuffer.allocate(len / 3)
    val charBuffer      = CharBuffer.allocate(len)

    def throwIllegalHex() = {
      throw new IllegalArgumentException(
        "URLDecoder: Illegal hex characters in escape (%) pattern"
      )
    }

    var i = 0
    while (i < len) {
      s.charAt(i) match {
        case '+' =>
          charBuffer.append(' ')
          i += 1

        case '%' if i + 3 > len =>
          throwIllegalHex()

        case '%' =>
          val decoder = charsetDecoder
          val buffer  = byteBuffer
          buffer.clear()
          decoder.reset()

          while (i + 3 <= len && s.charAt(i) == '%') {
            val c1 = Character.digit(s.charAt(i + 1), 16)
            val c2 = Character.digit(s.charAt(i + 2), 16)

            if (c1 < 0 || c2 < 0)
              throwIllegalHex()

            buffer.put(((c1 << 4) + c2).toByte)
            i += 3
          }

          buffer.flip()
          val decodeResult = decoder.decode(buffer, charBuffer, true)
          val flushResult  = decoder.flush(charBuffer)

          if (decodeResult.isError() || flushResult.isError())
            throwIllegalHex()

        case c =>
          charBuffer.append(c)
          i += 1
      }
    }

    charBuffer.flip()
    charBuffer.toString
  }
}
