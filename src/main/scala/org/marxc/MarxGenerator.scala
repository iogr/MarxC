package org.marxc


class ArnoldGenerator extends ClassLoader {

  def generate(marxCode: String, filename: String): Array[Byte] = {
    val parser = new ArnoldParser
    val rootNode = parser.parse(marxCode)
    rootNode.generateByteCode(filename)
  }
}
