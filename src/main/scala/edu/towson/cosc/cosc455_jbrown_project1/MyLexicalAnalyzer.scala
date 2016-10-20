package edu.towson.cosc.cosc455_jbrown_project1

class MyLexicalAnalyzer extends LexicalAnalyzer {
  override def addChar(): Unit = ???

  override def getChar(): Char = ???

  override def getNextToken(): Unit = {
    val c = getChar()
  }

  override def lookup(): Boolean = ???
}
