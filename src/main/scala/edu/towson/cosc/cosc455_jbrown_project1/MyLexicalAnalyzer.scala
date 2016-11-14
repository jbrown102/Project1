package edu.towson.cosc.cosc455_jbrown_project1

//
// Jacob Brown
// COSC 455-001
// Project 1: MyLexicalAnalyzer.scala
//

import scala.collection.mutable.ArrayBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {

  val lookup = new Array[String](20)
  var token = new ArrayBuffer[Char](50)
  var fileArray: Array[Char] = Array()
  var filePos: Int = -1
  var fileSize: Int = 0
  var nextChar: Char = ' '

  def start(f: String): Unit = {  // Starter method that prepares for getNextToken
    validTokens()
    fileArray = f.toCharArray()
    fileSize = fileArray.length - 1
  }

  def validTokens() = {  // Defines lookup array of valid tokens
    lookup(0) = "\\BEGIN"
    lookup(1) = "\\END"
    lookup(2) = "\\TITLE["
    lookup(3) = "]"
    lookup(4) = "#"
    lookup(5) = "\\PARAB"
    lookup(6) = "\\PARAE"
    lookup(7) = "\\DEF["
    lookup(8) = "\\USE["
    lookup(9) = "**"
    lookup(10) = "*"
    lookup(11) = "+"
    lookup(12) = "\\\\"
    lookup(13) = "["
    lookup(14) = "]"
    lookup(15) = "("
    lookup(16) = ")"
    lookup(17) = "="
    lookup(18) = "!["
    lookup(19) = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toString()
  }

  override def addChar(): Unit = {  // Adds character token to array
    token += nextChar
  }

  override def getChar(): Unit = {  // Gets token primed and increments
    if (filePos < fileSize) {
      filePos += 1
      nextChar = fileArray.charAt(filePos)
    } else {
      return
    }
  }

  override def getNextToken(): Unit = {  // Gets the next token while utilizing lookup tokens
    getChar()
    notText()
    if (nextChar.equals('+') || nextChar.equals('=') || nextChar.equals('#') || nextChar.equals('(') || nextChar.equals(')') || nextChar.equals('[') || nextChar.equals(']')) {
      addChar()
    } else if (nextChar.equals('\\')) {
      addChar()
      getChar()
      while (!nextChar.equals('[') && !nextChar.equals('\r') && !nextChar.equals('\n') && !nextChar.equals('\\')) {
        if (nextChar.equals('\r')) {
          addChar()
        } else {
          addChar()
          getChar()
        }
      }
      if (nextChar.equals('[')) { addChar() }
      if (nextChar.equals('\\')) { addChar() }
    } else if (nextChar.equals('*')) {
      addChar()
      getChar()
      if (nextChar.equals('*')) {
        addChar()
        getChar()
        package()
      } else {
        filePos -= 1
      }
    } else if (nextChar.equals('!')) {
      addChar()
      getChar()
      if (nextChar.equals('[')) { addChar() }
    } else {
      addChar()
      getChar()
      while (!CONSTANTS.SYMBOLS.contains(nextChar)) {
        addChar()
        if (filePos < fileSize) {
          getChar()
        } else {
          return
        }
      }
      filePos -= 1
    }
    getToken()
  }

  def isText(text: String): Boolean = {  // Checks token to see if there are any special characters
    if (text.contains("\\")) {
      return false
    } else {
      return true
    }
  }

  def getToken(): Unit = {  // Finishes the getNextToken method by processing for errors
    val possibleToken: String = token.mkString   // Convert array into string to check below
    if (lookup.contains(possibleToken.toUpperCase)) {
      setCurrent(possibleToken)  // Set current token in Compiler.scala
      token.clear()
    } else if (isText(possibleToken)) {
      println(possibleToken)
      setCurrent(possibleToken)
      token.clear()
    } else {
      println("LEXICAL ERROR: Incorrect token found... " + possibleToken + " was found instead...sorry...")
      System.exit(1)
    }
  }

  def setCurrent(currentToken: String): Unit = {  // Sets current token in Compiler.scala
    Compiler.currentToken = currentToken
  }

  def notText(): Unit = {  // Method for retrieving terminal tokens
    while (nextChar.equals(' ') || nextChar.equals('\r') || nextChar.equals('\n') || nextChar.equals('\t') {
      getChar()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        getChar()
        return
      }
    }
  }

}  // End of MyLexicalAnalyzer.scala
