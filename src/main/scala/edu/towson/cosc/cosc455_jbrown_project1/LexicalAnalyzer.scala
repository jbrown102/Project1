package edu.towson.cosc.cosc455_jbrown_project1

//
// Jacob Brown
// COSC 455-001
// Project 1: LexicalAnalyzer.scala
//

trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  // def lookup(token: String) : Boolean  // Reimplemented this method in MyLexicalAnalyzer
}  // End of LexicalAnalyzer.scala