package edu.towson.cosc.cosc455_jbrown_project1

trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup() : Boolean
}