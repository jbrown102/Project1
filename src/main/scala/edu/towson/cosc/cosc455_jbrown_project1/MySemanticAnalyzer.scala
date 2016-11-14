package edu.towson.cosc.cosc455_jbrown_project1

//
// Jacob Brown
// COSC 455-001
// Project 1: MySemanticAnalyzer.scala
//

import scala.collection.mutable.Stack
import java.awt.Desktop
import java.io.{File, IOException}
import java.io._

class MySemanticAnalyzer {

  var outputStack = Stack[String]()
  var parse = Stack[String]()
  var nextToken: String = ""
  var output: String = ""
  var count: Int = 0
  var varName = new Array[String](50)
  var varVal = new Array[String](50)

  def semantics: Unit = {  // "Main" method for MySemanticAnalyzer.scala
    parse = Compiler.Parser.parse.reverse
    nextToken = parse.pop()
    lexical()
  }

  def lexical() {  // Translates HTML and processes all HTML operations
    while (!parse.isEmpty) {
      if (nextToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
        outputStack.push("<html>")  // ("<html>\n")
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
        outputStack.push("<head>")
        outputStack.push("<title>")
        outputStack.push(parse.pop())
        outputStack.push("</title>")
        outputStack.push("</head>")
        parse.pop()
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
        outputStack.push("<h1>")
        outputStack.push(parse.pop())
        outputStack.push("</h1>")
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
        outputStack.push("<p>")
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        outputStack.push("</p>")  // ("</p>\n")
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        outputStack.push("<b>")
        outputStack.push(parse.pop())
        outputStack.push("</b>")
        parse.pop()
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
        outputStack.push("<i>")
        outputStack.push(parse.pop())
        outputStack.push("</i>")
        parse.pop()
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
        outputStack.push("<li>")
        nextToken = parse.pop()
        if (nextToken.contains("\n") && !parse.isEmpty) {
          outputStack.push(nextToken)
        } else {
          lexical()
        }
        outputStack.push("</li>")
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
        outputStack.push("<br>")  // ("<br>\n")
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
        val txt = parse.pop()
        parse.pop()
        parse.pop()
        nextToken = parse.pop()
        parse.pop()
        outputStack.push("< a href = \"")
        outputStack.push(nextToken)
        outputStack.push("\">")
        outputStack.push(txt)
        outputStack.push("</a>")
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
        val txt = parse.pop()
        parse.pop()
        parse.pop()
        nextToken = parse.pop()
        parse.pop()
        outputStack.push("<img src = \"")
        outputStack.push(nextToken)
        outputStack.push("\" alt=\"")
        outputStack.push(txt)
        outputStack.push("\">")
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        val name = parse.pop()
        parse.pop()
        val value = parse.pop()
        parse.pop()
        if (varName.contains(name)) {
          val scope = varName.indexWhere(name)
          varName(scope) = name
          varVal(scope) = value
        } else {
          varName(count) = name
          varVal(count) = value
          count += 1
        }
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase(CONSTANTS.USEB)) {
        val name: String = parse.pop()
        parse.pop()
        if (varName.contains(name)) {
          val scope = varName.IndexOf(name)
          outputStack.push(varVal(scope))
        } else {
          println("STATIC SEMANTIC ERROR: That is not an allowed token...sorry...")
          System.exit(1)
        }
        nextToken = parse.pop()
      } else if (nextToken.equalsIgnoreCase((CONSTANTS.DOCE)) {
        outputStack.push("</html>")
      } else {
        outputStack.push(nextToken)
        nextToken = parse.pop()
      }
    }
    val output = outputStack.reverse.mkString
    val print = new PrintWriter(new File(Compiler.fileName + ".html"))
    print.write(output)
    print.close()
    openHTMLFileInBrowswer(Compiler.fileName + ".html")
    return
  }  // End of lexical()

  def openHTMLFileInBrowswer(htmlFileStr: String) = {  // Given openHTML method
    val file: File = new File(htmlFileStr.trim)
    println(File.getAbsolutePath)
    if (!file.exists()) {
      sys.error("File " + htmlFileStr + " does not exist.")
    } try {
      Desktop.getDesktop.browse(file.toURI)
    } catch {
      case ioe: IOException => sys.error("Failed to open file: " + htmlFileStr)
      case e: Exception => sys.error("Sorry...")
    }
  }  // End of openHTMLFileInBrowser()
}  // End of MySemanticAnalyzer.scala