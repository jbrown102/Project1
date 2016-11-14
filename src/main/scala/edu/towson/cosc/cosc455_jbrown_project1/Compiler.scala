package edu.towson.cosc.cosc455_jbrown_project1

//
// Jacob Brown
// COSC 455-001
// Project 1: Compiler.scala
//

object Compiler {

  // STARTING NOTES
  // - A .jar for the project was created.  It is placed in .idea/artifacts.
  //   -  This most likely did not work properly.  There were runtime errors and it wouldn't allow me to select the Compiler program as the main class
  // - The Scaladocs for the project were attempted as well, but there was another runtime error that wouldn't allow them to be created at all.
  //   - The error statement was placed in its place in the documentation folder.
  // Both of these issues occurred exactly the same way as in Lab 4, so it has to be something with IntelliJ/Java on this laptop

  var currentToken : String = ""
  var fileContents : String = ""
  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer
  var end: Boolean = false
  var fileName: String = ""

  def main(args : Array[String]) = {  // Main method for Compiler.scala

    fileName = args(0)
    checkfile(args)
    readfile(args(0))

    println("File starting...")
    println(fileContents)
    println()
    println("Processing...")

    Scanner.start(fileContents)  // Passes fileContents as string to Scanner (MyLexicalAnalyzer)

    while (Scanner.filePos < Scanner.fileSize && !end) {
      Scanner.getNextToken()  // Gets current token using LexicalAnalyzer
      Parser.gittex()  // Calls start state of BNF in SyntaxAnalyzer
      if (currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        end = true  // Ends while loop and continues to HTML processing
      }
    }

    println("File has been processed...\n")
    println("File is now being converted to HTML...\n")

    SemanticAnalyzer.semantics()  // Calls SemanticAnalyzer
  }  // End of main()

  def readfile(file : String) = {  // Changes file text to string
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkfile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: Wrong number of arguments...sorry...")
      System.exit(1)
    } else if (!args(0).endsWith(".mkd")) {
      println("USAGE ERROR: Wrong extension type...sorry...")
      System.exit(1)
    }
  }
}  // End of Compiler.scala