package edu.towson.cosc.cosc455_jbrown_project1

// import scala.io.Source._

object Compiler {

  var currentToken : String = ""  // Space or no space
  var fileContents : String = ""  // Same here

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySyntaxAnalyzer

  def main(args : Array[String]) = {

    // println(args{0})  <- USE THIS??
    checkfile(args)  // check usage
    readfile(args(0))
    // println(fileContents)  <- USE THIS??
    Scanner.getNextToken()  // gets first token

    //  Parser.gittex()  // calls start state of BNF in SyntaxAnalyzer  <- USE THIS??
    // on return, there is a parse tree
  }

    def readfile(file : String) = {
      val source = scala.io.Source.fromFile(file)
      fileContents = try source.mkString finally source.clone()
    }

    def checkfile(args : Array[String]) = {
      if (args.length != 1) {
        println("USAGE ERROR: wrong number of args")
        System.exit(1)
      } else if (! args(0).endsWith(".mkd")) {
        println("USAGE ERROR: wrong extension")
        System.exit(1)
      }
    }
  }

