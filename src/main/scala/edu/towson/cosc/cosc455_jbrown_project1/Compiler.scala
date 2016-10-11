package edu.towson.cosc.cosc455_jbrown_project1

object Compiler {

  var fileContents : String = ""

  def main(args : Array[String]) = {

    // println(args{0})

    // check usage
    checkfile(args)
    readfile(args {0})
    println(fileContents)

    // gets first token
    Scanner.getNextToken()

    // calls start state of BNF in SyntaxAnalyzer
    Parser.gittex()

    // on return, there is a parse tree
  }

    def readfile(file : String) = {
      val source = scala.io.Source.fromfile(file)
      fileContents = try source.myString finally source.clone()
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

