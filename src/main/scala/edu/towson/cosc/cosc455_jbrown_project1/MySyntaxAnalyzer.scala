
package edu.towson.cosc.cosc455_jbrown_project1

//
// Jacob Brown
// COSC 455-001
// Project 1: MySemanticAnalyzer.scala
//

import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer {

  var parse = Stack[String]()
  var error: Boolean = false

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      parse.push(CONSTANTS.DOCB)
      Compiler.Scanner.getNextToken()
      while (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB) && !error) { variableDefine() }
      while (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) && !error) { variableUse() }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) && !error) { heading() }
      if (!error) { body() }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE) && !error) {
        parse.push(CONSTANTS.DOCE)
        Compiler.Scanner.getNextToken()
        if (Compiler.Scanner.nextChar.equals('\n')) {
          return
        } else {
          error = true
          println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.NEWLINE + " ...sorry...")
          System.exit(1)
        }
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.DOCE + " ...sorry...")
        System.exit(1)
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.DOCB + " ...sorry...")
      System.exit(1)
    }
  }  // End of gittex()

  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parse.push(CONSTANTS.TITLEB)
      Compiler.Scanner.getNextToken()
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) { innerText() }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) {
        parse.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.BRACKETE + " ...sorry...")
        System.exit(1)
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.TITLEB + " ...sorry...")
      System.exit(1)
    }
  }  // End of title()

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      parse.push(CONSTANTS.HEADING)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB) && !error) { title() }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) && !error) {
        parse.push(CONSTANTS.HEADING)
        Compiler.Scanner.getNextToken()
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.HEADING + " ...sorry...")
        System.exit(1)
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.HEADING + " ...sorry...")
      System.exit(1)
    }
  }  // End of heading()

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parse.push(CONSTANTS.PARAB)
      Compiler.Scanner.getNextToken()
      while (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB) && !error) { variableDefine() }
      while (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) && !error) { variableUse() }
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE) && !error ) { innerText() }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE) && !error) {
        parse.push(CONSTANTS.PARAE)
        Compiler.Scanner.getNextToken()
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.PARAE + " ...sorry...")
        System.exit(1)
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.PARAB + " ...sorry...")
      System.exit(1)
    }
  }  // End of paragraph()

  override def bold(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      parse.push(CONSTANTS.BOLD)
      Compiler.Scanner.getNextToken()
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) && !error) {
        variableDefine()
        variableUse()
        innerItem()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) && !error) {
        parse.push(CONSTANTS.BOLD)
        Compiler.Scanner.getNextToken()
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.BOLD + " ...sorry...")
        System.exit(1)
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.BOLD + " ...sorry...")
      System.exit(1)
    }
  }  // End of bold()

  override def italics(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
      parse.push(CONSTANTS.ITALICS)
      Compiler.Scanner.getNextToken()
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS) && !error) {
        variableDefine()
        variableUse()
        innerItem()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS) && !error) {
        parse.push(CONSTANTS.ITALICS)
        Compiler.Scanner.getNextToken()
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.ITALICS + " ...sorry...")
      System.exit(1)
    }
  }  // End of italics()

  override def listItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      parse.push(CONSTANTS.LISTITEM)
      Compiler.Scanner.getNextToken()
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM) && !error) {
        italics()
        bold()
        link()
        variableUse()
        innerItem()
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.LISTITEM + " ...sorry...")
      System.exit(1)
    }
  }  // End of listItem()

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      parse.push(CONSTANTS.NEWLINE)
      Compiler.Scanner.getNextToken()
    } else {
      error = true
      println("SYNTAX ERROR: Received" + Compiler.currentToken + ", but expected " + CONSTANTS.NEWLINE + " ...sorry...")
      System.exit(1)
    }
  }  // End of newline()

  override def link(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      parse.push(CONSTANTS.LINKB)
      Compiler.Scanner.getNextToken()
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) {
        variableUse()
        innerItem()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) {
        parse.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB) && !error) {
          parse.push(CONSTANTS.ADDRESSB)
          Compiler.Scanner.getNextToken()
          while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE) && !error) {
            variableUse()
            innerItem()
          }
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE) && !error) {
            parse.push(CONSTANTS.ADDRESSE)
            Compiler.Scanner.getNextToken()
          } else {
            error = true
            println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.ADDRESSE + " ...sorry...")
            System.exit(1)
          }
        } else {
          error = true
          println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.ADDRESSB + " ...sorry...")
          System.exit(1)
        }
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.BRACKETE + " ...sorry...")
        System.exit(1)
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.LINKB + " ...sorry...")
      System.exit(1)
    }
  }  // End of link()

  override def image(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      parse.push(CONSTANTS.IMAGEB)
      Compiler.Scanner.getNextToken()
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) {
        variableUse()
        innerItem()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) {
        parse.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB) && !error) {
          parse.push(CONSTANTS.ADDRESSB)
          Compiler.Scanner.getNextToken()
          while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE) && !error) {
            variableUse()
            innerItem()
          }
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE) && !error) {
            parse.push(CONSTANTS.ADDRESSE)
            Compiler.Scanner.getNextToken()
          } else {
            error = true
            println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.ADDRESSE + " ...sorry...")
            System.exit(1)
          }
        } else {
          error = true
          println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.ADDRESSB + " ...sorry...")
          System.exit(1)
        }
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.BRACKETE + " ...sorry...")
        System.exit(1)
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.IMAGEB + " ...sorry...")
      System.exit(1)
    }
  }  // End of image()

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      parse.push(CONSTANTS.DEFB)
      Compiler.Scanner.getNextToken()
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) { innerItem() }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN) && !error) {
        parse.push(CONSTANTS.EQSIGN)
        Compiler.Scanner.getNextToken()
        while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) { innerItem() }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) {
          parse.push(CONSTANTS.BRACKETE)
          Compiler.Scanner.getNextToken()
        } else {
          error = true
          println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.BRACKETE + " ...sorry...")
          System.exit(1)
        }
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.EQSIGN + " ...sorry...")
        System.exit(1)
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.DEFB + " ...sorry...")
      System.exit(1)
    }
  }  // End of variableDefine()

  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      parse.push(CONSTANTS.USEB)
      Compiler.Scanner.getNextToken()
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) { innerItem() }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE) && !error) {
        parse.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.BRACKETE + " ...sorry...")
        System.exit(1)
      }
    } else {
      error = true
      println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.USEB + " ...sorry...")
      System.exit(1)
    }
  }  // End of variableUse

  override def body(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB) && !error) {
      paragraph()
      body()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE) && !error) {
      newline()
      body()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE) && !error) {
      return
    } else {
      innerText()
      body()
    }
  }  // End of body()

  override def innerItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) && !error) {
      variableUse()
      innerItem()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) && !error) {
      bold()
      innerItem()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS) && !error) {
      italics()
      innerItem()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) && !error) {
      link()
      innerItem()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM) && !error) {
      listItem()
      innerItem()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE) && !error) {
      newline()
      innerItem()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) && !error) {
      variableUse()
      innerItem()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE) && !error) {
      error = true
    } else if (Compiler.Scanner.isText(Compiler.currentToken) && !error) {
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
    } else {
      error = true
    }
  }  // End of innerItem()

  override def innerText(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
      if (parse.contains(CONSTANTS.PARAB)) {
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE) && !error) {
          parse.push(CONSTANTS.PARAE)
          Compiler.Scanner.getNextToken()
        }
      } else {
        error = true
        println("SYNTAX ERROR: Received " + Compiler.currentToken + ", but expected " + CONSTANTS.PARAE + " ...sorry...")
        System.exit(1)
      }
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) && !error) {
      heading()
      innerText()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) && !error) {
      bold()
      innerText()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS) && !error) {
      italics()
      innerText()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM) && !error) {
      listItem()
      innerText()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB) && !error) {
      image()
      innerText()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) && !error) {
      variableUse()
      innerText()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) && !error) {
      link()
      innerText()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE) && !error) {
      newline()
      innerText()
    } else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE) && !error) {
      return
    } else if (Compiler.Scanner.isText(Compiler.currentToken) && !error) {
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    } else {
      error = true
    }
  }  // End of innerText()
}  // End of MySyntaxAnalyzer.scala