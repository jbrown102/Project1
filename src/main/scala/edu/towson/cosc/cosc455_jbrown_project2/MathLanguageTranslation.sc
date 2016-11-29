package edu.towson.cosc.cosc455_jbrown_project2

//
// NAME: Jacob Brown
// COSC 455-001 - Programming Languages: Implementation and Design
// Project 2: MathLanguageTranslation.sc
//

// Language Definition Lists
val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

// Test Cases
go(List("yi","nine","six","ba"))
/* NOTE:: Expected output:
   Translation: 1 9 6 8
   Addition: 1 + 9 + 6 + 8 = 24
   Multiplication: 1 * 9 * 6 * 8 = 432
*/

go(List("yi","josh","three","si"))
/* NOTE:: Expected output:
   Translation: 1 3 4    // Technically 1 _ 3 4 , but "josh" is discarded since it is invalid.
   Addition: 1 + 3 + 4 = 8
   Multiplication: 1 * 3 * 4 = 12
*/

// End of Test Cases


// Main function which queries the core Translation functions, and displays results.
def go(alist: List[String]): Unit = {

  // Define and sort the given List, by "copying" it to an Integer List.
  var blist = List[Int]()
  blist = sort(alist)

  // Execute core Math Translation functions and display results.
  val addition = add(blist)
  val multiplication = multiply(blist)
  println("Translation:" + blist)
  println("Addition: " + addition)
  println("Multiplication: " + multiplication)
  println()
}  // End of go()

// Sort() function checks the given list, by language, against the given libraries for potentially similar values.
def sort(alist: List[String]): List[Int] = {
  alist match {
    case Nil => Nil  // Empty case
    case head :: tail =>
      if (chinese.contains(head)) {  // Chinese
        chinese.indexOf(head) :: sort(tail)
      } else if (english.contains(head)) {  // English
        english.indexOf(head) :: sort(tail)
      } else {  // Other (a.k.a. "Josh")
        sort(tail)
      }
  }  // End of match
}  // End of sort()

// Add() function adds all the members of the given list together.
def add(alist: List[Int]): Int = {
  alist.foldLeft(0)(_ + _)
}  // End of add()

// Multiply() function multiplies all the members of the given list together.
def multiply(alist: List[Int]): Int = {
  alist.foldLeft(1)(_ * _)
}  // End of multiply()

// End of MathLanguageTranslation.sc