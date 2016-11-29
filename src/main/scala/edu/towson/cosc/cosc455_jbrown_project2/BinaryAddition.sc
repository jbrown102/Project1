package edu.towson.cosc.cosc455_jbrown_project2

//
// NAME: Jacob Brown
// COSC 455-001 - Programming Languages: Implementation and Design
// Project 2: BinaryAddition.sc
//

// Test Cases
val pTest1: List[Int] = List(1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExpectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List(1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExpectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List(1, 0, 0, 0, 1, 1, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExpectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List(1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExpectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

// End of Test Cases


// Binary Addition Testing

//
// Test 1
//
println("Test 1 beginning...")
if (binaryAddition(pTest1, qTest1).equals(test1ExpectedSolution)) {
  println("Test 1 passes!  Woohoo!!!")
} else {
  println("Test 1 fails...  Uh oh...")
}
println("Test 1 complete.")

//
// Test 2
//
println("Test 2 beginning...")
if (binaryAddition(pTest2, qTest2).equals(test2ExpectedSolution)) {
  println("Test 2 passes!  Woohoo!!!")
} else {
  println("Test 2 fails...  Uh oh...")
}
println("Test 2 complete.")

//
// Test 3
//
println("Test 3 beginning...")
if (binaryAddition(pTest3, qTest3).equals(test3ExpectedSolution)) {
  println("Test 3 passes!  Woohoo!!!")
} else {
  println("Test 3 fails...  Uh oh...")
}
println("Test 3 complete.")

//
// Test 4
//
println("Test 4 beginning...")
if (binaryAddition(pTest4, qTest4).equals(test4ExpectedSolution)) {
  println("Test 4 passes!  Woohoo!!!")
} else {
  println("Test 4 fails...  Uh oh...")
}
println("Test 4 complete.")
println("Testing complete.")

// End of Binary Addition Testing


/* This is the "main" function to do binary addition.  This function should:
1. Convert the input parameter lists from integers to boolean.  Use Scala reverse.
2. Reverse the lists (since binary addition is performed right to left).  Use Scala reverse.
3. Perform the binary addition with the doBinaryAddition function.
4. Reverse the lists (to get back in proper order).  Use Scala reverse.
5. Convert the answer back to binary integer form for output.
NOTE: The initial carry bit is assumed to be 0 (i.e., false). */
def binaryAddition(pList: List[Int], qList: List[Int]) = {
  var integerListP = pList.reverse
  var integerListQ = qList.reverse
  var booleanListP = convertIntegerListToBooleanList(integerListP)
  var booleanListQ = convertIntegerListToBooleanList(integerListQ)
  var addition = doBinaryAddition(booleanListP, booleanListQ, false)
  var integerListResult = addition.reverse
  var booleanListResult = convertBooleanListToIntegerList(integerListResult)
  println(booleanListResult)
}  // End of binaryAddition()

// This function does the binary addition of two boolean lists. NOTE: The lists may not be equal in length.
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  ((pBits.isEmpty) && (!qBits.isEmpty) && (carryBit == true)) match {
    case true => finishBinaryAdd(qBits, carryBit)
    case false => ((!pBits.isEmpty) && (qBits.isEmpty) && (carryBit == true)) match {
        case true => finishBinaryAdd(pBits, carryBit)
        case false => ((!pBits.isEmpty) && (!qBits.isEmpty)) match {
            case true => addBits(pBits.head, qBits.head, carryBit) :: doBinaryAddition(pBits.tail, qBits.tail, getNextCarryBit(pBits.head, qBits.head, carryBit))
            case false => Nil
        }  // End of third match
    }  // End of second match
  }  // End of first match
}  // End of doBinaryAddition()

// This function does the binary addition when there are uneven lists and still must finish the add with the carry bits.
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  ((!remainingBits.isEmpty) && (carryBit == false)) match {
    case true => remainingBits
    case false => ((remainingBits.isEmpty) && (carryBit == true)) match {
        case true => List(true)
        case false => ((!remainingBits.isEmpty) && (carryBit == true)) match {
            case true => !remainingBits.head :: finishBinaryAdd(remainingBits.tail, remainingBits.head)
        }  // End of third match
    }  // End of second match
  }  // End of first match
}  // End of finishBinaryAdd()

// This function determines what the next carry bit should be based on current bits.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  ((pBit && qBit) || (pBit && carryBit) || (qBit && carryBit))
}  // End of getNextCarryBit()

// This function does the binary addition of two booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  (carryBit == (pBit == qBit))
}  // End of addBits()

// This function converts a binary integer list into its respective boolean list
def convertIntegerListToBooleanList(integerList: List[Int]) = {
  integerList.map {
    case 0 => false
    case 1 => true
  }
}  // End of convertIntegerListToBooleanList()

// This function converts a binary boolean list into its respective integer list
def convertBooleanListToIntegerList(booleanList: List[Boolean]): Unit = {
  booleanList.map {
    case false => 0
    case true => 1
  }
}  // End of convertBooleanListToIntegerList()

// End of BinaryAddition.sc