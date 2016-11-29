package edu.towson.cosc.cosc455_jbrown_project2

//
// NAME: Jacob Brown
// COSC 455-001 - Programming Languages: Implementation and Design
// Project 2: Warmup.sc
//

// Import statement for Scala's Stack use
import scala.collection.mutable.Stack


// Test Cases

//
// Exercise 1: Prime Numbers
//
var num1: Int = 37
prime(num1)

//
// Exercise 2: Twin Primes
//
var num2: Int = 41
var num3: Int = 43
var num4: Int = 47
twinPrimes(num2, num3)  // Should return true.
twinPrimes(num3, num4)  // Should return false.

//
// Exercise 3: Twin Primes List
//
var numlist: List[Int] = twinPrimesList(50)
// Expected Output: [3, 5, 7, 11, 13, 17, 19, 29, 31, 41, 43].

//
// Exercise 4: Goldbach's Conjecture
//
goldbach(28)
// Expected Output: 5 + 23 = 28.

// End of Test Cases


// Prime() function tests to see if the given integer is a prime number.
// A prime number is an integer greater than 1 that has no positive divisors other than 1 and itself.
def prime(n: Int): Boolean = {
  (2 until n) forall (n % _ != 0)
}  // End of prime()

// TwinPrimes() function tests to see if 2 given numbers are twin primes.
// A twin prime is a prime number that differs from another prime number by two; an example is (41, 43).
def twinPrimes(n: Int, i: Int): Boolean = {
  if (prime(n) && prime(i)) {
    if ((n - i == 2) || (n - i == -2)) {
      return true
    } else {
      return false
    }
  } else {
    return false
  }
}  // End of twinPrimes()

// TwinPrimesList() function returns an integer list of all the twin primes going up to the given number.
// Used alongside the twinPrimesListHelp() helper function which conducts most of the associated execution steps.
def twinPrimesList(n: Int): List[Int] = {
  var alist = Stack[Int]()
  twinPrimesListHelp(n, alist)
}  // End of twinPrimesList()

// TwinPrimesListHelp() helper function to twinPrimesList that conducts most of the associated execution steps.
def twinPrimesListHelp(n: Int, alist: Stack[Int]): List[Int] = {
  if (prime(n)) {
    if (twinPrimes(n, n - 2)) {
      if (!alist.contains(n)) {
        alist.push(n)
      }
      alist.push(n - 2)
    }
    if (!n.equals(5)) {
      twinPrimesListHelp(n - 1, alist)
    } else {
      return alist.toList
    }
  } else {
    twinPrimesListHelp(n - 1, alist)
  }
}  // End of twinPrimesListHelp()

// Goldbach() function tests to see if the given integer passes the Goldbach Conjecture.
// The Goldbach Conjecture states that every positive number greater than 2 is the sum of two prime numbers.
// Example: 28 = 5 + 23
// Used alongside the goldbachHelp() helper function which conducts the remaining execution steps.
def goldbach(n: Int): Unit = {
  if ((n > 2) && (n % 2 == 0)) {
    var a: Int = 0
    var b: Int = n
    goldbachHelp(n, a, b)
  } else {
    println("ERROR: The given integer was not even or greater than 2.")
  }
}  // End of goldbach()

// GoldbachHelp() helper function to goldbach() that conducts the execution steps outside of test checking.
def goldbachHelp(n: Int, a: Int, b: Int): Unit = {
  if ((prime(b)) && (prime(a))) {
    if (!(b + a == n)) {
      var a2 = a + 1
      var b2 = b - 1
      goldbachHelp(n, a2, b2)
    } else {
      println("goldbach(" + n + ") returns: \n" + b + " + " + a + " = " + n)
    }
  } else {
    var a2 = a + 1
    var b2 = b - 1
    goldbachHelp(n, a2, b2)
  }
}  // End of goldbachHelp()

// End of Warmup.sc