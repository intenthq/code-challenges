package com.intenthq.challenge

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//    It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?

  def nice(xs: List[String]): Int = xs.map(string => isNice(string.toList)).count(_ == true)

  private def isNice(xs: List[Char], vowel: Int = 0, repeat: Boolean = false): Boolean = {
    xs match {
      case Nil => sys.error("Sorry, empty list")
      case last :: Nil => if (isVowel(last)) isLeastThreeVowels(vowel + 1) && repeat else isLeastThreeVowels(vowel) && repeat
      case first :: rest =>
        val notAllowedCombinationDetected = isNotAllowed(first + rest.head.toString)
        if (!notAllowedCombinationDetected){
          val count = if (isVowel(first)) vowel + 1  else vowel
          isNice(rest, count, repeat || repeatedCharacter(first, rest.head) )
        }else
          false
    }
  }

  val disallowedStrings = List("ab", "cd", "pq", "xy")
  val vowels = List('a', 'e', 'i', 'o', 'u')

  private def isLeastThreeVowels(n: Int): Boolean = n >= 3

  private def isInside[A](c: A, l: List[A]): Boolean = l.contains(c)

  private def isVowel(c: Char): Boolean = isInside(c, vowels)

  private def isNotAllowed(pair: String): Boolean = isInside(pair, disallowedStrings)

  private def repeatedCharacter(c: Char, cc: Char): Boolean = c == cc
}
