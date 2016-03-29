package com.mrfunkycold.demo.week4.hw

/**
  * Assignment 4: Huffman coding
  *
  */
object Huffman {

  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
    * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
    * leaves.
    */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree


  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, w) => w
    case Leaf(_, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_,_,cs,_) => cs
    case Leaf(c, w) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def counter(c: Char, chars: List[Char], count: Int): Int = chars match {
      case Nil => count
      case x :: xs => if(x == c) counter(c, xs, count + 1)
                      else counter(c, xs, count)
    }

    def removeFromList(c: Char, chars: List[Char]): List[Char] = chars match {
      case Nil => List()
      case x :: xs => if(x == c) removeFromList(c, xs)
                      else x :: removeFromList(c, xs)
    }

    def loop(chars: List[Char]): List[(Char, Int)] = chars match {
      case Nil => List()
      case c :: cs => (c, counter(c, cs, 1)) :: loop( removeFromList(c, cs) )
    }

    loop(chars)
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def ordering(current: (Char, Int), li: List[Leaf]): List[Leaf] = li match {
      case Nil => List( Leaf(current._1, current._2) )
      case leaf :: xs =>  if(current._2 < weight(leaf)) Leaf(current._1, current._2) :: li
                          else leaf :: ordering(current, xs)
    }

    def loop(f: List[(Char, Int)], li: List[Leaf]): List[Leaf] = f match {
      case Nil => li
      case p :: xs => loop(xs, ordering(p, li))
    }

    loop(freqs, Nil)
  }

  def singleton(trees: List[CodeTree]) = trees.length == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil => List()
    case t1 :: t2 :: xs => makeCodeTree(t1, t2) :: xs
    case List(t1) => trees
  }

  def until(singleton: List[CodeTree] => Boolean,
            combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if(singleton(trees)) trees
    else until(singleton, combine)(combine(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = {
    val t = times(chars)
    val orderedLeafList = makeOrderedLeafList(t)
    until(singleton, combine)(orderedLeafList).head
  }

  // Part 3: Decoding

  type Bit = Int

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree` and returns
    * the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeChars(curr: CodeTree, cBits: List[Bit]): List[Char] = (curr, cBits) match {
      case (Fork(lft, rght, _, _), bit::rest) => decodeChars(if(bit == 0) lft else rght, rest)
      case (Leaf(c, _), _) => c :: decodeChars(tree, cBits)
      case _ => Nil
    }

    decodeChars(tree, bits)
  }

  /**
    * A Huffman coding tree for the French language.
    * Generated from the data given at
    * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
    * What does the secret message say? Can you decode it?
    * For the decoding use the 'frenchCode' Huffman tree defined above.
    */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
    * This function encodes `text` using the code tree `tree`
    * into a sequence of bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeBit(c: Char, t: CodeTree): List[Bit] = t match {
      case Fork(l, r, cs, _) => if(chars(l).contains(c)) 0 :: encodeBit(c, l)
                                else 1 :: encodeBit(c, r)
      case Leaf(c, w) => Nil
    }

    def loop(text: List[Char]): List[Bit] = text match {
      case Nil => Nil
      case x :: xs => encodeBit(x, tree) ::: loop(xs)
    }

    loop(text)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character `char` in
    * the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case Nil => Nil
    case (c, bits) :: rest => if(c == char) bits else codeBits(rest)(char)
  }

  /**
    * Given a code tree, create a code table which contains, for every character in the
    * code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
    * a valid code tree that can be represented as a code table. Using the code tables of the
    * sub-trees, think of how to build the code table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = tree match {
    case Leaf(c, _) => List((c, Nil))
    case Fork(l, r, _, _) => mergeCodeTables(convert(l), convert(r))
  }

  /**
    * This function takes two code tables and merges them into one. Depending on how you
    * use it in the `convert` method above, this merge method might also do some transformations
    * on the two parameter code tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    def prepend(bit: Bit)(code: (Char, List[Bit])) = (code._1, bit :: code._2)

    a.map(prepend(0)) ::: b.map(prepend(1))
  }

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a code table
    * and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val Bits = codeBits(convert(tree))_

    text flatMap Bits
  }
}
