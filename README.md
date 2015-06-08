# Scala-RB-Tree
Scala Red Black Tree Implementation

This is a functional implementation of a red black tree.

Usage example:

    //Get a new empty tree of type int
    //The type has to implement Ordering

    val tree = RBTree.getEmptyTree[Int]

    // Add a bunch of values

    val e = tree addValue(2) addValue(4) addValue(3)  addValue(1)

    // returns the valuses in descending order
    println(e getValues)

    println(e contains(4)) // true

    println(e contains(7)) // false

Installation:
This is meant to be a simple drop-in so no compiled files are provided.  
Just copy the RBTree.scala to your project
