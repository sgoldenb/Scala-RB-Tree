package RBTree

import scala.annotation.tailrec
import scala.math.Ordering.Implicits._

/**
 * Functional red black tree implementation.
 *
 * TODO deletion
 *
 * Created by sebastian goldenberg.
 */
object RBTree {

  abstract class Tree[T: Ordering]()
  case class Node[T: Ordering](color: Color, left: Tree[T], a: T, right: Tree[T]) extends Tree[T]
  case class Empty[T: Ordering]() extends Tree[T]

  abstract class Color
  case class R() extends Color
  case class B() extends Color

  final def insert[T: Ordering](e: T, tree: Tree[T]): Tree[T] = {

    def build(color: Color, left: Tree[T], e: T, right: Tree[T]): Tree[T] = {
      (color, left, e, right) match {
        case (B(), Node(R(), Node(R(), a, x, b), y, c), z, d) => Node(R(), Node(B(), a, x, b), y, Node(B(), c, z, d))
        case (B(), Node(R(), a, x, Node(R(), b, y, c)), z, d) => Node(R(), Node(B(), a, x, b), y, Node(B(), c, z, d))
        case (B(), a, x, Node(R(), Node(R(), b, y, c), z, d)) => Node(R(), Node(B(), a, x, b), y, Node(B(), c, z, d))
        case (B(), a, x, Node(R(), b, y, Node(R(), c, z, d))) => Node(R(), Node(B(), a, x, b), y, Node(B(), c, z, d))
        case (c, l, x, r) => Node(c, l, x, r)
      }
    }

    // TODO make tailrec
    def ins(tree: Tree[T]): Tree[T] = {
        tree match {
        case Node(col, l, a, r) =>
          if (e < a) {
            build(col, ins(l), a, r)
          }
          else if (e > a) {
            build(col, l, a, ins(r))
          }
          else {
            tree
          }
        case Empty() => Node(R(), Empty(), e, Empty())
      }
    }

    // the first node has to be black
    ins(tree) match {
      case Node(_, l, v, r) => Node(B(), l, v, r)
    }
  }

  @tailrec
  final def isMember[T: Ordering](value: T, tree: Tree[T]): Boolean = {
    tree match {
      case Node(_, left, e, right) =>
        if (value == e) true
        else if (value < e) isMember(value, left)
        else if (value > e) isMember(value, right)
        else false // should never happen

      case Empty() => false
    }
  }

  /**
   * Returns the sorted list of the values in the given tree.
   *
   * @param tree
   *          Tree to get values from.
   * @return
   *         List of values in descending order.
   */
  final def getSortedValues[T: Ordering](tree: Tree[T]): List[T] = {

    /*
     * Tree traversal
     * Should be O(N)
     */
    @tailrec
    def summed(tree: Tree[T], acc: List[T]): List[T] = {
     tree match {
       case Node(_, Empty(), v, Empty()) => summed(Empty(), v::acc)
       case Node(_, Empty(), v, r) => summed(r,  v :: acc)
       case Node(_, Node(_, Empty(), b ,Empty()), v, r) => summed(r,v:: b::acc)
       case Node(_, Node(_, a, b, c), v, r) =>
          // rebuild branch to a simpler problem
          val reb = Node(R(), a, b, Node(R(), c, v, r))
          summed(reb, acc)
       case Empty() => acc
     }
    }
    summed(tree, List[T]())
  }

  /**
   * Constructor for an empty tree.
   * @tparam T
   *           The type of the tree.
   * @return
   *         An empty red black tree.
   */
  def getEmptyTree[T: Ordering] = Empty[T]()

  implicit class utils[T: Ordering](val tree: Tree[T]) {
    def addValue(v: T) = insert(v, tree)
    def getValues = getSortedValues(tree)
    def contains(v: T) = isMember(v, tree)
  }
}

