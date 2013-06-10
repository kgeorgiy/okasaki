package info.kgeorgiy.okasaki

sealed abstract class Tree[+T]
case object Leaf extends Tree[Nothing]
case class Node[T](l: Tree[T], x: T, r: Tree[T]) extends Tree[T]

object TreeSet {
  def apply[T](implicit ordering: Ordering[T]) = new TreeSet(Leaf, ordering)
}

class TreeSet[T](root: Tree[T], val ordering: Ordering[T]) extends OrderedSet[T] {
  def empty = TreeSet[T](ordering)

  def isEmpty = root match {
    case Leaf => true
    case Node(_, _, _) => false
  }

  def size = {
    def rec(node: Tree[T]): Int = node match {
      case Leaf => 0
      case Node(l, _, r) => rec(l) + 1 + rec(r)
    }
    rec(root)
  }

  def apply(value: T) = {
    def rec(node: Tree[T]): Boolean = node match {
      case Leaf => false
      case Node(l, x, r) =>
        if (ordering.lt(value, x)) rec(l)
        else if (ordering.gt(value, x)) rec(r)
        else true
    }
    rec(root)
  }

  def +(value: T) = {
    def rec(node: Tree[T]): Tree[T] = node match {
      case l@Leaf => Node(l, value, l)
      case n@Node(l, x, r) =>
        if (ordering.lt(value, x)) Node(rec(l), x, r)
        else if (ordering.gt(value, x)) Node(l, x, rec(r))
        else n
    }
    new TreeSet(rec(root), ordering)
  }

  def toSeq = {
    def rec(node: Tree[T]): Seq[T] = node match {
      case Leaf => Seq()
      case Node(l, x, r) => rec(l) ++: x +: rec(r)
    }
    rec(root)
  }
}

//  Exercise 2.2
object TreeSetShortInsert {
  def apply[T](implicit ordering: Ordering[T]) = new TreeSetShortInsert(Leaf, ordering)
}

class TreeSetShortInsert[T](root: Tree[T], ordering: Ordering[T]) extends TreeSet[T](root, ordering) {
  override def +(value: T) = {
    def rec(node: Tree[T], v: Option[T]): Tree[T] = node match {
      case l@Leaf => if (v.exists(ordering.equiv(_, value))) Leaf else Node(l, value, l)
      case n@Node(l, x, r) =>
        if (ordering.lt(value, x)) Node(rec(l, v), x, r)
        else Node(l, x, rec(r, Some(x)))
    }
    new TreeSetShortInsert(rec(root, None), ordering)
  }
}

//  Exercise 2.3
object TreeSetCutInsert {
  def apply[T](implicit ordering: Ordering[T]) = new TreeSetCutInsert(Leaf, ordering)
}

class TreeSetCutInsert[T](root: Tree[T], ordering: Ordering[T]) extends TreeSet[T](root, ordering) {
  override def +(value: T) = {
    def rec(node: Tree[T]): Option[Tree[T]] = node match {
      case l@Leaf => Some(Node(l, value, l))
      case n@Node(l, x, r) =>
        if (ordering.lt(value, x)) rec(l).map(Node(_, x, r))
        else if (ordering.gt(value, x)) rec(r).map(Node(l, x, _))
        else None
    }
    rec(root).map(new TreeSetCutInsert(_, ordering)).getOrElse(this)
  }
}

//  Exercise 2.4
object TreeSetShortCutInsert {
  def apply[T](implicit ordering: Ordering[T]) = new TreeSetShortCutInsert(Leaf, ordering)
}

class TreeSetShortCutInsert[T](root: Tree[T], ordering: Ordering[T]) extends TreeSet[T](root, ordering) {
  override def +(value: T) = {
    def rec(node: Tree[T], v: Option[T]): Option[Tree[T]] = node match {
      case l@Leaf => if (v.exists(ordering.equiv(_, value))) None else Some(Node(l, value, l))
      case n@Node(l, x, r) =>
        if (ordering.lt(value, x)) rec(l, v).map(Node(_, x, r))
        else rec(r, Some(x)).map(Node(l, x, _))
    }
    rec(root, None).map(new TreeSetShortCutInsert(_, ordering)).getOrElse(this)
  }
}

object TreeUtil {
  def iterate[T](value: T)(f: T => T): Stream[T] =  value #:: iterate(f(value))(f)

  //  Exercise 2.5a
  def complete[T](depth: Int, value: T) = iterate(Leaf: Tree[T])(node => Node(node, value, node))(depth)

  //  Exercise 2.5b
  def create[T](size: Int, value: T): Tree[T] = {
    def rec(s: Int): (Tree[T], Tree[T])  =
      if (s == 0) (Leaf, Node(Leaf, value, Leaf))
      else if (s % 2 == 0) rec(s / 2 - 1) match {case (n0, n1) => (Node(n0, value, n1), Node(n1, value, n1))}
      else rec(s / 2) match {case (n0, n1) => (Node(n0, value, n0), Node(n0, value, n1))}
    rec(size)._1
  }
}
