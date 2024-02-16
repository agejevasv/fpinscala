package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(i) => 0
    case Branch(l, r) => (l.depth + 1).max(r.depth + 1)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(i) => Leaf(f(i))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(i) => f(i)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int =
    this.fold(_ => 1, (ls, rs) => ls + rs + 1)

  def depthViaFold: Int =
    this.fold(_ => 0, (ld, rd) => (ld+1).max(rd+1))

  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(x => Leaf(f(x)), (lm, rm) => Branch(lm, rm))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) =>
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(l, r) => l.maximum.max(r.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(n => n, (lm, rm) => lm.max(rm))
