// # Warmup:
// f(x) = f(x-1) + f(x-1)
// f(0) = 1

def f(x: Int): Int =
  if x == 0 then 1
  else f(x - 1) + f(x - 1)

// 1. What big-O complexity is your solution?
//      T(f n) = 2 * T(f (n - 1))
//      T(f 0) = 1
//   => T(f n) = 2 ^ n

// 2. Can you do better? Why / why not / how?
//      Yep, try tail recursion which will be optimized into a loop

private def f_helper(carry: Int, x: Int): Int =
  if x == 0 then carry
  else f_helper(carry * 2, x - 1)

def f_tailrec(x: Int): Int = f_helper(1, x)

// # Parsing, Syntax Tree, and Matching Ah great I love these stuff

// 1. Create a suitable data structure to represent the trees in Scala.
enum Tree:
  case Node(trees: List[Tree])
  case Id(value: String)

// 2. Implement a way to check whether two trees are equal. Two trees are equal if they have
// the same identifier, or when they have the same number of children where all two
// children with the same index are equal.

// I think enums are compared by value already but we can make this explicit
def compare_tree(t1: Tree, t2: Tree): Boolean =
  (t1, t2) match
    case (Tree.Id(v1), Tree.Id(v2)) if v1 == v2 => true
    case (Tree.Node(ts1), Tree.Node(ts2)) =>
      ts1.zip(ts2).forall((t1, t2) => compare_tree(t1, t2))
    case _ => false

// 3. Implement a method that returns a string by converting a tree into its representation in TL.
def tree_to_string(t: Tree): String =
  t match
    case Tree.Id(v)    => v
    case Tree.Node(ts) => s"(${ts.map(tree_to_string).mkString(" ")})"

// 4. Implement the reverse, i.e. a function, that takes a string, parses it, and returns the tree
// that was represented by the string in TL. Fail and return a message with a reason if the
// string couldn’t be parsed. Parsing a string produced by your method in 3 should return a
// tree equal to the original one.

// Let's make a rust like result type to make the code cleaner
enum Result[A]:
  case Success(value: A)
  case Failure(msg: String)
  def map[B](f: A => B): Result[B] =
    this match
      case Success(a) => Success(f(a))
      case f          => f.asInstanceOf[Result[B]]
  def flatMap[B](f: A => Result[B]): Result[B] =
    this match
      case Success(a) => f(a)
      case f          => f.asInstanceOf[Result[B]]
  def get =
    this match
      case Success(a) => a
      case _          => ???
  def isSuccess =
    this match
      case Success(_) => true
      case _          => false
  def isFailure =
    this match
      case Failure(_) => true
      case _          => false

val validChar = (('a' to 'z') concat ('A' to 'Z') concat ('0' to '9')).toSet

extension [A](xss: List[List[A]])
  def <*(x: A) =
    xss match
      case Nil => List(List(x))
      case _   => xss.init ++ List(xss.last :+ x)

def trySplit(
    carry: List[List[Char]],
    rest: List[Char],
    bracketCount: Int
): Result[List[String]] =
  (rest, bracketCount) match
    case (Nil, 0) => Result.Success(carry.map(_.mkString))
    case (Nil, _) => Result.Failure("Unmatching brackets")
    // successfully matched bracket for one term
    case (' ' :: xs, 0) => trySplit(carry :+ Nil, xs.dropWhile(_ == ' '), 0)
    // this term has not ended yet
    case (' ' :: xs, c) => trySplit(carry <* ' ', xs, c)
    // try match bracket
    case ('(' :: xs, c) => trySplit(carry <* '(', xs, c + 1)
    case (')' :: xs, c) => trySplit(carry <* ')', xs, c - 1)
    case (x :: xs, c)   => trySplit(carry <* x, xs, c)

def parse_to_tree(input: String): Result[Tree] =
  input match
    case "" => Result.Failure("Empty Identifier")
    case s if s.startsWith("(") && s.endsWith(")") =>
      val body = s.slice(1, s.length - 1)
      if body.isEmpty then Result.Failure("Empty Node body")
      else
        trySplit(Nil, body.toList, 0)
          .flatMap(it =>
            it
              .map(parse_to_tree)
              .foldRight(Result.Success(List[Tree]()))((x, maybeC) =>
                maybeC.flatMap(c => x.map(_ :: c))
              )
          )
          .map(Tree.Node.apply)
    case s =>
      if s.forall(validChar) then Result.Success(Tree.Id(s))
      else Result.Failure(s"$s is not a valid identifier")

def replace(tree: Tree, searchTree: Tree, replacement: Tree): Tree =
  tree match
    case n @ Tree.Node(body) =>
      if compare_tree(n, searchTree) then replacement
      else Tree.Node(body.map(replace(_, searchTree, replacement)))
    case id =>
      if compare_tree(id, searchTree) then replacement
      else id
