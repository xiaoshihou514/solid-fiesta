import org.scalatest._
import flatspec._
import matchers._
import scala.language.implicitConversions // yes, I know what I am doing

abstract class UnitSpec
    extends AnyFlatSpec
    with should.Matchers
    with OptionValues
    with Inside
    with Inspectors

given Conversion[String, Tree] with
  def apply(s: String): Tree = Tree.Id(s)

class AnswerSpec extends UnitSpec {
  def pow2(x: Int) = Math.pow(2, x).intValue
  val t1: Tree = "42"
  val t2: Tree = "dsjkLL29a"
  val t3 = Tree.Node(List("ajk", "sdjkl", "22ks99"))
  val t4 = Tree.Node(
    List(
      "asdf",
      t3,
      t1,
      t2,
      Tree.Node(List(Tree.Node(List(Tree.Node(List(t3, t2)), t1)), t3))
    )
  )

  "f" should "give powers of 2" in {
    // we won't test really big values cause it will be slow as hell
    assert(f(5) == pow2(5))
    assert(f(6) == pow2(6))
    assert(f(0) == pow2(0))
    assert(f(1) == pow2(1))
    assert(f(3) == pow2(3))
  }

  "f_tailrec" should "give powers of 2" in {
    assert(f_tailrec(5) == pow2(5))
    assert(f_tailrec(6) == pow2(6))
    assert(f_tailrec(0) == pow2(0))
    assert(f_tailrec(1) == pow2(1))
    assert(f_tailrec(3) == pow2(3))
    assert(f_tailrec(15) == pow2(15))
    assert(f_tailrec(26) == pow2(26))
    assert(f_tailrec(10) == pow2(10))
    assert(f_tailrec(21) == pow2(21))
    assert(f_tailrec(13) == pow2(13))
  }

  "compare_tree" should "compare trees recursively" in {
    assert(compare_tree(t1, t2) == (t1 == t2))
    assert(compare_tree(t1, t3) == (t1 == t3))
    assert(compare_tree(t1, t4) == (t1 == t4))

    assert(compare_tree(t1, t1))
    assert(compare_tree(t2, t2))
    assert(compare_tree(t3, t3))
    assert(compare_tree(t4, t4))
  }

  "tree_to_string" should "give unparsed form" in {
    assert(tree_to_string(t1) == "42")
    assert(tree_to_string(t2) == "dsjkLL29a")
    assert(tree_to_string(t3) == "(ajk sdjkl 22ks99)")
    assert(
      tree_to_string(
        t4
      ) == "(asdf (ajk sdjkl 22ks99) 42 dsjkLL29a ((((ajk sdjkl 22ks99) dsjkLL29a) 42) (ajk sdjkl 22ks99)))"
    )
  }

  "parse_to_tree" should "parse a string into a tree" in {
    assert(parse_to_tree(tree_to_string(t1)).get == t1)
    assert(parse_to_tree(tree_to_string(t2)).get == t2)
    assert(parse_to_tree(tree_to_string(t3)).get == t3)
    assert(parse_to_tree(tree_to_string(t4)).get == t4)
  }

  "parse_to_tree" should "give failure with reason when input is not valid" in {
    assert(parse_to_tree("(a b )").isFailure)
    assert(parse_to_tree("(a b**&& cc)").isFailure)
    assert(parse_to_tree("(a (d e cc)").isFailure)
  }

  "replace" should "replace all occurances in a tree" in {
    assert(tree_to_string(replace(t1, t1, "asdf")) == "asdf")
    // original unchanged
    assert(tree_to_string(t1) == "42")

    assert(
      tree_to_string(
        replace(t4, t3, "kja")
      ) == "(asdf kja 42 dsjkLL29a (((kja dsjkLL29a) 42) kja))"
    )
    // original unchanged
    assert(
      tree_to_string(
        t4
      ) == "(asdf (ajk sdjkl 22ks99) 42 dsjkLL29a ((((ajk sdjkl 22ks99) dsjkLL29a) 42) (ajk sdjkl 22ks99)))"
    )
  }
}
