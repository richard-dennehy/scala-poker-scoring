package poker

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HandSpec extends AnyWordSpec with Matchers {
  "creating a hand" when {
    "the cards do not form a hand" should {
      "produce a HighCard of the highest value card" in {
        forAllPermutationsOf(
          Ace of Spades,
          Two of Hearts,
          Five of Diamonds,
          Nine of Clubs,
          King of Spades
        ) { cards =>
          Hand.fromTuple(cards) shouldBe HighCard(Ace, King, Nine, Five, Two)
        }
      }
    }

    "the cards contain two cards with the same value, and three higher valued cards" should {
      "produce a Pair of the duplicate value" in {
        forAllPermutationsOf(
          Two of Spades,
          Two of Diamonds,
          Three of Hearts,
          Ten of Clubs,
          King of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Pair(Two, King, Ten, Three)
        }
      }
    }

    "the cards contain two cards with the same value, two higher valued cards, and a lower valued card" should {
      "produce a Pair of the duplicate value" in {
        forAllPermutationsOf(
          Two of Spades,
          Three of Diamonds,
          Three of Hearts,
          Four of Clubs,
          King of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Pair(Three, King, Four, Two)
        }
      }
    }

    "the cards contain two cards with the same value, a higher valued card, and two lower valued cards" should {
      "produce a Pair of the duplicate value" in {
        forAllPermutationsOf(
          Two of Spades,
          Three of Diamonds,
          Four of Hearts,
          Four of Clubs,
          King of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Pair(Four, King, Three, Two)
        }
      }
    }

    "the cards contain two cards with the same value, and three lower valued cards" should {
      "produce a Pair of the duplicate value" in {
        forAllPermutationsOf(
          Two of Spades,
          Three of Diamonds,
          Five of Hearts,
          Ten of Clubs,
          Ten of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Pair(Ten, Five, Three, Two)
        }
      }
    }

    "the cards contain two pairs of values, and a lower valued card" should {
      "produce a Two Pair of the duplicate values" in {
        forAllPermutationsOf(
          Two of Clubs,
          Three of Spades,
          Three of Diamonds,
          Four of Clubs,
          Four of Hearts
        ) { cards =>
          Hand.fromTuple(cards) shouldBe TwoPair(Three, Four, Two)
        }
      }
    }

    "the cards contain two pairs of values, and a higher valued card" should {
      "produce a Two Pair of the duplicate values" in {
        forAllPermutationsOf(
          Two of Spades,
          Two of Diamonds,
          Three of Clubs,
          Three of Hearts,
          King of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe TwoPair(Two, Three, King)
        }
      }
    }

    "the cards contain a three cards of the same value, and two higher valued cards" should {
      "produce a Three of a kind of the triplicate value" in {
        forAllPermutationsOf(
          Two of Spades,
          Two of Diamonds,
          Two of Clubs,
          Three of Hearts,
          King of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe ThreeOfAKind(Two, King, Three)
        }
      }
    }

    "the cards contain a three cards of the same value, a lower valued card, and a higher valued card" should {
      "produce a Three of a kind of the triplicate value" in {
        forAllPermutationsOf(
          Two of Spades,
          Three of Clubs,
          Three of Diamonds,
          Three of Spades,
          King of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe ThreeOfAKind(Three, King, Two)
        }
      }
    }

    "the cards contain a three cards of the same value, and two lower valued cards" should {
      "produce a Three of a kind of the triplicate value" in {
        forAllPermutationsOf(
          Two of Spades,
          Three of Clubs,
          King of Diamonds,
          King of Spades,
          King of Hearts
        ) { cards =>
          Hand.fromTuple(cards) shouldBe ThreeOfAKind(King, Three, Two)
        }
      }
    }
  }

  private def forAllPermutationsOf(
    first: Card,
    second: Card,
    third: Card,
    fourth: Card,
    fifth: Card
  )(assertion: ((Card, Card, Card, Card, Card)) => Assertion): Unit = {
    val permutations = List(first, second, third, fourth, fifth).permutations

    permutations.foreach { perm =>
      assertion(perm.head, perm(1), perm(2), perm(3), perm(4))
    }
  }
}
