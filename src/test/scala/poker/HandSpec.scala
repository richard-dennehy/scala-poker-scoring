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
          Hand.fromTuple(cards) shouldBe HighCard(Ace)(King, Nine, Five, Two)
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
          Hand.fromTuple(cards) shouldBe Pair(Two)(King, Ten, Three)
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
          Hand.fromTuple(cards) shouldBe Pair(Three)(King, Four, Two)
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
          Hand.fromTuple(cards) shouldBe Pair(Four)(King, Three, Two)
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
          Hand.fromTuple(cards) shouldBe Pair(Ten)(Five, Three, Two)
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
          Hand.fromTuple(cards) shouldBe TwoPair(Three, Four)(Two)
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
          Hand.fromTuple(cards) shouldBe TwoPair(Two, Three)(King)
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
          Hand.fromTuple(cards) shouldBe ThreeOfAKind(Two)(King, Three)
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
          Hand.fromTuple(cards) shouldBe ThreeOfAKind(Three)(King, Two)
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
          Hand.fromTuple(cards) shouldBe ThreeOfAKind(King)(Three, Two)
        }
      }
    }

    "the sorted cards are all adjacent to each other" should {
      "produce a Straight hand of the highest valued card" in {
        forAllPermutationsOf(
          Six of Spades,
          Seven of Hearts,
          Eight of Clubs,
          Nine of Hearts,
          Ten of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Straight(Ten)
        }
      }
    }

    "the cards contain an Ace, a Two, a Three, a Four, and a Five" should {
      "produce a Five high Straight hand" in {
        forAllPermutationsOf(
          Ace of Spades,
          Two of Hearts,
          Three of Clubs,
          Four of Hearts,
          Five of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Straight(Five)
        }
      }
    }

    "the cards contain a Ten, a Jack, a Queen, a King, and an Ace" should {
      "produce an Ace high Straight hand" in {
        forAllPermutationsOf(
          Ten of Spades,
          Jack of Hearts,
          Queen of Clubs,
          King of Hearts,
          Ace of Diamonds
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Straight(Ace)
        }
      }
    }

    "all cards have the same suit, and do not form a straight" should {
      "produce a Flush hand of the highest ranked card" in {
        forAllPermutationsOf(
          Two of Hearts,
          Four of Hearts,
          Seven of Hearts,
          Ten of Hearts,
          King of Hearts
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Flush(Hearts)(King, Ten, Seven, Four, Two)
        }
      }
    }

    // can't happen in a single deck game, but doesn't hurt to check
    "all cards have the same suit, and three of the face values are the same" should {
      "produce a Flush hand of the highest ranked card" in {
        forAllPermutationsOf(
          Two of Hearts,
          Two of Hearts,
          Two of Hearts,
          Ten of Hearts,
          King of Hearts
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Flush(Hearts)(King, Ten, Two, Two, Two)
        }
      }
    }

    "all cards have the same suit, and there are two pairs of face values" should {
      "produce a Flush hand of the highest ranked card" in {
        forAllPermutationsOf(
          Two of Hearts,
          Two of Hearts,
          Ten of Hearts,
          Ten of Hearts,
          King of Hearts
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Flush(Hearts)(King, Ten, Ten, Two, Two)
        }
      }
    }

    "all cards have the same suit, and two of the face values are the same" should {
      "produce a Flush hand of the highest ranked card" in {
        forAllPermutationsOf(
          Two of Hearts,
          Two of Hearts,
          Five of Hearts,
          Ten of Hearts,
          King of Hearts
        ) { cards =>
          Hand.fromTuple(cards) shouldBe Flush(Hearts)(King, Ten, Five, Two, Two)
        }
      }
    }

    "the cards contain a pair of the same face value and a triple of the same face value" should {
      "produce a Full House hand of the duplicate and triplicate values" in {
        forAllPermutationsOf(
          Ace of Spades,
          Ace of Hearts,
          Ace of Clubs,
          King of Diamonds,
          King of Spades
        ) { cards =>
          Hand.fromTuple(cards) shouldBe FullHouse(Ace, King)
        }
      }
    }

    "the cards contain four cards of the same value" should {
      "produce a Four of a Kind of the quadruplet" in {
        forAllPermutationsOf(
          Two of Spades,
          Two of Clubs,
          Two of Diamonds,
          Two of Hearts,
          Ace of Spades
        ) { cards =>
          Hand.fromTuple(cards) shouldBe FourOfAKind(Two)(Ace)
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
