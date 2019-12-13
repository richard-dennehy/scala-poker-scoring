package poker

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.HandMatchers

class ThreeOfAKindSpec extends AnyWordSpec with Matchers with HandMatchers {

  "A Three of a Kind" should {
    "beat any Two Pair hand" in {
      val winningHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds,
        King of Diamonds
      )

      val losingHand = Hand.from(
        Ace of Spades,
        Ace of Diamonds,
        Five of Spades,
        Five of Clubs,
        Nine of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat any Pair hand" in {
      val winningHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds,
        King of Diamonds
      )

      val losingHand = Hand.from(
        Ace of Spades,
        Ace of Diamonds,
        Five of Spades,
        Seven of Clubs,
        Nine of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat any High Card hand" in {
      val winningHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds,
        King of Diamonds
      )

      val losingHand = Hand.from(
        Ace of Spades,
        Two of Diamonds,
        Four of Spades,
        Five of Clubs,
        Nine of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat a Three of a Kind hand where the triplicate card has a lower ranking" in {
      val winningHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds,
        King of Diamonds
      )

      val losingHand = Hand.from(
        Two of Spades,
        Two of Hearts,
        Two of Clubs,
        Seven of Diamonds,
        Ace of Diamonds
      )

      winningHand should beat(losingHand)
    }

    // can these even happen?
    "beat a Three of a Kind hand with the same triplicate card " +
      "if the highest ranked unused card is higher ranked than the losing hand's highest ranked unused card" in {
      val winningHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Spades,
        King of Diamonds
      )

      val losingHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds,
        Jack of Diamonds
      )

      winningHand should beat(losingHand)
    }

    "beat a Three of a Kind hand with the same triplicate card " +
      "if the lowest ranked unused card is higher ranked than the losing hand's lowest ranked unused card" in {
      val winningHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds,
        King of Diamonds
      )

      val losingHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Four of Diamonds,
        King of Spades
      )

      winningHand should beat(losingHand)
    }

    "tie with a Three of a Kind hand if all cards have the same value" in {
      val firstHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds,
        King of Diamonds
      )

      val secondHand = Hand.from(
        Three of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds,
        King of Diamonds
      )

      firstHand should tieWith(secondHand)
    }
  }
}
