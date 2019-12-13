package poker

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.HandMatchers

class StraightSpec extends AnyWordSpec with Matchers with HandMatchers {

  "A straight" should {
    "beat any Three of a Kind" in {
      val winningHand = Hand.from(
        Two of Spades,
        Three of Hearts,
        Four of Clubs,
        Five of Diamonds,
        Six of Diamonds
      )

      val losingHand = Hand.from(
        Ace of Spades,
        Ace of Diamonds,
        Ace of Clubs,
        Five of Spades,
        Nine of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat any Two Pair" in {
      val winningHand = Hand.from(
        Two of Spades,
        Three of Hearts,
        Four of Clubs,
        Five of Diamonds,
        Six of Diamonds
      )

      val losingHand = Hand.from(
        Ace of Spades,
        Ace of Diamonds,
        King of Clubs,
        King of Spades,
        Nine of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat any Pair" in {
      val winningHand = Hand.from(
        Two of Spades,
        Three of Hearts,
        Four of Clubs,
        Five of Diamonds,
        Six of Diamonds
      )

      val losingHand = Hand.from(
        Ace of Spades,
        Ace of Diamonds,
        Three of Clubs,
        Five of Spades,
        Nine of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat any High Card hand" in {
      val winningHand = Hand.from(
        Two of Spades,
        Three of Hearts,
        Four of Clubs,
        Five of Diamonds,
        Six of Diamonds
      )

      val losingHand = Hand.from(
        Ace of Spades,
        Two of Diamonds,
        Three of Clubs,
        Five of Spades,
        Nine of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat a Straight where the highest ranked card is higher ranked than the highest ranked card of the losing hand" in {
      val winningHand = Hand.from(
        Two of Spades,
        Three of Hearts,
        Four of Clubs,
        Five of Diamonds,
        Six of Diamonds
      )

      val losingHand = Hand.from(
        Ace of Spades,
        Two of Hearts,
        Three of Clubs,
        Four of Diamonds,
        Five of Spades,
      )

      winningHand should beat(losingHand)
    }

    "tie with a Straight where all cards have the same value" in {
      val firstHand = Hand.from(
        Two of Spades,
        Three of Hearts,
        Four of Clubs,
        Five of Diamonds,
        Six of Diamonds
      )

      val secondHand = Hand.from(
        Two of Diamonds,
        Three of Clubs,
        Four of Hearts,
        Five of Spades,
        Six of Spades
      )

      firstHand should tieWith(secondHand)
    }
  }
}
