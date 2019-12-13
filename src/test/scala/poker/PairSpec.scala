package poker

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.HandMatchers

class PairSpec extends AnyWordSpec with Matchers with HandMatchers {
  "A pair of any value" should {
    "beat any High Card hand" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Two of Spades,
        Three of Diamonds,
        Four of Diamonds,
        Five of Diamonds,
      )

      val losingHand = Hand.from(
        Ace of Diamonds,
        Three of Diamonds,
        Four of Diamonds,
        Five of Diamonds,
        Seven of Diamonds,
      )

      winningHand should beat(losingHand)
    }

    "beat pair with a lower value" in {
      val winningHand = Hand.from(
        Three of Hearts,
        Three of Spades,
        Four of Diamonds,
        Five of Diamonds,
        King of Spades,
      )

      val losingHand = Hand.from(
        Two of Hearts,
        Two of Spades,
        Three of Diamonds,
        Four of Diamonds,
        Five of Diamonds,
      )

      winningHand should beat(losingHand)
    }

    "beat a pair of the same value if the highest ranking unused card is of higher value" in {
      val winningHand = Hand.from(
        Three of Clubs,
        Three of Diamonds,
        Seven of Spades,
        Jack of Hearts,
        Ace of Diamonds,
      )

      val losingHand = Hand.from(
        Three of Hearts,
        Three of Spades,
        Seven of Diamonds,
        Queen of Clubs,
        King of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat a pair of the same value where the highest ranking unused card is the same in both hands" +
      "if the second-best unused card is of higher value" in {
      val winningHand = Hand.from(
        Three of Clubs,
        Three of Diamonds,
        Seven of Spades,
        Jack of Hearts,
        Ace of Diamonds,
      )

      val losingHand = Hand.from(
        Three of Hearts,
        Three of Spades,
        Seven of Diamonds,
        Ten of Clubs,
        Ace of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat a pair of the same value where the top two highest-ranking unused cards are the same in both hands" +
      "if the lowest ranked unused card is of higher value" in {
      val winningHand = Hand.from(
        Three of Clubs,
        Three of Diamonds,
        Seven of Spades,
        Jack of Hearts,
        Ace of Diamonds,
      )

      val losingHand = Hand.from(
        Three of Hearts,
        Three of Spades,
        Six of Diamonds,
        Jack of Clubs,
        Ace of Hearts
      )

      winningHand should beat(losingHand)
    }

    "tie with another pair if all values are the same" in {
      val firstHand = Hand.from(
        Two of Diamonds,
        Three of Diamonds,
        Four of Diamonds,
        Seven of Spades,
        Seven of Clubs
      )

      val secondHand = Hand.from(
        Two of Hearts,
        Three of Hearts,
        Four of Hearts,
        Seven of Diamonds,
        Seven of Spades
      )

      firstHand should tieWith(secondHand)
    }
  }
}
