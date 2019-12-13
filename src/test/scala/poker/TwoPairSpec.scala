package poker

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.HandMatchers

class TwoPairSpec extends AnyWordSpec with Matchers with HandMatchers  {

  "A Two Pair hand" should {
    "beat any Pair hand" in {
      val winningHand = Hand.from(
        Two of Diamonds,
        Two of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds
      )

      val losingHand = Hand.from(
        Four of Diamonds,
        Four of Spades,
        Nine of Clubs,
        Ten of Clubs,
        Jack of Clubs
      )

      winningHand should beat(losingHand)
    }

    "beat any High Card hand" in {
      val winningHand = Hand.from(
        Two of Diamonds,
        Two of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds
      )

      val losingHand = Hand.from(
        Two of Hearts,
        Four of Spades,
        Nine of Clubs,
        Ten of Clubs,
        Jack of Clubs
      )

      winningHand should beat(losingHand)
    }

    "tie with a Two Pair hand, if the unused card has the same rank" in {
      val firstHand = Hand.from(
        Two of Diamonds,
        Two of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds
      )

      val secondHand = Hand.from(
        Two of Clubs,
        Two of Hearts,
        Three of Diamonds,
        Three of Spades,
        Five of Hearts
      )

      firstHand should tieWith(secondHand)
    }

    "beat a Two Pair hand if the highest ranked pair is ranked higher than the losing hand's highest pair" in {
      val winningHand = Hand.from(
        Two of Diamonds,
        Two of Spades,
        Four of Hearts,
        Four of Clubs,
        Five of Diamonds
      )

      val losingHand = Hand.from(
        Two of Clubs,
        Two of Hearts,
        Three of Diamonds,
        Three of Spades,
        Five of Spades
      )

      winningHand should beat(losingHand)
    }

    "lose against a Two Pair if the lowest ranked pair is ranked lower than the winning hand's lowest pair" in {
      val winningHand = Hand.from(
        Three of Hearts,
        Three of Clubs,
        Four of Diamonds,
        Four of Spades,
        Five of Diamonds
      )

      val losingHand = Hand.from(
        Two of Diamonds,
        Two of Spades,
        Four of Hearts,
        Four of Clubs,
        Five of Spades
      )

      losingHand should (not(beat(winningHand)) and not(tieWith(winningHand)))
    }

    "beat a Two Pair hand if the unused card is ranked higher than the losing hand's unused card" in {
      val winningHand = Hand.from(
        Two of Diamonds,
        Two of Spades,
        Three of Hearts,
        Three of Clubs,
        Five of Diamonds
      )

      val losingHand = Hand.from(
        Two of Clubs,
        Two of Hearts,
        Three of Diamonds,
        Three of Spades,
        Four of Hearts
      )

      winningHand should beat(losingHand)
    }
  }
}
