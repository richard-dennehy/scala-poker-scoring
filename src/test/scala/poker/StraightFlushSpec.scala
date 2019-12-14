package poker

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.HandMatchers

class StraightFlushSpec extends AnyWordSpec with Matchers with HandMatchers {
  "A Straight Flush" should {
    "beat any Four of a Kind" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Two of Spades,
        Three of Spades,
        Four of Spades,
        Five of Spades
      )

      val losingHand = Hand.from(
        King of Spades,
        King of Diamonds,
        King of Hearts,
        King of Clubs,
        Ace of Diamonds
      )

      winningHand should beat(losingHand)
    }

    "beat any Full House" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Two of Spades,
        Three of Spades,
        Four of Spades,
        Five of Spades
      )

      val losingHand = Hand.from(
        Six of Spades,
        Six of Diamonds,
        Six of Hearts,
        Ace of Clubs,
        Ace of Spades
      )

      winningHand should beat(losingHand)
    }

    "beat any Flush" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Two of Spades,
        Three of Spades,
        Four of Spades,
        Five of Spades
      )

      val losingHand = Hand.from(
        Six of Diamonds,
        Jack of Diamonds,
        Queen of Diamonds,
        King of Diamonds,
        Ace of Diamonds
      )

      winningHand should beat(losingHand)
    }

    "beat any Straight" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Two of Spades,
        Three of Spades,
        Four of Spades,
        Five of Spades
      )

      val losingHand = Hand.from(
        Ten of Spades,
        Jack of Diamonds,
        Queen of Hearts,
        King of Clubs,
        Ace of Spades
      )

      winningHand should beat(losingHand)
    }

    "beat any Three of a Kind" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Two of Spades,
        Three of Spades,
        Four of Spades,
        Five of Spades
      )

      val losingHand = Hand.from(
        Ten of Spades,
        Ten of Diamonds,
        Ten of Hearts,
        King of Clubs,
        Ace of Spades
      )

      winningHand should beat(losingHand)
    }

    "beat any Two Pair" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Two of Spades,
        Three of Spades,
        Four of Spades,
        Five of Spades
      )

      val losingHand = Hand.from(
        Ten of Spades,
        Ten of Diamonds,
        King of Hearts,
        King of Clubs,
        Ace of Spades
      )

      winningHand should beat(losingHand)
    }

    "beat any Pair" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Two of Spades,
        Three of Spades,
        Four of Spades,
        Five of Spades
      )

      val losingHand = Hand.from(
        Ten of Spades,
        Ten of Diamonds,
        Queen of Hearts,
        King of Clubs,
        Ace of Spades
      )

      winningHand should beat(losingHand)
    }

    "beat any High Card" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Two of Spades,
        Three of Spades,
        Four of Spades,
        Five of Spades
      )

      val losingHand = Hand.from(
        Six of Spades,
        Jack of Diamonds,
        Queen of Hearts,
        King of Clubs,
        Ace of Spades
      )

      winningHand should beat(losingHand)
    }

    "beat a Straight Flush based on the highest ranking face value" in {
      val winningHand = Hand.from(
        Ace of Spades,
        King of Spades,
        Queen of Spades,
        Jack of Spades,
        Ten of Spades
      )

      val losingHand = Hand.from(
        Ace of Hearts,
        Two of Hearts,
        Three of Hearts,
        Four of Hearts,
        Five of Hearts
      )

      winningHand should beat(losingHand)
    }

    "tie with a Straight Flush if all face values are the same" in {
      val firstHand = Hand.from(
        Ace of Spades,
        King of Spades,
        Queen of Spades,
        Jack of Spades,
        Ten of Spades
      )

      val secondHand = Hand.from(
        Ace of Hearts,
        King of Hearts,
        Queen of Hearts,
        Jack of Hearts,
        Ten of Hearts
      )

      firstHand should tieWith(secondHand)
    }
  }
}
