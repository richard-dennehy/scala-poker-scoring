package poker

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.HandMatchers

class FourOfAKindSpec extends AnyWordSpec with Matchers with HandMatchers {

  "A Four of a Kind" should {
    "beat any Full House" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
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
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
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
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
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
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
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
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
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
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
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
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
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

    "beat another Four of a Kind based on the quadruplet value" in {
      val winningHand = Hand.from(
        Three of Hearts,
        Three of Clubs,
        Three of Diamonds,
        Three of Spades,
        Seven of Hearts
      )

      val losingHand = Hand.from(
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat another Four of a Kind with the same quadruplet based on the unused card's face value" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
      )

      val losingHand = Hand.from(
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Six of Hearts
      )

      winningHand should beat(losingHand)
    }

    "tie with another Four of a Kind with the same face values" in {
      val firstHand = Hand.from(
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
      )

      val secondHand = Hand.from(
        Two of Hearts,
        Two of Clubs,
        Two of Diamonds,
        Two of Spades,
        Seven of Hearts
      )

      firstHand should tieWith(secondHand)
    }
  }
}
