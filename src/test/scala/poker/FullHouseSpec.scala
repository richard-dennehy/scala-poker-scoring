package poker

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.HandMatchers

class FullHouseSpec extends AnyWordSpec with Matchers with HandMatchers {

  "A Full House" should {
    "beat any Flush" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Two of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
      )

      val losingHand = Hand.from(
        Two of Hearts,
        Four of Hearts,
        Five of Hearts,
        Six of Hearts,
        Eight of Hearts
      )

      winningHand should beat(losingHand)
    }

    "beat any Straight" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Two of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
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
        Two of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
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
        Two of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
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
        Two of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
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
        Two of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
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

    "beat another Full House based on the rank of the triplet" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Two of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
      )

      val losingHand = Hand.from(
        Three of Hearts,
        Three of Spades,
        Four of Hearts,
        Four of Spades,
        Four of Clubs
      )

      winningHand should beat(losingHand)
    }

    "beat another Full House with the same triplet based on the rank of the pair" in {
      val winningHand = Hand.from(
        Three of Hearts,
        Three of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
      )

      val losingHand = Hand.from(
        Two of Hearts,
        Two of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
      )

      winningHand should beat(losingHand)
    }

    "tie with another Full House with the same triplet and pair" in {
      val firstHand = Hand.from(
        Two of Hearts,
        Two of Spades,
        Five of Hearts,
        Five of Spades,
        Five of Clubs
      )

      val secondHand = Hand.from(
        Two of Diamonds,
        Two of Clubs,
        Five of Diamonds,
        Five of Clubs,
        Five of Spades
      )

      firstHand should tieWith(secondHand)
    }
  }
}
