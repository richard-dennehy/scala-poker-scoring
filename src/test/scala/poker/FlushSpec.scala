package poker

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.HandMatchers

class FlushSpec extends AnyWordSpec with Matchers with HandMatchers {

  "A Flush" should {
    "beat any Straight" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Four of Hearts,
        Five of Hearts,
        Six of Hearts,
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
        Four of Hearts,
        Five of Hearts,
        Six of Hearts,
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
        Four of Hearts,
        Five of Hearts,
        Six of Hearts,
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
        Four of Hearts,
        Five of Hearts,
        Six of Hearts,
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
        Four of Hearts,
        Five of Hearts,
        Six of Hearts,
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

    "beat another Flush based on the highest ranked card" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Four of Hearts,
        Five of Hearts,
        Six of Hearts,
        Eight of Hearts
      )

      val losingHand = Hand.from(
        Two of Clubs,
        Four of Clubs,
        Five of Clubs,
        Six of Clubs,
        Seven of Clubs
      )

      winningHand should beat(losingHand)
    }

    "beat another Flush with the same highest ranked card based on the second highest ranked card" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Four of Hearts,
        Five of Hearts,
        Seven of Hearts,
        Eight of Hearts
      )

      val losingHand = Hand.from(
        Two of Clubs,
        Four of Clubs,
        Five of Clubs,
        Six of Clubs,
        Eight of Clubs
      )

      winningHand should beat(losingHand)
    }

    "beat another Flush with the same highest and second highest ranked cards based on the third highest ranked card" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Four of Hearts,
        Six of Hearts,
        Seven of Hearts,
        Eight of Hearts
      )

      val losingHand = Hand.from(
        Two of Clubs,
        Four of Clubs,
        Five of Clubs,
        Seven of Clubs,
        Eight of Clubs
      )

      winningHand should beat(losingHand)
    }

    "beat another Flush with the same highest, second highest, and third highest ranked cards based on the fourth highest ranked card" in {
      val winningHand = Hand.from(
        Two of Hearts,
        Five of Hearts,
        Six of Hearts,
        Seven of Hearts,
        Eight of Hearts
      )

      val losingHand = Hand.from(
        Two of Clubs,
        Four of Clubs,
        Six of Clubs,
        Seven of Clubs,
        Eight of Clubs
      )

      winningHand should beat(losingHand)
    }

    "beat another Flush with the same highest, second highest, third highest, and fourth highest ranked cards based on the lowest ranked card" in {
      val winningHand = Hand.from(
        Three of Hearts,
        Four of Hearts,
        Five of Hearts,
        Seven of Hearts,
        Eight of Hearts
      )

      val losingHand = Hand.from(
        Two of Clubs,
        Four of Clubs,
        Five of Clubs,
        Seven of Clubs,
        Eight of Clubs
      )

      winningHand should beat(losingHand)
    }

    "tie with another Flush with the same face values" in {
      val firstHand = Hand.from(
        Two of Hearts,
        Four of Hearts,
        Six of Hearts,
        Seven of Hearts,
        Eight of Hearts
      )

      val secondHand = Hand.from(
        Two of Clubs,
        Four of Clubs,
        Six of Clubs,
        Seven of Clubs,
        Eight of Clubs
      )

      firstHand should tieWith(secondHand)
    }
  }
}
