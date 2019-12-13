package poker

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec
import utils.HandMatchers

class HighCardSpec extends AnyWordSpec with Matchers with TableDrivenPropertyChecks with HandMatchers {

  "A High card hand" should {
    "beat a high card hand where the highest card is of lower value" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Two of Diamonds,
        Four of Clubs,
        Six of Hearts,
        Eight of Spades
      )

      val losingHand = Hand.from(
        King of Spades,
        Two of Clubs,
        Four of Hearts,
        Six of Diamonds,
        Eight of Diamonds
      )

      winningHand should beat(losingHand)
    }

    "beat a high card hand where the highest card has the same value, if the second highest card has higher value" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Eight of Spades,
        Two of Diamonds,
        Four of Clubs,
        Six of Hearts,
      )

      val losingHand = Hand.from(
        Ace of Hearts,
        Seven of Diamonds,
        Two of Clubs,
        Four of Hearts,
        Six of Diamonds,
      )

      winningHand should beat(losingHand)
    }

    "beat a high card hand where the top two cards have the same value, if the third highest card has higher value" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Eight of Spades,
        Seven of Hearts,
        Two of Diamonds,
        Four of Clubs,
      )

      val losingHand = Hand.from(
        Ace of Hearts,
        Eight of Diamonds,
        Six of Diamonds,
        Two of Clubs,
        Four of Hearts,
      )

      winningHand should beat(losingHand)
    }

    "beat a high card hand where the top three cards have the same value, if the fourth highest card has higher value" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Eight of Spades,
        Six of Hearts,
        Five of Clubs,
        Two of Diamonds,
      )

      val losingHand = Hand.from(
        Ace of Hearts,
        Seven of Diamonds,
        Six of Diamonds,
        Four of Hearts,
        Two of Clubs,
      )

      winningHand should beat(losingHand)
    }

    "beat a high card hand where the top four cards have the same value, if the fifth highest card has higher value" in {
      val winningHand = Hand.from(
        Ace of Spades,
        Eight of Spades,
        Six of Hearts,
        Four of Clubs,
        Three of Diamonds,
      )

      val losingHand = Hand.from(
        Ace of Hearts,
        Seven of Diamonds,
        Six of Diamonds,
        Four of Hearts,
        Two of Clubs,
      )

      winningHand should beat(losingHand)
    }

    "tie with a high card hand if all cards have the same value" in {
      val firstHand = Hand.from(
        Ace of Spades,
        Eight of Spades,
        Two of Diamonds,
        Four of Clubs,
        Six of Hearts,
      )

      val secondHand = Hand.from(
        Ace of Hearts,
        Eight of Diamonds,
        Two of Clubs,
        Four of Hearts,
        Six of Diamonds,
      )

      firstHand should tieWith(secondHand)
    }
  }
}
