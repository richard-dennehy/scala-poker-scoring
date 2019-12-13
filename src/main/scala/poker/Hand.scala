package poker

sealed trait Hand {
  def prettyPrint: String

  override def toString: String = prettyPrint
}

// base case
case class HighCard(of: FaceValue, highestUnused: FaceValue, secondUnused: FaceValue, thirdUnused: FaceValue, fourthUnused: FaceValue) extends Hand {
  override def prettyPrint: String = s"High card: $of, with $highestUnused, $secondUnused, $thirdUnused, and $fourthUnused"
}

case class Pair(of: FaceValue, highestUnused: FaceValue, secondUnused: FaceValue, thirdUnused: FaceValue) extends Hand {
  override def prettyPrint: String = s"Pair of ${of}s, with $highestUnused, $secondUnused, and $thirdUnused"
}

case class TwoPair(lowestPair: FaceValue, highestPair: FaceValue, unused: FaceValue) extends Hand {
  override def prettyPrint: String = s"Pair of ${highestPair}s and ${lowestPair}s, with $unused"
}

case class ThreeOfAKind(of: FaceValue, highestUnused: FaceValue, lowestUnused: FaceValue) extends Hand {
  override def prettyPrint: String = s"Three of a Kind of ${of}s, with $highestUnused and $lowestUnused"
}

case class Straight(highest: FaceValue) extends Hand {
  override def prettyPrint: String = s"$highest high Straight"
}

object Hand {
  def fromTuple(
    cards: (Card, Card, Card, Card, Card)
  ): Hand = {
    val (first, second, third, fourth, fifth) = cards
    from(first, second, third, fourth, fifth)
  }

  def from(
    first: Card,
    second: Card,
    third: Card,
    fourth: Card,
    fifth: Card,
  ): Hand = {
    val sortedCards = List(first, second, third, fourth, fifth).sorted

    maybeStraight(sortedCards)
      .orElse(maybeThreeOfAKind(sortedCards))
      .orElse(maybeTwoPair(sortedCards))
      .orElse(maybePair(sortedCards))
      .getOrElse(makeHighCard(sortedCards))
  }

  private def maybeStraight(cards: SortedCards): Option[Straight] = {
    Some(cards.map(_.value)) collect {
      case List(Two, Three, Four, Five, Ace) => Straight(Five) // Ace gets sorted after Five
      case List(Ten, Jack, Queen, King, Ace) => Straight(Ace)
      case List(v1, v2, v3, v4, v5)
        if v5.ranking - v4.ranking == 1 &&
          v4.ranking - v3.ranking == 1 &&
          v3.ranking - v2.ranking == 1 &&
          v2.ranking - v1.ranking == 1 =>
        Straight(v5)
    }
  }

  private def maybeThreeOfAKind(cards: SortedCards): Option[ThreeOfAKind] = {
    Some(cards) collect {
      case List(c1, c2, c3, c4, c5) if haveSameValue(c1, c2, c3) =>
        ThreeOfAKind(c1.value, c5.value, c4.value)

      case List(c1, c2, c3, c4, c5) if haveSameValue(c2, c3, c4) =>
        ThreeOfAKind(c2.value, c5.value, c1.value)

      case List(c1, c2, c3, c4, c5) if haveSameValue(c3, c4, c5) =>
        ThreeOfAKind(c3.value, c2.value, c1.value)
    }
  }

  private def haveSameValue(c1: Card, c2: Card, c3: Card): Boolean = {
    c1.value == c2.value && c2.value == c3.value
  }

  private def maybeTwoPair(sortedCards: SortedCards): Option[TwoPair] = {
    for {
      (first, rest) <- findPairIn(sortedCards)
      (second, last) <- findPairIn(rest)
      remaining <- last.headOption
    } yield TwoPair(first, second, remaining.value)
  }

  private def maybePair(sortedCards: SortedCards): Option[Pair] = {
    Some(sortedCards) collect {
      case List(c1, c2, c3, c4, c5) if c1.value == c2.value =>
        Pair(c1.value, c5.value, c4.value, c3.value)

      case List(c1, c2, c3, c4, c5) if c2.value == c3.value =>
        Pair(c2.value, c5.value, c4.value, c1.value)

      case List(c1, c2, c3, c4, c5) if c3.value == c4.value =>
        Pair(c3.value, c5.value, c2.value, c1.value)

      case List(c1, c2, c3, c4, c5) if c4.value == c5.value =>
        Pair(c4.value, c3.value, c2.value, c1.value)
    }
  }

  private def findPairIn(sortedCards: SortedCards): Option[(FaceValue, SortedCards)] = {
    sortedCards.sliding(2).collectFirst {
      case List(c1, c2) if c1.value == c2.value => (c1.value, sortedCards.filterNot(_.value == c1.value))
    }
  }

  private def makeHighCard(sortedCards: SortedCards): HighCard = {
    HighCard(sortedCards.last.value, sortedCards(3).value, sortedCards(2).value, sortedCards(1).value, sortedCards.head.value)
  }
}