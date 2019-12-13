package poker

sealed trait Hand {
  def prettyPrint: String

  override def toString: String = prettyPrint
}

// base case
case class HighCard(of: Value, highestUnused: Value, secondUnused: Value, thirdUnused: Value, fourthUnused: Value) extends Hand {
  override def prettyPrint: String = s"High card: $of, with $highestUnused, $secondUnused, $thirdUnused, and $fourthUnused"
}

case class Pair(of: Value, highestUnused: Value, secondUnused: Value, thirdUnused: Value) extends Hand {
  override def prettyPrint: String = s"Pair of ${of}s, with $highestUnused, $secondUnused, and $thirdUnused"
}

case class TwoPair(lowestPair: Value, highestPair: Value, unused: Value) extends Hand {
  override def prettyPrint: String = s"Pair of ${highestPair}s and ${lowestPair}s, with $unused"
}

case class ThreeOfAKind(of: Value, unused: (Value, Value)) extends Hand {
  override def prettyPrint: String = s"Three of a Kind of ${of}s, with ${unused._1} and ${unused._2}"
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

    maybeThreeOfAKind(sortedCards)
      .orElse(maybeTwoPair(sortedCards))
      .orElse(maybePair(sortedCards))
      .getOrElse(makeHighCard(sortedCards))
  }

  private def maybeThreeOfAKind(cards: SortedCards): Option[ThreeOfAKind] = {
    Some(cards) collect {
      case List(c1, c2, c3, c4, c5) if haveSameValue(c1, c2, c3) =>
        ThreeOfAKind(c1.value, (c4.value, c5.value))

      case List(c1, c2, c3, c4, c5) if haveSameValue(c2, c3, c4) =>
        ThreeOfAKind(c2.value, (c1.value, c5.value))

      case List(c1, c2, c3, c4, c5) if haveSameValue(c3, c4, c5) =>
        ThreeOfAKind(c3.value, (c1.value, c2.value))
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

  private def findPairIn(sortedCards: SortedCards): Option[(Value, SortedCards)] = {
    sortedCards.sliding(2).collectFirst {
      case List(c1, c2) if c1.value == c2.value => (c1.value, sortedCards.filterNot(_.value == c1.value))
    }
  }

  private def makeHighCard(sortedCards: SortedCards): HighCard = {
    HighCard(sortedCards.last.value, sortedCards(3).value, sortedCards(2).value, sortedCards(1).value, sortedCards.head.value)
  }
}