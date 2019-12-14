package poker

sealed trait Hand {
  def prettyPrint: String

  override def toString: String = prettyPrint
}

// base case
case class HighCard(of: FaceValue)(
  val highestUnused: FaceValue,
  val secondUnused: FaceValue,
  val thirdUnused: FaceValue,
  val fourthUnused: FaceValue
) extends Hand {
  override def prettyPrint: String = s"High card: $of, with $highestUnused, $secondUnused, $thirdUnused, and $fourthUnused"
}

case class Pair(of: FaceValue)(
  val highestUnused: FaceValue,
  val secondUnused: FaceValue,
  val thirdUnused: FaceValue
) extends Hand {
  override def prettyPrint: String = s"Pair of ${of}s, with $highestUnused, $secondUnused, and $thirdUnused"
}

case class TwoPair(lowestPair: FaceValue, highestPair: FaceValue)(val unused: FaceValue) extends Hand {
  override def prettyPrint: String = s"Pair of ${highestPair}s and ${lowestPair}s, with $unused"
}

case class ThreeOfAKind(of: FaceValue)(val highestUnused: FaceValue, val lowestUnused: FaceValue) extends Hand {
  override def prettyPrint: String = s"Three of a Kind of ${of}s, with $highestUnused and $lowestUnused"
}

case class Straight(highest: FaceValue) extends Hand {
  override def prettyPrint: String = s"$highest high Straight"
}

case class Flush(suit: Suit)(
  val highestRanked: FaceValue,
  val secondHighest: FaceValue,
  val thirdHighest: FaceValue,
  val fourthHighest: FaceValue,
  val lowestRanked: FaceValue
) extends Hand {
  override def prettyPrint: String = s"$highestRanked high $suit Flush, with $secondHighest, $thirdHighest, $fourthHighest, and $lowestRanked"
}

case class FullHouse(triplet: FaceValue, pair: FaceValue) extends Hand {
  override def prettyPrint: String = s"Full house, ${triplet}s over ${pair}s"
}

case class FourOfAKind(of: FaceValue)(val unused: FaceValue) extends Hand {
  override def prettyPrint: String = s"Four of a kind of ${of}s, with $unused"
}

case class StraightFlush(highest: Card) extends Hand {
  override def prettyPrint: String = if (highest.value == Ace) {
    s"Royal Flush (${highest.suit})"
  } else {
    s"${highest.value} high Straight Flush (${highest.suit})"
  }
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

    maybeStraightFlush(sortedCards)
      .orElse(maybeFourOfAKind(sortedCards))
      .orElse(maybeFullHouse(sortedCards))
      .orElse(maybeFlush(sortedCards))
      .orElse(maybeStraight(sortedCards))
      .orElse(maybeThreeOfAKind(sortedCards))
      .orElse(maybeTwoPair(sortedCards))
      .orElse(maybePair(sortedCards))
      .getOrElse(makeHighCard(sortedCards))
  }

  private def maybeStraightFlush(cards: SortedCards): Option[StraightFlush] = {
    for {
      flush <- maybeFlush(cards)
      straight <- maybeStraight(cards)
    } yield StraightFlush(straight.highest of flush.suit)
  }

  private def maybeFourOfAKind(cards: SortedCards): Option[FourOfAKind] = {
    for {
      (first, rest) <- findPairIn(cards)
      (second, remaining) <- findPairIn(rest)
      if first == second
    } yield FourOfAKind(first)(remaining.head.value)
  }

  private def maybeFullHouse(cards: SortedCards): Option[FullHouse] = {
    findPairIn(cards).collect {
      case (pair, List(c1, c2, c3)) if haveSameValue(c1, c2, c3) =>
        FullHouse(c1.value, pair)
    }
  }

  private def maybeFlush(cards: SortedCards): Option[Flush] = {
    val suits = cards.map(_.suit).toSet

    if (suits.size == 1) {
      Some(Flush(suits.head)(cards(4).value, cards(3).value, cards(2).value, cards(1).value, cards.head.value))
    } else {
      None
    }
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
        ThreeOfAKind(c1.value)(c5.value, c4.value)

      case List(c1, c2, c3, c4, c5) if haveSameValue(c2, c3, c4) =>
        ThreeOfAKind(c2.value)(c5.value, c1.value)

      case List(c1, c2, c3, c4, c5) if haveSameValue(c3, c4, c5) =>
        ThreeOfAKind(c3.value)(c2.value, c1.value)
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
    } yield TwoPair(first, second)(remaining.value)
  }

  private def maybePair(sortedCards: SortedCards): Option[Pair] = {
    findPairIn(sortedCards).map { case (value, rest) =>
      Pair(value)(rest(2).value, rest(1).value, rest.head.value)
    }
  }

  private def findPairIn(sortedCards: SortedCards): Option[(FaceValue, SortedCards)] = {
    Some(sortedCards) collect {
      case c1 :: c2 :: rest if c1.value == c2.value =>
        (c1.value, rest)

      case c1 :: c2 :: c3 :: rest if c2.value == c3.value =>
        (c2.value, c1 :: rest)

      case c1 :: c2 :: c3 :: c4 :: rest if c3.value == c4.value =>
        (c3.value, c1 :: c2 :: rest)

      case c1 :: c2 :: c3 :: c4 :: c5 :: Nil if c4.value == c5.value =>
        (c4.value, c1 :: c2 :: c3 :: Nil)
    }
  }

  private def makeHighCard(sortedCards: SortedCards): HighCard = {
    HighCard(sortedCards.last.value)(sortedCards(3).value, sortedCards(2).value, sortedCards(1).value, sortedCards.head.value)
  }
}