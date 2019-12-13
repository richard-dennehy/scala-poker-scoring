package poker

object Poker {
  def winnerOf(left: Hand, right: Hand): Result = {
    leftWins(left, right)
      .orElse(rightWins(left, right))
      .getOrElse(Tie)
  }

  private def leftWins(left: Hand, right: Hand): Option[Result] = {
    leftHandTypeWins
      .orElse(leftTwoPairWins)
      .orElse(leftPairWins)
      .orElse(leftHighCardWins)
      .lift
      .apply((left, right))
  }

  private def rightWins(left: Hand, right: Hand): Option[Result] = leftWins(right, left)

  type MatchResult = PartialFunction[(Hand, Hand), Result]

  private lazy val leftHandTypeWins: MatchResult = {
    case (tp: TwoPair, _: Pair | _: HighCard) =>
      Winner(tp)
    case (p1: Pair, _: HighCard) =>
      Winner(p1)
  }

  private lazy val leftTwoPairWins: MatchResult = {
    case (tp1: TwoPair, tp2: TwoPair)
      if tp1.highestPair > tp2.highestPair =>
      Winner(tp1)

    case (tp1: TwoPair, tp2: TwoPair)
      if tp1.highestPair == tp2.highestPair &&
        tp1.lowestPair > tp2.lowestPair =>
      Winner(tp1)

    case (tp1: TwoPair, tp2: TwoPair)
      if tp1.highestPair == tp2.highestPair &&
        tp1.lowestPair == tp2.lowestPair &&
        tp1.unused > tp2.unused =>
      Winner(tp1)
  }

  private lazy val leftPairWins: MatchResult = {
    case (p1: Pair, p2: Pair)
      if p1.of > p2.of =>
      Winner(p1)

    case (p1: Pair, p2: Pair)
      if p1.of == p2.of &&
        p1.highestUnused > p2.highestUnused =>
      Winner(p1)

    case (p1: Pair, p2: Pair)
      if p1.of == p2.of &&
        p1.highestUnused == p2.highestUnused &&
        p1.secondUnused > p2.secondUnused =>
      Winner(p1)

    case (p1: Pair, p2: Pair)
      if p1.of == p2.of &&
        p1.highestUnused == p2.highestUnused &&
        p1.secondUnused == p2.secondUnused &&
        p1.thirdUnused > p2.thirdUnused =>
      Winner(p1)
  }

  private lazy val leftHighCardWins: MatchResult = {
    case (hc1: HighCard, hc2: HighCard)
      if hc1.of > hc2.of =>
      Winner(hc1)

    case (hc1: HighCard, hc2: HighCard)
      if hc1.of == hc2.of &&
        hc1.highestUnused > hc2.highestUnused =>
      Winner(hc1)

    case (hc1: HighCard, hc2: HighCard)
      if hc1.of == hc2.of &&
        hc1.highestUnused == hc2.highestUnused &&
        hc1.secondUnused > hc2.secondUnused =>
      Winner(hc1)

    case (hc1: HighCard, hc2: HighCard)
      if hc1.of == hc2.of &&
        hc1.highestUnused == hc2.highestUnused &&
        hc1.secondUnused == hc2.secondUnused &&
        hc1.thirdUnused > hc2.thirdUnused =>
      Winner(hc1)

    case (hc1: HighCard, hc2: HighCard)
      if hc1.of == hc2.of &&
        hc1.highestUnused == hc2.highestUnused &&
        hc1.secondUnused == hc2.secondUnused &&
        hc1.thirdUnused == hc2.thirdUnused &&
        hc1.fourthUnused > hc2.fourthUnused =>
      Winner(hc1)
  }
}
