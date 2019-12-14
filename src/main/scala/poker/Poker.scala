package poker

object Poker {
  def chooseWinner(left: Hand, right: Hand): Result = {
    breakTieForDifferentHands(left, right)
      .orElse(breakTieForSameHand(left, right)) match {
      case Resolved(result) => result
      case Unresolved => throw new IllegalStateException(s"Unable to find winner for $left vs $right")
    }
  }

  private def breakTieForDifferentHands(left: Hand, right: Hand): MaybeResult = {
    if (rankingOf(left) > rankingOf(right)) {
      Resolved(Winner(left))
    } else if (rankingOf(right) > rankingOf(left)) {
      Resolved(Winner(right))
    } else Unresolved
  }

  private def rankingOf(hand: Hand) = hand match {
    case _: FullHouse => 7
    case _: Flush => 6
    case _: Straight => 5
    case _: ThreeOfAKind => 4
    case _: TwoPair => 3
    case _: Pair => 2
    case _: HighCard => 1
  }

  private def breakTieForSameHand(left: Hand, right: Hand): MaybeResult = {
    (left, right) match {
      case (fh1: FullHouse, fh2: FullHouse) => tryResolve(tieBreakersForFullHouse)(fh1, fh2)
      case (f1: Flush, f2: Flush) => tryResolve(tieBreakersForFlush)(f1, f2)
      case (s1: Straight, s2: Straight) => tryResolve(tieBreakersForStraight)(s1, s2)
      case (tk1: ThreeOfAKind, tk2: ThreeOfAKind) => tryResolve(tieBreakersForThreeOfAKind)(tk1, tk2)
      case (tp1: TwoPair, tp2: TwoPair) => tryResolve(tieBreakersForTwoPairs)(tp1, tp2)
      case (p1: Pair, p2: Pair) => tryResolve(tieBreakersForPairs)(p1, p2)
      case (hc1: HighCard, hc2: HighCard) => tryResolve(tieBreakersForHighCards)(hc1, hc2)
      case _ => Unresolved
    }
  }

  type TieBreaker[T <: Hand] = T => FaceValue

  private def tieBreakersForFullHouse: List[TieBreaker[FullHouse]] = {
    List(
      _.triplet,
      _.pair
    )
  }

  private def tieBreakersForFlush: List[TieBreaker[Flush]] = {
    List(
      _.highestRanked,
      _.secondHighest,
      _.thirdHighest,
      _.fourthHighest,
      _.lowestRanked
    )
  }

  private def tieBreakersForStraight: List[TieBreaker[Straight]] = {
    List(_.highest)
  }

  private def tieBreakersForThreeOfAKind: List[TieBreaker[ThreeOfAKind]] = {
    List(
      _.of,
      _.highestUnused,
      _.lowestUnused
    )
  }

  private def tieBreakersForTwoPairs: List[TieBreaker[TwoPair]] = {
    List(
      _.highestPair,
      _.lowestPair,
      _.unused
    )
  }

  private def tieBreakersForPairs: List[TieBreaker[Pair]] = {
    List(
      _.of,
      _.highestUnused,
      _.secondUnused,
      _.thirdUnused
    )
  }

  private def tieBreakersForHighCards: List[TieBreaker[HighCard]] = {
    List(
      _.of,
      _.highestUnused,
      _.secondUnused,
      _.thirdUnused,
      _.fourthUnused
    )
  }

  private def tryResolve[T <: Hand](tieBreakers: List[TieBreaker[T]]): ((T, T) => MaybeResult) = (left, right) => {
    tieBreakers match {
      case Nil => Unresolved
      case h :: _ if h(left) > h(right) => Resolved(Winner(left))
      case h :: _ if h(left) < h(right) => Resolved(Winner(right))
      case h :: Nil if h(left) == h(right) => Resolved(Tie)
      case h :: rest if h(left) == h(right) => tryResolve(rest)(left, right)
    }
  }

  // effectively a specialised Option for Results
  // run TieBreakers until either a Winner or definite Tie is found, and return Resolved,
  // or return Unresolved if more TieBreakers need to be run.
  // if no more TieBreakers left and result is still unknown, throw exception
  private sealed trait MaybeResult {
    def orElse(other: => MaybeResult): MaybeResult
  }
  private case class  Resolved(result: Result) extends MaybeResult {
    override def orElse(other: => MaybeResult): MaybeResult = this
  }
  private case object Unresolved extends MaybeResult {
    override def orElse(other: => MaybeResult): MaybeResult = other
  }
}
