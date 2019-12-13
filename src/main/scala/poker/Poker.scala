package poker

object Poker {
  def chooseWinner(left: Hand, right: Hand): Result = {
    breakTieForDifferentHands(left, right)
      .orElse(breakTieForSameHand(left, right)) match {
      case Known(result) => result
      case Unknown => throw new IllegalStateException(s"Unable to find winner for $left vs $right")
    }
  }

  private def breakTieForDifferentHands(left: Hand, right: Hand): MaybeResult = {
    leftWins(left, right).orElse(rightWins(left, right))
  }

  private def leftWins(left: Hand, right: Hand): MaybeResult = {
    (left, right) match {
      case (l: ThreeOfAKind, _: TwoPair | _: Pair | _: HighCard) => Known(Winner(l))
      case (l: TwoPair, _: Pair | _: HighCard) => Known(Winner(l))
      case (l: Pair, _: HighCard) => Known(Winner(l))
      case _ => Unknown
    }
  }

  private def rightWins(left: Hand, right: Hand): MaybeResult = {
    leftWins(right, left)
  }

  private def breakTieForSameHand(left: Hand, right: Hand): MaybeResult = {
    (left, right) match {
      case (tk1: ThreeOfAKind, tk2: ThreeOfAKind) => tryResolve(tieBreakersForThreeOfAKind)(tk1, tk2)
      case (tp1: TwoPair, tp2: TwoPair) => tryResolve(tieBreakersForTwoPairs)(tp1, tp2)
      case (p1: Pair, p2: Pair) => tryResolve(tieBreakersForPairs)(p1, p2)
      case (hc1: HighCard, hc2: HighCard) => tryResolve(tieBreakersForHighCards)(hc1, hc2)
      case _ => Unknown
    }
  }

  type TieBreaker[T <: Hand] = T => FaceValue

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
      case Nil => Unknown
      case h :: _ if h(left) > h(right) => Known(Winner(left))
      case h :: _ if h(left) < h(right) => Known(Winner(right))
      case h :: Nil if h(left) == h(right) => Known(Tie)
      case h :: rest if h(left) == h(right) => tryResolve(rest)(left, right)
    }
  }

  // effectively a specialised Option for Results
  // run TieBreakers until either a Winner or definite Tie is found, and return Known,
  // or return Unknown if more TieBreakers need to be run.
  // if no more TieBreakers left and result is still unknown, throw exception
  private sealed trait MaybeResult {
    def orElse(other: => MaybeResult): MaybeResult
  }
  private case class  Known(result: Result) extends MaybeResult {
    override def orElse(other: => MaybeResult): MaybeResult = this
  }
  private case object Unknown extends MaybeResult {
    override def orElse(other: => MaybeResult): MaybeResult = other
  }
}
