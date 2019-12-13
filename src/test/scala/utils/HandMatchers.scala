package utils

import org.scalatest.matchers.{MatchResult, Matcher}
import poker.{Hand, Poker, Tie, Winner}

trait HandMatchers {
  def beat(rightHand: Hand): Matcher[Hand] = (leftHand: Hand) => {
    val firstWinner = Poker.chooseWinner(leftHand, rightHand)
    val winsOnLeftSide = firstWinner == Winner(leftHand)

    val secondWinner = Poker.chooseWinner(rightHand, leftHand)
    val winsOnRightSide = secondWinner == Winner(leftHand)

    MatchResult(
      winsOnLeftSide && winsOnRightSide,
      if (firstWinner == secondWinner) {
        s"$leftHand did not beat $rightHand - when passed as first argument: $firstWinner; when passed as second argument: $secondWinner"
      } else {
        s"$leftHand did not consistently win against $rightHand - when passed as first argument: $firstWinner; when passed as second argument: $secondWinner"
      },
      if (firstWinner == secondWinner) {
        s"$leftHand did not lose against $rightHand - when passed as first argument: $firstWinner; when passed as second argument: $secondWinner"
      } else {
        s"$leftHand did not consistently lose against $rightHand - when passed as first argument: $firstWinner; when passed as second argument: $secondWinner"
      }
    )
  }

  def tieWith(rightHand: Hand): Matcher[Hand] = (leftHand: Hand) => {
    val firstTie = Poker.chooseWinner(leftHand, rightHand)
    val tiesOnLeftSide = firstTie == Tie

    val secondTie = Poker.chooseWinner(rightHand, leftHand)
    val tiesOnRightSide = secondTie == Tie

    MatchResult(
      tiesOnLeftSide && tiesOnRightSide,
      if (firstTie == secondTie) {
        s"$leftHand did not tie with $rightHand"
      } else {
        s"$leftHand did not consistently tie with $rightHand - when passed as first argument: $firstTie; when passed as second argument: $secondTie"
      },
      if (firstTie == secondTie) {
        s"$leftHand tied with $rightHand"
      } else {
        s"$leftHand inconsistently tied with $rightHand - when passed as first argument: $firstTie; when passed as second argument: $secondTie"
      }
    )
  }
}
