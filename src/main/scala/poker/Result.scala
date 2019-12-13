package poker

sealed trait Result

case class Winner(hand: Hand) extends Result
case object Tie extends Result
