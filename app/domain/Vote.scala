package domain

import domain.impl.{TeamMembers, VoteRound}

trait VoteTrack {
  def startTrack: Either[VoteError, VoteTrack]

  def openBallot(teamMembers: TeamMembers): Either[VoteLog, VoteTrack]

  def submit(member: Member, voteToken: VoteToken): Either[VoteLog, VoteTrack]

  def closeBallot: Either[VoteLog, VoteTrack]

  def endTrack: Either[VoteLog, VoteTrack]

  def round: Either[VoteLog, VoteRound]

  def result: Either[VoteLog, VoteResult]

  def spyWin: Either[VoteLog, Boolean]

  def resultHistory: VoteRecord = ???

}

trait VoteRecord {

  def openBallot(teamMembers: TeamMembers): Either[VoteError, VoteRecord]

  def submit(member: Member, voteToken: VoteToken): Either[VoteLog, VoteRecord]

  def closeBallot: Either[VoteLog, VoteRecord]

  def round: VoteRound

  def result: Either[VoteError, VoteResult]

  def spyWin: Boolean

}

trait Ballot {
  def submit(member: Member, voteToken: VoteToken): Either[VoteError, Ballot]

  def result: Either[VoteLog, VoteResult]
}

trait VoteResult {
  def majority: VoteMajority

  def teamMembers: TeamMembers
}


trait VoteHistory {
  def add(voteRecord: VoteRecord): VoteHistory
}

trait VoteMajority

object VoteMajority {

  case object Approved extends VoteMajority

  case object Rejected extends VoteMajority

}

abstract class VoteLog extends LogMessage

case class VoteError(klass: Any, body: String) extends VoteLog with ErrorLog {
  override def message = s"$klass: $body"
}
