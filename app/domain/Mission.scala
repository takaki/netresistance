package domain

import domain.impl.{AllMembers, MissionRound, TeamMembers}

trait MissionTrack {

  def open(TeamMembers: TeamMembers): Either[MissionError, MissionTrack]

  def submit(member: Member, missionCard: MissionCard): Either[MissionError, MissionTrack]

  def close: Either[MissionLog, MissionTrack]

  def history: Seq[MissionResult]

  def round: MissionRound

  def gameWinner: Option[Affiliation]
}

trait MissionResult {
  def teamMembers: TeamMembers

  def isSuccess: Boolean
}

trait Mission {
  def submit(member: Member, missionCard: MissionCard): Either[MissionError, Mission]

  def result(allMembers: AllMembers): Either[MissionLog, MissionResult]
}

trait MissionCriteria {
  def isSuccess(missionCards: Seq[MissionCard]): Boolean
}

trait MissionHistory {
  def history: Seq[MissionResult]

  def add(missionResult: MissionResult): MissionHistory

  def round: MissionRound

  def countSuccess: Int

  def countFail: Int
}

abstract class MissionLog extends LogMessage

case class MissionError(klass: Any, body: String) extends MissionLog with ErrorLog  {
  override def message = s"$klass: $body"
}

