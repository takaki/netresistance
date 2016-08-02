package domain

import domain.impl.{MissionRound, TeamMembers}

trait ResistanceGame {

  // API
  def assignTeam(leader: Member, members: TeamMembers): Either[LogMessage, ResistanceGame]

  def submitVoteToken(member: Member, voteToken: VoteToken): Either[LogMessage, ResistanceGame]

  def submitMissionCard(member: Member, missionCard: MissionCard): Either[LogMessage, ResistanceGame]

  def assassination(member: Member, guess: Member): Either[LogMessage, ResistanceGame]

  def examination(inquisitor: Member, target: Member): Either[LogMessage, ResistanceGame]

  // util

  def logFlush: ResistanceGame

  // status information

  def winner: Option[Affiliation]

  def log: Seq[LogMessage]

  def leader: Member

  def voteResultHistory: VoteRecord
}

trait EventTable {
  def queueEmpty: Boolean

  def getEvent: Either[LogMessage, GameEvent]

  def finishEvent(): Either[LogMessage, EventTable]

  def afterResistanceWin: Seq[GameEvent]

  def fireBeforeMission(round: MissionRound): EventTable

  def fireAfterResistanceWin: EventTable
}

trait GameEvent {
}

trait MemberSet {
  def size: Int

  def isMember(member: Member): Boolean

  def same(members: Set[Member]): Boolean
}

trait Member {
  def role: Role

  def assignRole(role: Role): Member
}

trait LeaderMarker {
  def leader(members: Seq[Member]): Member

  def rotate: LeaderMarker
}

trait InquisitorToken {
  def inquisitor(members: Seq[Member]): Member

  def isExamined(member: Member, members: Seq[Member]): Boolean

  def pass(target: Member, members: Seq[Member]): InquisitorToken

}

abstract class GameLog extends LogMessage

case class GameError(klass: Any, body: String) extends GameLog with ErrorLog  {
  override def message = s"$klass: $body"
}
