package domain

import java.util.UUID

import domain.impl.TeamMembers

trait GameSystem {

  def createRoom(user: User): Either[LogMessage, GameSystem]

  def deleteRoom(user: User, room: PlayRoom): Either[LogMessage, GameSystem]

  def enterRoom(user: User, room: PlayRoom): Either[LogMessage, GameSystem]

  def exitRoom(user: User): Either[LogMessage, GameSystem]

}


trait PlayRoom {
  def owner: User

  def startGame: PlayRoom

  // game
  def assignTeam(leader: User, members: TeamMembers): Either[LogMessage, PlayRoom]

  def submitVoteToken(member: Member, voteToken: VoteToken): Either[LogMessage, PlayRoom]

  def submitMissionCard(member: Member, missionCard: MissionCard): Either[LogMessage, PlayRoom]


  //chat
  def systemMessage(message: Message)

  def sendMessage(member: Member, message: Message)

  def privateMessage(to: Member, message: Message)

}

trait GameOption {
  def resistanceGeme(users: Set[User): ResistanceGame
}

trait Message {

}

trait User {
  def uuid: UUID

  def rename(name: String): User

}

abstract class SystemLog extends LogMessage

case class SystemError(klass: Any, body: String) extends SystemLog with ErrorLog {
  override def message = s"$klass: $body"
}
