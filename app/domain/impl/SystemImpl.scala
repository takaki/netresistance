package domain.impl

import java.util.UUID

import domain._

case class GameSystemImpl(rooms: Set[PlayRoom] = Set.empty, location: Map[User, PlayRoom] = Map.empty) extends GameSystem {

  override def createRoom(user: User): Either[LogMessage, GameSystem] = {
    for {
      _ <- Either.cond(location.contains(user), (), SystemError(this, s"$user is in room.")).right
      rv <- {
        val room = PlayRoomImpl(user)
        copy(rooms = rooms + room).enterRoom(user, room)
      }.right
    } yield rv
  }

  override def deleteRoom(user: User, room: PlayRoom): Either[LogMessage, GameSystem] = {
    for {
      room <- location.get(user).toRight(SystemError(this, s"$user is not in room.")).right
      _ <- Either.cond(room.owner == user, (), SystemError(this, s"$user is not $room's owner.")).right
      _ <- Either.cond(rooms.contains(room), (), SystemError(this, s"$room does not exist.")).right
      rv <- exitRoom(user).right
    } yield rv
  }

  override def enterRoom(user: User, room: PlayRoom): Either[LogMessage, GameSystem] = {
    for {
      _ <- Either.cond(!location.contains(user), (), SystemError(this, s"$user is in other room.")).right
    } yield copy(location = location.updated(user, room))
  }


  override def exitRoom(user: User): Either[LogMessage, GameSystem] = {
    Either.cond(location.contains(user), copy(location = location - user), SystemError(this, s"$user is not in a room."))
  }
}

case class PlayRoomImpl(owner: User, players: Set[User] = Set.empty,
                        option: GameOption = GameOptionImpl(),
                        game: Option[ResistanceGame] = Option.empty) extends PlayRoom {
  override def startGame: PlayRoom = {
    copy(game = Option(option.resistanceGeme(players)))
  }

  // game
  override def assignTeam(leader: User, members: TeamMembers): Either[LogMessage, PlayRoom] = ???

  //    game.toRight(SystemError(this, "game is not start.")).right.flatMap(_.assignTeam())

  override def submitVoteToken(member: Member, voteToken: VoteToken): PlayRoom = ???

  override def submitMissionCard(member: Member, missionCard: MissionCard): PlayRoom = ???

  //chat
  override def systemMessage(message: Message): Unit = ???

  override def sendMessage(member: Member, message: Message): Unit = ???

  override def privateMessage(to: Member, message: Message): Unit = ???

}

case class GameOptionImpl() extends GameOption {
  override def resistanceGeme(users: Set[User]): ResistanceGame = {
    val spyNum = users.size match {
      case 5 => 2
      case 6 => 2
      case 7 => 3
      case 8 => 3
      case 9 => 3
      case 10 => 4
    }
    ResistanceGameImpl(
      AllMembers.assignRole(users.map(user => MemberImpl(user.uuid.toString)).toSeq,
        List.fill(users.size - spyNum)(Role.Resistance) ++ List.fill(spyNum)(Role.Spy)))
  }

}

case class UserImpl(name: String, uuid: UUID = UUID.randomUUID()) extends User {
  override def rename(name: String): User = this.copy(name = name)
}

