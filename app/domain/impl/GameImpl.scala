package domain.impl

import domain._

import scala.util.Random

//case class MemberImpl(name: String, uuid: UUID = randomUUID(), role: Role = Role.Resistance) extends Member {

case class MemberImpl(name: String, role: Role = Role.Resistance) extends Member {
  override def assignRole(role: Role): Member = this.copy(role = role)
}

case class AllMembers(members: Seq[Member],
                      leaderMarker: LeaderMarker = LeaderMarkerImpl(),
                      inquisitorToken: Option[InquisitorToken] = None) extends MemberSet {

  require(members.size == members.toSet.size)
  require(5 to 10 contains members.size)
  require(members.count(_.role.affiliation == Affiliation.Spy) == spyNum)

  def spyNum = members.size match {
    case 5 => 2
    case 6 => 2
    case 7 => 3
    case 8 => 3
    case 9 => 3
    case 10 => 4
  }

  def teamMemberSize(missionRound: MissionRound): Int = {
    (members.size match {
      case 5 => Vector(2, 3, 2, 3, 3)
      case 6 => Vector(2, 3, 4, 3, 4)
      case 7 => Vector(2, 3, 3, 4, 4)
      case 8 => Vector(3, 4, 4, 5, 5)
      case 9 => Vector(3, 4, 4, 5, 5)
      case 10 => Vector(3, 4, 4, 5, 5)
    }) (missionRound.value - 1)
  }

  def requiredFails(missionRound: MissionRound): Int = {
    members.size match {
      case x if (7 to 10 contains x) && missionRound.value == 4 => 2
      case _ => 1
    }
  }


  override def isMember(member: Member): Boolean = members.contains(member)

  override def same(members: Set[Member]): Boolean = this.members.toSet == members

  override def size: Int = members.size

  def leader = leaderMarker.leader(members)

  def rotateLeader: AllMembers = this.copy(leaderMarker = leaderMarker.rotate)

  def inquisitor: Option[Member] = inquisitorToken.map(_.inquisitor(members))

  def passInquisitor(target: Member): Option[AllMembers] = inquisitorToken.map(token => this.copy(inquisitorToken = Option(token.pass(target, members))))

  def isExamined(target: Member): Option[Boolean] = inquisitorToken.map(_.isExamined(target, members))

}

object AllMembers {
  def assignRole(members: Seq[Member], roles: Seq[Role], shuffle: Boolean = true): AllMembers = {
    require(members.size == roles.size)
    AllMembers(members.zip(if (shuffle) Random.shuffle(roles) else roles)
      .map { case (member, role) => member.assignRole(role) })
  }
}

case class TeamMembers(members: Set[Member]) extends MemberSet {
  override def isMember(member: Member): Boolean = members.contains(member)

  override def same(members: Set[Member]): Boolean = this.members == members

  override def size: Int = members.size
}

case class LogQueueImpl(logs: List[LogMessage] = List.empty) extends LogQueue {
  override def add(logMessage: LogMessage) = LogQueueImpl(logMessage :: logs)

  override def queue: Seq[LogMessage] = logs.reverse
}

case class LeaderMarkerImpl(marker: Int = 0) extends LeaderMarker {
  override def leader(members: Seq[Member]): Member = members(marker % members.size)

  override def rotate: LeaderMarker = this.copy(marker = marker + 1)
}

case class InquisitorTokenImpl(position: Int = -1, examined: Set[Member] = Set.empty) extends InquisitorToken {
  override def inquisitor(members: Seq[Member]): Member = {
    if (position < 0) members.last else members(position)
  }

  override def isExamined(member: Member, members: Seq[Member]): Boolean = examined.contains(member) || member == members.last

  override def pass(target: Member, members: Seq[Member]): InquisitorToken = {
    this.copy(position = members.indexOf(target), examined = examined + target)
  }
}

case class ResistanceGameImpl(allMembers: AllMembers,
                              voteTrack: VoteTrack,
                              missionTrack: MissionTrack,
                              eventQueue: EventTable,
                              winner: Option[Affiliation] = None,
                              logQueue: LogQueue = LogQueueImpl()
                             ) extends ResistanceGame {

  private def checkEventEmpty = Either.cond(eventQueue.queueEmpty, this, GameError(this, "event running"))

  override def assignTeam(leader: Member, teamMembers: TeamMembers): Either[LogMessage, ResistanceGame] = {
    for {
      game <- checkEventEmpty.right
      _ <- Either.cond(leader == allMembers.leader, (), GameError(this, "startTemVote: not leader")).right
    } yield voteTrack.openBallot(teamMembers).fold(game.addLog(_), voteTrack => game.copy(voteTrack = voteTrack))
  }

  override def submitVoteToken(member: Member, voteToken: VoteToken): Either[LogMessage, ResistanceGame] = {
    val submitted = checkEventEmpty.right
      .map(game => voteTrack.submit(member, voteToken).fold(game.addLog(_),
        voteTrack => game.copy(voteTrack = voteTrack).addLog(GameVoteSubmitted(member, voteToken))))
    val closed = submitted.right
      .map(game => game.voteTrack.closeBallot.fold(game.addLog(_),
        voteTrack => game.copy(voteTrack = voteTrack, allMembers = allMembers.rotateLeader)))
    closed.right.flatMap(game => game.voteTrack.result.fold(log => Right(game.addLog(log)),
      result =>
        if (result.majority == VoteMajority.Approved) {
          for {
            voteTrack <- game.voteTrack.endTrack.right
            missionTrack <- missionTrack.open(result.teamMembers).right
          } yield {
            game.copy(voteTrack = voteTrack, missionTrack = missionTrack).addLog(GameVoteApprove)
          }
        } else {
          game.voteTrack.spyWin.right.map(spyWin =>
            if (!spyWin)
              game.addLog(GameVoteReject)
            else
              game.copy(winner = Option(Affiliation.Spy)).addLog(GameVoteReject).addLog(GameSpyWinFiveVoteRejects))
        }))
  }

  override def submitMissionCard(member: Member, missionCard: MissionCard): Either[LogMessage, ResistanceGame] = {
    val submitted = checkEventEmpty.right.map(game => game.missionTrack.submit(member, missionCard)
      .fold(game.addLog(_),
        missionTrack => game.copy(missionTrack = missionTrack).addLog(GameMissionSubmit(member, missionCard))))
    submitted.right.flatMap(game => game.missionTrack.close.fold(log => Right(game.addLog(log)),
      missionTrack => {
        val logged = game.addLog(GameMissionClosed(missionTrack.history.last.isSuccess))
        missionTrack.gameWinner match {
          case Some(Affiliation.Resistance) => Right(if (eventQueue.afterResistanceWin.isEmpty) {
            logged.copy(winner = Option(Affiliation.Resistance))
              .addLog(GameResistanceWin)
          } else {
            logged.copy(eventQueue = eventQueue.fireAfterResistanceWin)
          })
          case Some(Affiliation.Spy) => Right(logged.copy(winner = Option(Affiliation.Spy)).addLog(GameSpyWin))
          case None => logged.voteTrack.startTrack.right.map(voteTrack =>
            logged.copy(eventQueue = eventQueue.fireBeforeMission(missionTrack.round),
              missionTrack = missionTrack, voteTrack = voteTrack))
        }
      }))
  }

  override def assassination(assassin: Member, target: Member): Either[LogMessage, ResistanceGame] = {
    for {
      event <- eventQueue.getEvent.right
      _ <- Either.cond(event == AssassinationEvent, (), GameError(this, s"not $AssassinationEvent event")).right
      r <- Either.cond(assassin.role == Role.Assassin,
        if (target.role == Role.Commander) {
          this.copy(winner = Option(Affiliation.Spy))
            .addLog(GameSpyWinAssassination)

        } else {
          this.copy(winner = Option(Affiliation.Resistance))
            .addLog(GameResistanceWinAssassinationFailed)
        }, GameError(this, "not assassin can not do assassination. " + assassin.role)
      ).right
      rv <- r.eventFinish.right
    } yield rv
  }

  override def examination(inquisitor: Member, target: Member): Either[LogMessage, ResistanceGame] = {
    for {
      event <- eventQueue.getEvent.right
      _ <- Either.cond(event == InquisitorEvent, (), GameError(this, s"not $InquisitorEvent event")).right
      check <- allMembers.inquisitor.map(_ == inquisitor).toRight(GameError(this, "no inquisitor")).right
      _ <- Either.cond(check, (), GameError(this, s"$inquisitor is not a inquisitor.")).right
      isExamined <- allMembers.isExamined(target).toRight(GameError(this, "no inquisitor")).right
      _ <- Either.cond(isExamined, (), GameError(this, s"$target is already examined")).right
      members <- allMembers.passInquisitor(target).toRight(GameError(this, "no inquisitor")).right
    } yield this.copy(allMembers = members).addLog(GameInquisitorExamination(inquisitor, target, target.role.affiliation))
  }

  override def leader: Member = allMembers.leader

  override def log: Seq[LogMessage] = logQueue.queue

  def addLog(logMessage: LogMessage) = this.copy(logQueue = logQueue.add(logMessage))

  override def voteResultHistory: VoteRecord = {
    voteTrack.resultHistory
  }

  override def logFlush: ResistanceGame = this.copy(logQueue = LogQueueImpl())

  def eventFinish: Either[LogMessage, ResistanceGame] = {
    eventQueue.finishEvent().right.map(eventHook => this.copy(eventQueue = eventHook))
  }

}

object ResistanceGameImpl {
  def create(allMembers: AllMembers): ResistanceGame = {
    ResistanceGameImpl(allMembers,
      voteTrack = VoteTrackImpl(allMembers, Option(VoteRecordImpl(allMembers))),
      missionTrack = MissionTrackImpl(allMembers),
      eventQueue = EventTableImpl())
  }
}

case class EventTableImpl(eventQueue: Seq[GameEvent] = Seq.empty,
                          beforeMission: Map[MissionRound, Seq[GameEvent]] = Map.empty,
                          afterResistanceWin: Seq[GameEvent] = Seq.empty) extends EventTable {
  override def queueEmpty: Boolean = eventQueue.isEmpty

  override def getEvent: Either[LogMessage, GameEvent] = {
    eventQueue.headOption.toRight(GameError(this, "not run event handler phase"))
  }

  override def finishEvent(): Either[LogMessage, EventTable] = {
    eventQueue.headOption.map(_ => this.copy(eventQueue = eventQueue.tail)).toRight(GameError(this, "event Queue is empty."))
  }

  override def fireBeforeMission(round: MissionRound): EventTable =
    this.copy(eventQueue = eventQueue ++ beforeMission.getOrElse(round, Seq.empty))

  override def fireAfterResistanceWin: EventTable = this.copy(eventQueue = eventQueue ++ afterResistanceWin)
}

object AssassinationEvent extends GameEvent

object InquisitorEvent extends GameEvent


sealed abstract class GameInfo extends GameLog with InfoLog

case class GameVoteSubmitted(member: Member, voteToken: VoteToken) extends GameInfo

case class GameMissionSubmit(member: Member, missionCard: MissionCard) extends GameInfo

case class GameMissionClosed(success: Boolean) extends GameInfo

case class GameInquisitorExamination(inquisitor: Member, target: Member, affiliation: Affiliation) extends GameInfo

object GameSpyWin extends GameInfo

object GameSpyWinFiveVoteRejects extends GameInfo

object GameSpyWinAssassination extends GameInfo

object GameResistanceWin extends GameInfo

object GameResistanceWinAssassinationFailed extends GameInfo

object GameVoteApprove extends GameInfo

object GameVoteReject extends GameInfo

