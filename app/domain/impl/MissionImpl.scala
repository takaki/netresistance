package domain.impl

import domain._

case class MissionTrackImpl(allMembers: AllMembers,
                            mission: Option[Mission] = None,
                            missionHistory: MissionHistory = MissionHistoryImpl()) extends MissionTrack {
  override def open(teamMembers: TeamMembers): Either[MissionError, MissionTrack] = {
    for {
      _ <- Either.cond(mission.isEmpty, (), MissionError(this, "Mission is already open.")).right
      _ <- Either.cond(allMembers.teamMemberSize(round) == teamMembers.size, (),
        MissionError(this, "Team member size is incorrect.")).right
    } yield this.copy(mission = Option(MissionImpl(teamMembers, round)))
  }

  override def submit(member: Member, missionCard: MissionCard): Either[MissionError, MissionTrack] = {
    mission.toRight(MissionError(this, "Mission is not open")).right
      .flatMap(_.submit(member, missionCard)).right
      .map(mission => this.copy(mission = Option(mission)))
  }

  override def close(): Either[MissionLog, MissionTrack] = {
    mission.toRight(MissionError(this, "Mission is not open")).right
      .flatMap(_.result(allMembers)).right
      .map(result => this.copy(mission = None, missionHistory = missionHistory.add(result)))
  }

  override def gameWinner: Option[Affiliation] = {
    if (missionHistory.countSuccess >= 3) {
      Option(Affiliation.Resistance)
    } else if (missionHistory.countFail >= 3) {
      Option(Affiliation.Spy)
    } else {
      None
    }
  }

  override def round: MissionRound = missionHistory.round

  override def history: Seq[MissionResult] = {
    missionHistory.history
  }
}

case class MissionImpl(team: TeamMembers, round: MissionRound, action: Map[Member, MissionCard] = Map.empty) extends Mission {

  override def submit(member: Member, missionCard: MissionCard): Either[MissionError, Mission] = {
    for {
      _ <- Either.cond(missionCard.allow(member), (), MissionError(this, s"$member can not use $missionCard")).right
      _ <- Either.cond(team.isMember(member), (), MissionError(this, s"$member is not a team member.")).right
      _ <- Either.cond(!action.contains(member), (), MissionError(this, s"$member is already acted.")).right
    } yield this.copy(action = action.updated(member, missionCard))
  }

  override def result(allMembers: AllMembers): Either[MissionLog, MissionResult] = {
    Either.cond(team == TeamMembers(action.keySet),
      MissionResultImpl(action, MissionCriteriaImpl(round, allMembers)),
      MissionNotFinished)
  }
}

case class MissionHistoryImpl(history: Seq[MissionResult] = Seq.empty) extends MissionHistory {
  override def add(missionResult: MissionResult): MissionHistory = MissionHistoryImpl((missionResult +: history.reverse).reverse)

  override def round = MissionRound(history.size + 1)

  override def countSuccess: Int = history.count(_.isSuccess)

  override def countFail: Int = history.count(!_.isSuccess)

}

case class MissionResultImpl(action: Map[Member, MissionCard], criteria: MissionCriteria) extends MissionResult {
  override def isSuccess: Boolean = criteria.isSuccess(action.values.toSeq)

  override def teamMembers: TeamMembers = TeamMembers(action.keySet)
}

case class MissionRound(round: Int) extends IntValue {
  require(1 to 5 contains round)

  override def value: Int = round


}

case class MissionCriteriaImpl(round: MissionRound, allMembers: AllMembers) extends MissionCriteria {
  override def isSuccess(missionCards: Seq[MissionCard]): Boolean = {
    require(allMembers.teamMemberSize(round) == missionCards.size)
    missionCards.count(_ == MissionCard.Fail) < allMembers.requiredFails(round)
  }
}


sealed abstract class MissionInfo extends MissionLog with InfoLog

object MissionNotFinished extends MissionInfo
