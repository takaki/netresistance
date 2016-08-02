package domain.impl

import domain._

case class VoteTrackImpl(allMembers: AllMembers,
                         voteRecord: Option[VoteRecord] = None,
                         voteHistory: VoteHistory = VoteHistoryImpl()
                        ) extends VoteTrack {

  override def startTrack: Either[VoteError, VoteTrack] = {
    Either.cond(voteRecord.isEmpty,
      this.copy(voteRecord = Option(VoteRecordImpl(allMembers = allMembers))),
      VoteError(this, "Vote track is already started."))
  }

  override def openBallot(teamMembers: TeamMembers): Either[VoteLog, VoteTrack] = {
    voteRecord.toRight(VoteNotTeamVote).right
      .flatMap(_.openBallot(teamMembers)).right
      .map(voteRecord => this.copy(voteRecord = Some(voteRecord)))
  }

  override def submit(member: Member, voteToken: VoteToken): Either[VoteLog, VoteTrack] = {
    voteRecord.toRight(VoteNotTeamVote).right
      .flatMap(_.submit(member, voteToken)).right
      .map(voteRecord => this.copy(voteRecord = Option(voteRecord)))
  }

  override def closeBallot: Either[VoteLog, VoteTrack] = {
    voteRecord.toRight(VoteNotTeamVote).right
      .flatMap(_.closeBallot).right
      .map(voteRecord => this.copy(voteRecord = Some(voteRecord)))
  }

  override def endTrack: Either[VoteLog, VoteTrack] = {
    voteRecord.toRight(VoteNotTeamVote).right
      .map(voteRecord => this.copy(voteRecord = None, voteHistory = voteHistory.add(voteRecord)))
  }

  override def round: Either[VoteLog, VoteRound] = {
    voteRecord.toRight(VoteNotTeamVote).right.map(_.round)
  }

  override def result: Either[VoteLog, VoteResult] = {
    voteRecord.toRight(VoteNotTeamVote).right.flatMap(_.result)
  }

  override def spyWin: Either[VoteLog, Boolean] = {
    voteRecord.toRight(VoteNotTeamVote).right.map(_.spyWin)
  }

}

case class VoteRecordImpl(allMembers: AllMembers,
                          ballot: Option[Ballot] = None,
                          results: Seq[VoteResult] = Seq.empty) extends VoteRecord {
  require(results.size <= 5)

  override def openBallot(teamMembers: TeamMembers): Either[VoteError, VoteRecord] = {
    Either.cond(ballot.isEmpty,
      this.copy(ballot = Some(BallotImpl(allMembers = allMembers, teamMembers = teamMembers))),
      VoteError(this, "Ballot is already open."))
  }

  override def submit(member: Member, voteToken: VoteToken): Either[VoteLog, VoteRecord] = {
    ballot.toRight(VoteBallotIsClosed).right
      .flatMap(_.submit(member, voteToken)).right
      .map(ballot => this.copy(ballot = Option(ballot)))
  }

  override def closeBallot: Either[VoteLog, VoteRecord] = {
    ballot.toRight(VoteBallotIsClosed).right
      .flatMap(_.result).right
      .map(voteResult => this.copy(ballot = None, results = (voteResult +: results.reverse).reverse))
  }

  override def round: VoteRound = VoteRound(results.size + 1)

  override def result: Either[VoteError, VoteResult] = {
    for {
      _ <- Either.cond(ballot.isEmpty, (), VoteError(this, "Vote is still open.")).right
      result <- results.lastOption.toRight(VoteError(this, "No records.")).right
    } yield result
  }

  override def spyWin: Boolean = results.size >= 5

}

case class BallotImpl(teamMembers: TeamMembers,
                      allMembers: AllMembers,
                      ballot: Map[Member, VoteToken] = Map.empty) extends Ballot {
  override def submit(member: Member, voteToken: VoteToken): Either[VoteError, Ballot] = {
    for {
      _ <- Either.cond(allMembers.isMember(member), (), VoteError(this, s"$member is not a member.")).right
      _ <- Either.cond(!ballot.contains(member), (), VoteError(this, s"$member already voted")).right
    } yield this.copy(ballot = ballot.updated(member, voteToken))
  }

  override def result: Either[VoteLog, VoteResult] = {
    Either.cond(allMembers.same(ballot.keySet),
      VoteResultImpl(teamMembers, ballot),
      VoteNotFinished)
  }
}

case class VoteHistoryImpl(history: Seq[VoteRecord] = Seq.empty) extends VoteHistory {
  require(history.size <= 5)

  override def add(voteRecord: VoteRecord): VoteHistory = VoteHistoryImpl((voteRecord +: history).reverse)
}

case class VoteResultImpl(teamMembers: TeamMembers, result: Map[Member, VoteToken]) extends VoteResult {
  override def majority: VoteMajority = {
    if (result.values.count(_ == VoteToken.Approve) > result.values.count(_ == VoteToken.Reject)) {
      VoteMajority.Approved
    } else {
      VoteMajority.Rejected
    }
  }

}


case class VoteRound(round: Int) extends IntValue {
  require(1 to 5 contains round)

  override def value: Int = round

}

sealed abstract class VoteInfo extends VoteLog with InfoLog

object VoteNotTeamVote extends VoteInfo

object VoteBallotIsClosed extends VoteInfo

object VoteNotFinished extends VoteInfo
