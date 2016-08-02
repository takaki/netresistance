package domain

import domain.impl._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class VoteSpec extends Specification {
  val members = (0 to 4).map(i => MemberImpl(i.toString, role = if (i < 2) Role.Spy else Role.Resistance))
  "VoteTrackImpl with 7 players" should {
    val voteTrack = VoteTrackImpl(AllMembers(members))
    val teamMembers = TeamMembers(Set(members(0), members(1)))

    "before startTack can not submit" should {
      voteTrack.submit(members(0), VoteToken.Approve) must beLeft
    }

    val vt0 = voteTrack.startTrack
    "1st startTrack success" in {
      vt0 must beRight[VoteTrack]
      vt0.right.flatMap(_.round).right.map(_.value) must beRight(1)
    }
    "2nd startTrack fail" in {
      vt0.right.flatMap(_.startTrack) must beLeft
    }

    val vt1 = vt0.right.flatMap(_.openBallot(teamMembers))
    "1st startVote success" in {
      vt1 must beRight[VoteTrack]
    }
    "2nd startVote fail" in {
      vt1.right.flatMap(_.openBallot(teamMembers)) must beLeft
    }

    val vt2 = vt1.right.flatMap(_.submit(members(0), VoteToken.Approve))
    "1st submit success" in {
      vt2 must beRight[VoteTrack]
    }
    "2nd submit fail" in {
      vt2.right.flatMap(_.submit(members(0), VoteToken.Approve)) must beLeft
    }

    "can not get result from open voting" in {
      vt2.right.flatMap(_.closeBallot) must beLeft
      vt2.right.flatMap(_.result) must beLeft
      vt2.right.flatMap(_.spyWin) must beRight(false)
    }

    val vt3 = vt2.right.flatMap(_.submit(members(1), VoteToken.Approve))
      .right.flatMap(_.submit(members(2), VoteToken.Approve))
      .right.flatMap(_.submit(members(3), VoteToken.Approve))
      .right.flatMap(_.submit(members(4), VoteToken.Approve))

    val vt4 = vt3.right.flatMap(_.closeBallot)
    "can end full ballot" in {
      vt4 must beRight
      vt4.right.flatMap(_.result).right.map(_.majority) must beRight(VoteMajority.Approved)
      vt4.right.flatMap(_.spyWin) must beRight(false)
    }

    "can get info twice" in {
      vt4.right.flatMap(_.result).right.map(_.majority) must beRight(VoteMajority.Approved)
      vt4.right.flatMap(_.result).right.map(_.teamMembers) must beRight(teamMembers)
      vt4.right.flatMap(_.spyWin) must beRight(false)
    }

    val vt5 = vt4.right.flatMap(_.endTrack)
    "endTrack" in {
      vt5 must beRight
      vt4.right.flatMap(_.round).right.map(_.value) must beRight(2)
      vt5.right.flatMap(_.result) must beLeft
      vt5.right.flatMap(_.spyWin) must beLeft
    }
    "not entTrack twice" in {
      vt5.right.flatMap(_.endTrack) must beLeft
    }

    val vt20 = vt5.right.flatMap(_.startTrack)
    "start 2nd round" in {
      vt20 must beRight
      vt20.right.flatMap(_.round).right.map(_.value) must beRight(1)
    }

    val vt21 = vt20.right.flatMap(_.openBallot(teamMembers))
    val vt22 = vt21.right.flatMap(_.submit(members(0), VoteToken.Approve))
      .right.flatMap(_.submit(members(1), VoteToken.Approve))
      .right.flatMap(_.submit(members(2), VoteToken.Reject))
      .right.flatMap(_.submit(members(3), VoteToken.Reject))
      .right.flatMap(_.submit(members(4), VoteToken.Reject))
      .right.flatMap(_.closeBallot)

    "vote result is reject" in {
      vt22.right.flatMap(_.result).right.map(_.majority) must beRight(VoteMajority.Rejected)
      vt22.right.flatMap(_.result).right.map(_.teamMembers) must beRight(teamMembers)
      vt22.right.flatMap(_.spyWin) must beRight(false)
    }

    val vt23 = vt22.right.flatMap(_.openBallot(teamMembers))
      .right.flatMap(_.submit(members(0), VoteToken.Approve))
      .right.flatMap(_.submit(members(1), VoteToken.Approve))
      .right.flatMap(_.submit(members(2), VoteToken.Approve))
      .right.flatMap(_.submit(members(3), VoteToken.Reject))
      .right.flatMap(_.submit(members(4), VoteToken.Reject))
      .right.flatMap(_.closeBallot)

    "vote result is reject" in {
      vt23 must beRight[VoteTrack]
      vt23.right.flatMap(_.result).right.map(_.majority) must beRight(VoteMajority.Approved)
      vt23.right.flatMap(_.result).right.map(_.teamMembers) must beRight(teamMembers)
      vt23.right.flatMap(_.spyWin) must beRight(false)
    }

    "spy win by 5 rejects" in {
      val vt30 = vt23.right.flatMap(_.endTrack).right.flatMap(_.startTrack)
      vt30 must beRight[VoteTrack]
      val f0 = (voteTrack: Either[VoteLog, VoteTrack], members: Seq[Member]) =>
        members.foldLeft(voteTrack.right.flatMap(_.openBallot(teamMembers)))((vt, m) =>
          vt.right.flatMap(_.submit(m, VoteToken.Reject))).right.flatMap(_.closeBallot)

      val vt32 = (1 to 5).foldLeft(vt30)((vt, _) => f0(vt, members))
      vt32 must beRight[VoteTrack]
      vt32.right.flatMap(_.result).right.map(_.majority) must beRight(VoteMajority.Rejected)
      vt32.right.flatMap(_.result).right.map(_.teamMembers) must beRight(teamMembers)
      vt32.right.flatMap(_.spyWin) must beRight(true)
    }

  }

  "VoteRecordImpl" should {
    val allMembers = AllMembers(members)
    val teamMembers = TeamMembers(Set(members(0), members(1)))
    val voteRecord = VoteRecordImpl(allMembers = allMembers)

    val vr0 = voteRecord.openBallot(teamMembers)

    "can submit" in {
      vr0 must beRight[VoteRecord]
      vr0.right.flatMap(_.submit(members(0), VoteToken.Approve)) must beRight[VoteRecord]
    }

    "can not submit twice" in {
      vr0.right.flatMap(_.submit(members(0), VoteToken.Approve)).right.flatMap(_.submit(members(0), VoteToken.Approve)) must beLeft
    }
  }

  "BallotImpl" should {
    val teamMembers = TeamMembers(Set(members(0), members(1)))
    val allMembers = AllMembers(members)

    val ballot = BallotImpl(teamMembers, allMembers)

    "can submit" in {
      ballot.submit(members(0), VoteToken.Approve) must beRight[Ballot]
    }

    "can not submit twice" in {
      ballot.submit(members(0), VoteToken.Approve).right.flatMap(_.submit(members(0), VoteToken.Approve)) must beLeft
    }

    "can not get result before full" in {
      ballot.submit(members(0), VoteToken.Approve).right.flatMap(_.result) must beLeft
    }
    "get result" in {
      val result = ballot.submit(members(0), VoteToken.Approve)
        .right.flatMap(_.submit(members(1), VoteToken.Approve))
        .right.flatMap(_.submit(members(2), VoteToken.Approve))
        .right.flatMap(_.submit(members(3), VoteToken.Approve))
        .right.flatMap(_.submit(members(4), VoteToken.Approve))
        .right.flatMap(_.result)
      result.right.map(_.majority) must beRight(VoteMajority.Approved)
      result.right.map(_.teamMembers) must beRight(teamMembers)

    }
  }

}