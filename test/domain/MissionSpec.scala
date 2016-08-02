package domain

import domain.impl._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MissionSpec extends Specification {
  val members = (0 to 6).map(i => MemberImpl(i.toString, role = if (i < 3) Role.Spy else Role.Resistance))

  "MissionTrackImpl" should {
    val tm1 = TeamMembers(members.toList.slice(0, 2).toSet)
    val tm2 = TeamMembers(members.toList.slice(0, 3).toSet)
    val tm3 = TeamMembers(members.toList.slice(0, 3).toSet)
    val tm4 = TeamMembers(members.toList.slice(0, 4).toSet)
    val tm5 = TeamMembers(members.toList.slice(0, 4).toSet)
    val allMembers = AllMembers.assignRole(members, List.fill(4)(Role.Resistance) ++ List.fill(3)(Role.Spy))

    val mt00 = MissionTrackImpl(allMembers)
    "MissionTrack can open" in {
      mt00.open(tm1) must beRight[MissionTrack]
    }
    "MissionTrack can not open twice" in {
      mt00.open(tm1).right.flatMap(_.open(tm1)) must beLeft
    }
    val mt10 = mt00.open(tm1)

    "team member can submit" in {
      mt10.right.flatMap(_.submit(members(0), MissionCard.Success)) must beRight
      mt10.right.flatMap(_.submit(members(0), MissionCard.Fail)) must beRight
    }

    "non team member can not submit" in {
      mt10.right.flatMap(_.submit(members(6), MissionCard.Success)) must beLeft
    }

    "can not end incomplete mission" in {
      mt10.right.flatMap(_.submit(members(0), MissionCard.Success)).right.flatMap(_.close) must beLeft
    }
    "can not submit twice" in {
      mt10.right.flatMap(_.submit(members(0), MissionCard.Success))
        .right.flatMap(_.submit(members(0), MissionCard.Success)) must beLeft
    }

    "can end complete mission" in {
      mt10.right.flatMap(_.submit(members(0), MissionCard.Success))
        .right.flatMap(_.submit(members(1), MissionCard.Success))
        .right.flatMap(_.close) must beRight
    }

    val mt11 = mt10.right.flatMap(_.submit(members(0), MissionCard.Success))
      .right.flatMap(_.submit(members(1), MissionCard.Success))
      .right.flatMap(_.close)
    "1st mission is success" in {
      mt11.right.flatMap(_.history.lastOption.toRight("error")).right.map(_.isSuccess) must beRight(true)
    }

    val mt20 = mt11.right.flatMap(_.open(tm2))
      .right.flatMap(_.submit(members(0), MissionCard.Success))
      .right.flatMap(_.submit(members(1), MissionCard.Success))
      .right.flatMap(_.submit(members(2), MissionCard.Fail))
      .right.flatMap(_.close)
    "2nd mission is fail" in {
      mt20.right.flatMap(_.history.lastOption.toRight("error")).right.map(_.isSuccess) must beRight(false)
    }

    val mt30 = mt20.right.flatMap(_.open(tm3))
      .right.flatMap(_.submit(members(0), MissionCard.Fail))
      .right.flatMap(_.submit(members(1), MissionCard.Fail))
      .right.flatMap(_.submit(members(2), MissionCard.Fail))
      .right.flatMap(_.close)
    "3rd mission is fail" in {
      mt30.right.flatMap(_.history.lastOption.toRight("error")).right.map(_.isSuccess) must beRight(false)
    }

    "4th mission requires two fails to fail" in {
      mt30.right.flatMap(_.open(tm4))
        .right.flatMap(_.submit(members(0), MissionCard.Fail))
        .right.flatMap(_.submit(members(1), MissionCard.Fail))
        .right.flatMap(_.submit(members(2), MissionCard.Success))
        .right.flatMap(_.submit(members(3), MissionCard.Success))
        .right.flatMap(_.close)
        .right.flatMap(_.history.lastOption.toRight("error")).right.map(_.isSuccess) must beRight(false)

    }

    val mt40 = mt30.right.flatMap(_.open(tm4))
      .right.flatMap(_.submit(members(0), MissionCard.Fail))
      .right.flatMap(_.submit(members(1), MissionCard.Success))
      .right.flatMap(_.submit(members(2), MissionCard.Success))
      .right.flatMap(_.submit(members(3), MissionCard.Success))
      .right.flatMap(_.close)
    "4th mission is success with one fail" in {
      mt40.right.flatMap(_.history.lastOption.toRight("error")).right.map(_.isSuccess) must beRight(true)
      mt40.right.map(_.gameWinner) must beRight(None)
    }

    "Registance win" in {
      mt40.right.flatMap(_.open(tm5))
        .right.flatMap(_.submit(members(0), MissionCard.Success))
        .right.flatMap(_.submit(members(1), MissionCard.Success))
        .right.flatMap(_.submit(members(2), MissionCard.Success))
        .right.flatMap(_.submit(members(3), MissionCard.Success))
        .right.flatMap(_.close)
        .right.map(_.gameWinner) must beRight(Some(Affiliation.Resistance))
    }

    "Spy win" in {
      mt40.right.flatMap(_.open(tm5))
        .right.flatMap(_.submit(members(0), MissionCard.Success))
        .right.flatMap(_.submit(members(1), MissionCard.Success))
        .right.flatMap(_.submit(members(2), MissionCard.Fail))
        .right.flatMap(_.submit(members(3), MissionCard.Success))
        .right.flatMap(_.close)
        .right.map(_.gameWinner) must beRight(Some(Affiliation.Spy))
    }
  }

  "MissionCriteriaImpl" should {
    val members = (1 to 7).map(i => MemberImpl(i.toString))
    val allMembers = AllMembers.assignRole(members, List.fill(4)(Role.Resistance) ++ List.fill(3)(Role.Spy))

    "round = 1 and fails = 0 is success" in {
      val mc = MissionCriteriaImpl(MissionRound(1), allMembers)
      mc.isSuccess(Seq(MissionCard.Success, MissionCard.Success)) must beTrue
    }
  }

  "MissionRound" should {
    val allMembers = AllMembers.assignRole(members, List.fill(4)(Role.Resistance) ++ List.fill(3)(Role.Spy))
    val mr = MissionRound(1)
    "requireFails returns 1" in {
      allMembers.requiredFails(mr) === 1
    }
    "requireFails returns 2" in {
      allMembers.requiredFails(MissionRound(4)) === 2
    }

  }
}
