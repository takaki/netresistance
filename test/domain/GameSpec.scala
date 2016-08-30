package domain

import domain.impl._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameSpec extends Specification {
  val members = Set(MemberImpl("0", Role.Assassin),
    MemberImpl("1", Role.Spy),
    MemberImpl("2", Role.Spy),
    MemberImpl("3", Role.Resistance),
    MemberImpl("4", Role.Resistance),
    MemberImpl("5", Role.Resistance),
    MemberImpl("6", Role.Commander))
  val tm1 = TeamMembers(members.toList.slice(0, 2).toSet)
  val tm2 = TeamMembers(members.toList.slice(1, 4).toSet)
  val tm3 = TeamMembers(members.toList.slice(2, 5).toSet)
  val tm4 = TeamMembers(members.toList.slice(0, 4).toSet)
  val tm5 = TeamMembers(members.toList.slice(0, 4).toSet)
  val allMembers = AllMembers(members)

  "ResistanceGameImpl" should {
    val game = ResistanceGameImpl(allMembers,
      voteTrack = VoteTrackImpl(allMembers, Option(VoteRecordImpl(allMembers))),
      missionTrack = MissionTrackImpl(allMembers),
      eventTable = EventTableImpl(afterResistanceWin = Seq(AssassinationEvent)))



    "leader is members(0)" in {
      game.leader === members(0)
    }
    "can not submit vote" in {
      game.submitVoteToken(members(0), VoteToken.Approve).right.map(_.log) mustNotEqual beRight(Seq.empty)
    }

    "assign team member" in {
      val leader = game.leader
      game.assignTeam(leader, tm1) must beRight
    }
    "not leader can not assign team" in {
      game.assignTeam(members(1), tm1) must beLeft
    }
    "spy win by 5 times reject" in {
      val f = {
        (g: Either[LogMessage, ResistanceGame]) =>
          (0 to 6).foldLeft(g)((g, i) => g.right.flatMap(_.submitVoteToken(members(i), VoteToken.Reject)))
      }
      val rv = for {
        g2 <- f(game.assignTeam(members(0), tm1)).right
        g3 <- f(g2.assignTeam(members(1), tm2)).right
        g4 <- f(g3.assignTeam(members(2), tm3)).right
        g5 <- f(g4.assignTeam(members(3), tm4)).right
        gl <- f(g5.logFlush.assignTeam(members(4), tm4)).right
      } yield gl
      rv.right.map(_.winner) must beRight(Option(Affiliation.Spy))
      rv.right.map(_.log) must beRight(contain(GameSpyWinFiveVoteRejects: LogMessage))
    }

    val vote11 = game.assignTeam(members(0), tm1)
      .right.flatMap(_.submitVoteToken(members(0), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(1), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(2), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(3), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(4), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(5), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(6), VoteToken.Reject))
    "team vote (1-1) is rejected" in {
      vote11 must beRight
      vote11.right.flatMap(_.log.lastOption.toRight("Error")) must beRight(GameVoteReject)
    }

    "leader is members(1)" in {
      vote11.right.map(_.leader) must beRight(members(1))
    }
    val vote12 = vote11.right.flatMap(_.assignTeam(members(1), tm1))
      .right.flatMap(_.submitVoteToken(members(0), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(1), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(2), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(3), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(4), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(5), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(6), VoteToken.Reject))

    "team vote (1-2) is approved" in {
      vote12 must beRight
      vote12.right.flatMap(_.log.lastOption.toRight("Error")) must beRight(GameVoteApprove)
    }

    "can not vote closed team vote" in {
      vote12.right.flatMap(_.submitVoteToken(members(0), VoteToken.Approve))
        .right.flatMap(_.log.lastOption.toRight("Error")) must beRight(VoteNotTeamVote)
    }

    val ms1 = vote12.right.map(_.logFlush)
      .right.flatMap(_.submitMissionCard(members(0), MissionCard.Success))
      .right.flatMap(_.submitMissionCard(members(1), MissionCard.Success))
    "1st mission success" in {
      ms1 must beRight
      ms1.right.map(_.log) must beRight(contain(GameMissionClosed(true): LogMessage))
    }

    "leader is members(2)" in {
      ms1.right.map(_.leader) must beRight(members(2))
    }
    val vote21 = ms1.right.flatMap(_.assignTeam(members(2), tm2))
      .right.flatMap(_.submitVoteToken(members(0), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(1), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(2), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(3), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(4), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(5), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(6), VoteToken.Reject))
    "team vote (2-1) is approved" in {
      vote21.right.map(_.log.last) must beRight(GameVoteApprove)
    }

    val ms2 = vote21.right.map(_.logFlush)
      .right.flatMap(_.submitMissionCard(members(3), MissionCard.Success))
      .right.flatMap(_.submitMissionCard(members(2), MissionCard.Success))
      .right.flatMap(_.submitMissionCard(members(1), MissionCard.Fail))
    "2nd mission fail" in {
      ms2 must beRight
      ms2.right.map(_.log) must beRight(contain(GameMissionClosed(false): LogMessage))
    }

    "leader is member(3)" in {
      ms2.right.map(_.leader) must beRight(members(3))
    }

    val vote31 = ms2.right.map(_.logFlush)
      .right.flatMap(_.assignTeam(members(3), tm3))
      .right.flatMap(_.submitVoteToken(members(0), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(1), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(2), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(3), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(4), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(5), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(6), VoteToken.Reject))
    "team vote (3-1) is approved" in {
      vote31 must beRight
      vote31.right.map(_.log.last) must beRight(GameVoteApprove)
    }

    val ms3 = vote31.right.map(_.logFlush)
      .right.flatMap(_.submitMissionCard(members(2), MissionCard.Fail))
      .right.flatMap(_.submitMissionCard(members(3), MissionCard.Success))
      .right.flatMap(_.submitMissionCard(members(4), MissionCard.Success))
    "3rd mission fail" in {
      ms3 must beRight
      ms3.right.map(_.log) must beRight(contain(GameMissionClosed(false): LogMessage))
    }

    val vote41 = ms3.right.map(_.logFlush)
      .right.flatMap(_.assignTeam(members(4), tm4))
      .right.flatMap(_.submitVoteToken(members(0), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(1), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(2), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(3), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(4), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(5), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(6), VoteToken.Reject))
    "team vote (4-1) is approved" in {
      vote41 must beRight
      vote41.right.map(_.log.last) must beRight(GameVoteApprove)
    }

    val ms4 = vote41.right.map(_.logFlush)
      .right.flatMap(_.submitMissionCard(members(0), MissionCard.Fail))
      .right.flatMap(_.submitMissionCard(members(1), MissionCard.Success))
      .right.flatMap(_.submitMissionCard(members(2), MissionCard.Success))
      .right.flatMap(_.submitMissionCard(members(3), MissionCard.Success))
    "4th mission success(1 fail)" in {
      ms4 must beRight
      ms4.right.map(_.log) must beRight(contain(GameMissionClosed(true): LogMessage))
    }

    val vote51 = ms4.right.map(_.logFlush)
      .right.flatMap(_.assignTeam(members(5), tm5))
      .right.flatMap(_.submitVoteToken(members(0), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(1), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(2), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(3), VoteToken.Approve))
      .right.flatMap(_.submitVoteToken(members(4), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(5), VoteToken.Reject))
      .right.flatMap(_.submitVoteToken(members(6), VoteToken.Reject))
    "team vote (5-1) is approved" in {
      vote51 must beRight
      vote51.right.map(_.log.last) must beRight(GameVoteApprove)
    }

    "5th mission fail make spy win" in {
      val ms5 = vote51.right.map(_.logFlush)
        .right.flatMap(_.submitMissionCard(members(0), MissionCard.Fail))
        .right.flatMap(_.submitMissionCard(members(1), MissionCard.Success))
        .right.flatMap(_.submitMissionCard(members(2), MissionCard.Success))
        .right.flatMap(_.submitMissionCard(members(3), MissionCard.Success))
      ms5 must beRight
      ms5.right.map(_.winner) must beRight(Option(Affiliation.Spy))
    }

    val ms5 = vote51.right.map(_.logFlush)
      .right.flatMap(_.submitMissionCard(members(0), MissionCard.Success))
      .right.flatMap(_.submitMissionCard(members(1), MissionCard.Success))
      .right.flatMap(_.submitMissionCard(members(2), MissionCard.Success))
      .right.flatMap(_.submitMissionCard(members(3), MissionCard.Success))
    "5th mission success" in {
      ms5 must beRight
      ms5.right.map(_.log) must beRight(contain(GameMissionClosed(true): LogMessage))
    }

    "game is not finished" in {
      ms5.right.map(_.winner) must beRight(None)
    }
    "assassination success" in {
      val arw = ms5.right.map(_.logFlush).right.flatMap(_.assassination(members(0), members(6)))
      arw must beRight
      arw.right.map(_.winner) must beRight(Option(Affiliation.Spy))
    }
    "assassination fail" in {
      val arw = ms5.right.map(_.logFlush).right.flatMap(_.assassination(members(0), members(5)))
      arw must beRight
      arw.right.map(_.winner) must beRight(Option(Affiliation.Resistance))
    }
    "not assassin can not do assassination" in {
      val arw = ms5.right.map(_.logFlush).right.flatMap(_.assassination(members(1), members(6)))
      arw must beLeft
    }

  }

  "LeaderMarker" should {
    "7 members" in {
      val leaderMarker: LeaderMarker = LeaderMarkerImpl()
      leaderMarker.leader(members) === members(0)
      leaderMarker.rotate.rotate.leader(members) === members(2)
      (1 to 7).foldLeft(leaderMarker)((lm, _) => lm.rotate).leader(members) === members(0)
    }
  }
}







