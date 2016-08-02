package domain

import domain.impl.TeamMembers

trait GameRoom {
  def assignTeam(leader: Member, members: TeamMembers): GameRoom

  def submitVoteToken(member: Member, voteToken: VoteToken): GameRoom

  def submitMissionCard(member: Member, missionCard: MissionCard): GameRoom

  def sendBroadcast(message: String)

  def sendMessage(member: Member, message: String)

}
