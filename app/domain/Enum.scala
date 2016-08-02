package domain

trait IntValue {
  def value: Int
}

sealed trait Affiliation

object Affiliation {

  case object Resistance extends Affiliation

  case object Spy extends Affiliation

}

sealed trait MissionCard {
  def allow(member: Member): Boolean
}

object MissionCard {

  case object Success extends MissionCard {
    override def allow(member: Member): Boolean = true
  }

  case object Fail extends MissionCard {
    override def allow(member: Member): Boolean = member.role.affiliation == Affiliation.Spy
  }

}

sealed trait VoteToken

object VoteToken {

  case object Approve extends VoteToken

  case object Reject extends VoteToken

}

sealed trait Role {
  def affiliation: Affiliation
}

sealed trait ResistanceRole extends Role {
  def affiliation = Affiliation.Resistance
}

sealed trait SpyRole extends Role {
  def affiliation = Affiliation.Spy
}


object Role {

  case object Resistance extends ResistanceRole

  case object Spy extends SpyRole

  case object Commander extends ResistanceRole

  case object Assassin extends SpyRole

  case object BodyGuard extends ResistanceRole

  case object DeepCover extends SpyRole

  case object BlindSpy extends SpyRole

  case object FalseCommander extends SpyRole

}
