package grade_horaria.basico

import scala.collection.immutable.SortedSet

import grade_horaria._
import ag.{Aptidão, População => AgPop}

class População(
  prob:Problema,
  fapt:Aptidão[Cromossomo],
  val indivíduos : SortedSet[Indivíduo]
) extends AgPop[Cromossomo, Indivíduo, População] {

  def replica(inds: SortedSet[Indivíduo]):População =
    new População(prob, fapt, inds)

}

object População {
  def apply( prob:Problema, fapt:Aptidão[Cromossomo],tamPop:Int) = {
    val indivíduos = SortedSet[Indivíduo]() ++
      (for {i <- 1 to tamPop} yield Indivíduo(prob, fapt))
    new População(prob, fapt, indivíduos)
  }
}

