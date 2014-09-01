package grade_horaria.basico

import ag.{Aptidão, Indivíduo => AgInd}
import grade_horaria.{Cromossomo, Problema, Ctt}

class Indivíduo(
  val pr:Problema,
  fapt:Aptidão[Cromossomo],
  val cromossomo: Cromossomo
) extends AgInd[Cromossomo, Indivíduo](fapt)  {

  val apt = fapt.avalia(cromossomo)
  //val penalidades = fapt.penalidades.clone
  //val vInvioláveis = fapt.vInvioláveis.clone
  //val vPreferenciais = fapt.vPreferenciais.clone

  def replica(c:Cromossomo) = new Indivíduo(pr,fapt,c)
}

object Indivíduo {
  def apply(p:Problema, a:Aptidão[Cromossomo]) = {
    val c = Cromossomo(p.nEventos)(p.nSalas, p.nHorários)
    new Indivíduo(p, a, c)
  }
}
