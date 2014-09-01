package grade_horaria.alocador

import scala.collection.mutable.{Map => MMap}

import ag.{Aptidão, Indivíduo => AgInd}

import grade_horaria._
import grade_horaria.{Cromossomo, Problema}

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
  
  def apply(pr:Problema, a:Aptidão[Cromossomo], rep: Reparação) =  {
    var c = Cromossomo(pr.nEventos)(pr.nSalas, pr.nHorários)
    for (t <- 0 until pr.nHorários)
      if (c.eventosSimultâneos contains t)
         c = rep.realocaSalas(c, t)
    new Indivíduo(pr, a, c)
  }
}

