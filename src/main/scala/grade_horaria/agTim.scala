package grade_horaria
import grade_horaria._

import ag.{AG, Indivíduo, População}

abstract class AGBaseTim[Ind <: Indivíduo[Cromossomo, Ind],
                      P <: População[Cromossomo,Ind,P]](
  par:Parâmetros,
  arqProb:String
) extends AG[Cromossomo, Ind, P] {

  par.semente match {
    case None => ()
    case Some(seed) => rng.setSeed(seed)
  }

  val prob      = Problema(arqProb)
  val fapt      = new Aptidão(prob)

  val vInv       = new ViolacõesInv(prob)
  val aulasCons  = new AulasConsecutivas(prob)
  val unicaAula  = new ÚnicaAula(prob)
  val ultHor     = new ÚltimoHorário(prob)

  fapt.addRestriçãoInviolável(vInv)
  fapt.addRestriçãoPreferencial(aulasCons)
  fapt.addRestriçãoPreferencial(unicaAula)
  fapt.addRestriçãoPreferencial(ultHor)
}
