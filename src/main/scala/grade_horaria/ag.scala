package grade_horaria
import grade_horaria._

import ag.{AG, Indivíduo, População}

abstract class AGBase[Ind <: Indivíduo[Cromossomo, Ind],
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
  val aulasAg   = new AulasAgendadas(prob)
  val membConf  = new MembrosConflitantes(prob)
  val salasConf = new SalasConflitantes(prob)
  val indsp     = new IndisponibilidadesProf(prob)
  val capSala   = new CapacidadeSalas(prob)
  val minD      = new MínimoDias(prob)
  val aIso      = new AulasIsoladas(prob)
  val eSala     = new EstabilidadeSala(prob)

  fapt.addRestriçãoInviolável(aulasAg)
  fapt.addRestriçãoInviolável(membConf)
  fapt.addRestriçãoInviolável(salasConf)
  fapt.addRestriçãoInviolável(indsp)
  fapt.addRestriçãoPreferencial(capSala)
  fapt.addRestriçãoPreferencial(minD)
  fapt.addRestriçãoPreferencial(aIso)
  fapt.addRestriçãoPreferencial(eSala)



}
