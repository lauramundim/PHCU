package grade_horaria

import grade_horaria._
import ag.Restrição

//Restrição Preferencial das instâncias Tim.

class ÚltimoHorário(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número de alunos que têm aula no
  //     último horário

  val nHD = prob.periodosPorDia

  def penalizaEvento(e:Int, cromo:Cromossomo) = { 
    var eCRP = 0
    var t = cromo(e).horário		
    //alunos nao devem ter aula no ultimo horário
    if(t%nHD == nHD-1)
      eCRP += prob.estudanteNum(e)
    if(debug)
      println("Violações de último horário do evento " + e + ": " + eCRP)
    eCRP
  } 

  //restrições preferenciais não precisam do afetado
  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    0
  }

  def violadas(c:Cromossomo):Int = { 
    var v_ult = 0
    // alunos não devem ter aula no utimo horario do dia
    //uma penalidade para cada estudante que tenha aula 
    //no último horário
    for(i <-0 until prob.nEventos){
      if((c(i).horário) % nHD == nHD-1)
        v_ult += prob.estudanteNum(i)
    }

    if(debug)
      println("Violações de  último horário: " + v_ult)
    
    v_ult
  }

/*
def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {
    cromo.indices foreach { e => 
      var eCRP = 0
      var t = cromo(e).horário		
      //alunos nao devem ter aula no ultimo horário
      if(t%nHD == nHD-1)
        eCRP += prob.estudanteNum(e)
      penalidades(e) += eCRP
    }
  } 
*/
}
