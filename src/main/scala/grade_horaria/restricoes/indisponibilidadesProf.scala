package grade_horaria
import grade_horaria._

import ag.Restrição

//Restrição Inviolável das instâncias XML.

class IndisponibilidadesProf(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número de eventos alocados em horários
  //    em que o professor do evento não está disponível

  def penalizaEvento(e:Int, cromo:Cromossomo) = {
    var continds = 0
    var t = cromo(e).horário
    for(k <- 0 until prob.nRestricoes){
      if(prob.listaRestricoes(k).tipo == "period"){
        if(prob.listaRestricoes(k).curso == prob.eventos(e)){
          for(l <- 0 until prob.listaRestricoes(k).timeslots.length){
            var d = prob.listaRestricoes(k).timeslots(l).dia
            var pe = prob.listaRestricoes(k).timeslots(l).periodo
            if(t == prob.diasHorários(d)(pe))
              continds = continds + 1  
          }
        }
      }
    } 
    if(debug)
      println("Violações de indisponibilidade de professor do evento " + e + ": " + continds)
    continds
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    var continds = 0 //zera o contador dos eventos afetados
    var t = cromo(e).horário // t eh o horario do evento analisado
    val es = cromo.eventosSimultâneos
    for(i <- 0 until es(t).length){
      //acrescentando pq a disciplina nao pode ficar nesse horario
      for(k <- 0 until prob.nRestricoes){
        //se for restricao de periodo
        if(prob.listaRestricoes(k).tipo == "period"){ 
          //e for do professor do evento
          if(prob.listaRestricoes(k).curso == prob.eventos(es(t)(i))){
            for(l <- 0 until prob.listaRestricoes(k).timeslots.length){
              var d = prob.listaRestricoes(k).timeslots(l).dia
              var pe = prob.listaRestricoes(k).timeslots(l).periodo
              if(t == prob.diasHorários(d)(pe))
                continds += 1
            }
          }
        }
      }
    }
    continds
  }

  def violadas(c:Cromossomo):Int = {
    var v_disponibilidades = 0
    for(i <- 0 until prob.nEventos){		
    //percorre a lista de rstricoes
      for(k <- 0 until prob.nRestricoes){
        //se for restricoes do tipo periodo
        if(prob.listaRestricoes(k).tipo == "period"){
        //percorre a lista de timeslot da estricao
          for(l<- 0 until prob.listaRestricoes(k).timeslots.length){
            var d = prob.listaRestricoes(k).timeslots(l).dia
            var pe = prob.listaRestricoes(k).timeslots(l).periodo
            //se o evento eh o evento da restricao e esta no dia 
            //e horario q nao pode, entao conta-se uma violacao
            if(prob.listaRestricoes(k).curso == prob.eventos(i) && 
               c(i).horário == prob.diasHorários(d)(pe)){
               /*println("Course " + prob.eventos(i) + 
                       " has a lecture at unavilable period " + 
                       c(i).horário + "(day" + d + ", timeslot " + 
                       pe + ")")*/
               v_disponibilidades = v_disponibilidades + 1
            }
          }
        }
      }
    }
    if (debug)
      println("IndisponibilidadeProf: " +
              v_disponibilidades)
    v_disponibilidades
  }//fim violadas

/*
  def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {
  
   cromo.indices foreach { e =>
      var continds = 0
      var t = cromo(e).horário
      for(k <- 0 until prob.nRestricoes){
        if(prob.listaRestricoes(k).tipo == "period"){
          if(prob.listaRestricoes(k).curso == prob.eventos(e)){
            for(l <- 0 until prob.listaRestricoes(k).timeslots.length){
              var d = prob.listaRestricoes(k).timeslots(l).dia
              var pe = prob.listaRestricoes(k).timeslots(l).periodo
              if(t == prob.diasHorários(d)(pe))
                continds = continds + 1  
            }
          }
        }
      } 
      penalidades(e) += continds
    }//fim cromo foreach
  }
*/
}
