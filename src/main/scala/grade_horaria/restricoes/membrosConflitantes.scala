package grade_horaria

import grade_horaria._
import ag.Restrição

//Restrição Inviolável das instâncias XML.

class MembrosConflitantes(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número total de conflitos entre eventos
  //   que possuam alunos ou professores em comum e que
  //   ocorram no mesmo horário.

  def penalizaEvento(e:Int, cromo:Cromossomo) = {
    var contconflito = 0
    var t = cromo(e).horário
    val es = cromo.eventosSimultâneos
    for(i <- 0 until es(t).length)
      if(es(t)(i) != e)
        if(prob.eventoCorrelacoes(e)(es(t)(i)))
          contconflito = contconflito + 1
    if(debug)
      println("Violações de membros conflitantes do evento " + e + ": " + contconflito)
    contconflito
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    val es = cromo.eventosSimultâneos
    var contaluno = 0 //zera o contador dos eventos afetados
    var t = cromo(e).horário // t eh o horario do evento analisado
    for(i <- 0 until es(t).length){
      for(j <- i+1 until es(t).length){
      // acrescentado por causa de eventos correlacionados
      if(prob.eventoCorrelacoes(es(t)(i))(es(t)(j)))
        contaluno += 1
      }
    }
    contaluno
  }

  def violadas(c:Cromossomo):Int = {
    var v_conflitos = 0
    //armazeno os id das disciplinas conflitantes e o seu 
    //horario em comum
    var eventos_conflitantes = Array[Tuple3[String,String,Int]]()
    for(i <- 0 until prob.nEventos){
      for(j <- i+1 until prob.nEventos){
        var aulasConflitantes = 0
        //dois eventos que compartilham estudantes nao podem estar 
        //no mesmo horario
        if((c(i).horário == c(j).horário) && 
           (prob.eventoCorrelacoes(i)(j)) && 
           prob.eventos(i) != prob.eventos(j)){
          if(!eventos_conflitantes.contains(
              (prob.eventos(i), prob.eventos(j), c(i).horário))){
            eventos_conflitantes = 
              eventos_conflitantes :+ 
              (prob.eventos(i), prob.eventos(j), c(i).horário)
            aulasConflitantes = aulasConflitantes + 1
            v_conflitos = v_conflitos + 1
          }//fim if
        }//fim if
      }//fim for j
    }//fim for i
    if (debug)
      println("MembrosConflitantes: " + v_conflitos) 
    v_conflitos
  }//fim violadas

  /*def violadas(c:Cromossomo):Int = {
    var v_conflitos = 0 //contador de conflitos
    //eventos_comflitantes armazena os eventos
    //conflitantes e o seu horário em comum
    var eventos_conflitantes = Array[(Int,Int,Int)]()
    for(i <- 0 until prob.nEventos){
      for(j <- i+1 until prob.nEventos){
        //dois eventos correlacionados (com mesmo aluno ou
        //mesmo professor) não podem acontecer no mesmo horário
        if((c(i).horário == c(j).horário) &&
          (prob.eventoCorrelacoes(i)(j))){
          //se esse conflito ainda não foi contado
          if(!eventos_conflitantes.contains((i, j, c(i).horário))){
            eventos_conflitantes :+= (i, j, c(i).horário)
                  v_conflitos = v_conflitos + 1
          }//fim if(!eventos_conflitantes.contains..)
        }//fim if((c(i).horário == c(j).horário)...)
      }//fim for j
    }//fim for i
    if (debug)
      println("MembrosConflitantes: número de restrições violadas " + v_conflitos)      
    v_conflitos
  } // fim violadas*/

/*
def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {
    cromo.indices foreach { e =>
      var contconflito = 0
      var t = cromo(e).horário
      for(i <- 0 until cromo.eventosSimultâneos(t).length)
        if(cromo.eventosSimultâneos(t)(i) != e)
          if(prob.eventoCorrelacoes(e)(cromo.eventosSimultâneos(t)(i)))
            contconflito = contconflito + 1

      penalidades(e) += contconflito
    }//fim cromo foreach
  }
*/
}// fim classe
