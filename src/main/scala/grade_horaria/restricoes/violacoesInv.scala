package grade_horaria

import grade_horaria._
import ag.Restrição

//Restrições Invioláveis das instâncias Tim.

class ViolacõesInv(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número de restrições
  //     invioláveis em um cromossomo

  def penalizaEvento(e: Int, cromo:Cromossomo) = {
   var eCRI = 0
   var t = cromo(e).horário
   val es = cromo.eventosSimultâneos
   for(i <- 0 until es(t).length){
     if(es(t)(i) != e){
       //acrescenta-se o numero de eventos que compartilham a 
       //sala com o horário
       if(cromo(e).sala == cromo(es(t)(i)).sala)
          eCRI = eCRI + 1
        //estudantes em comum, eventos no mesmo horário
        if(prob.eventoCorrelacoes(e)(es(t)(i)))
          eCRI = eCRI + 1				
      }
    }
    if(debug)
      println("Violações de invioláveis do evento " + 
              e + ": " + eCRI)
    eCRI
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    var aCRI = 0 //zera o contador dos eventos afetados
    var t = cromo(e).horário // t eh o horario do evento analisado
    val es = cromo.eventosSimultâneos
    for(i <- 0 until es(t).length){
      for(j <- i+1 until es(t).length){
      //acrescentando por ter eventos na mesma sala no mesmo horario
        if(cromo(es(t)(i)).sala == cromo(es(t)(j)).sala)
          aCRI = aCRI + 1			
       }
       // acrescentado por causa de alunos em comum
       if(es(t)(i) != e){
         if(prob.eventoCorrelacoes(e)(es(t)(i)))
	   aCRI = aCRI + 1
       }
    }
    aCRI
  }

  def violadas(c:Cromossomo):Int = {
    var cri = 0
    for(i <- 0 until prob.nEventos){
      for(j <- i+1 until prob.nEventos){
        //apenas uma turma pode estar em cada sala, 
        //em qualquer horário
        if((c(i).horário == c(j).horário) &&
           (c(i).sala == c(j).sala)) {
          cri = cri + 1
        }
					
       //dois eventos que compartilham estudantes não 
       //podem estar no mesmo horário
       if((c(i).horário == c(j).horário) &&
         (prob.eventoCorrelacoes(i)(j)))
          cri = cri + 1
      }//fim for j
      //um evento deve ocorrer em uma sala adequada
      if(!prob.possiveisSalas(i)(c(i).sala)){
        cri = cri + 1
        if(debug)
          println("Event0 " + i + 
                  "\nNão pode ficar na sala (" + c(i).sala + ")")
      }
    }
    if(debug)
      println("Violações invioláveis: " + cri)
    cri
  }

/*
def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {

   cromo.indices foreach { e =>
     var eCRI = 0
     var t = cromo(e).horário

     for(i <- 0 until cromo.eventosSimultâneos(t).length){
       if(cromo.eventosSimultâneos(t)(i) != e){
         //acrescenta-se o numero de eventos que compartilham a 
         //sala com o horário
         if(cromo(e).sala == 
            cromo(cromo.eventosSimultâneos(t)(i)).sala)
	         eCRI = eCRI + 1
          //estudantes em comum, eventos no mesmo horário
         if(prob.eventoCorrelacoes(e)(cromo.eventosSimultâneos(t)(i)))
	         eCRI = eCRI + 1				
        }
      }
      penalidades(e) += eCRI
    }
  }
*/
}
