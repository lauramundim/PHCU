package grade_horaria.alocador

import ag.Cronômetro
import grade_horaria._
import util.control.Breaks._

class LS1(
  val pbm:Problema,
  Mov1: Movimento1,
  Mov2: Movimento2,
  Mov3: Movimento3,
  val prob1: Double,
  val prob2: Double,
  val prob3: Double,
  val maxSteps:Int,
  val limiteBL: Double
){

  var contadorPassos = 0
  var contaEv = 0
  var melhorou = false

  val cronômetro = new Cronômetro

  def aplica(pop:População):População = {
    var n = 1
  //  println("LS1: ")
    val novosInds = (for (ind <- pop.indivíduos) yield {
                       //print(".")
                       n += 1
                       buscaLocal(ind) })
    //println
    pop.replica(novosInds)
  }


  //var feasible = computeFeasibility

  private def computeFeasibility(ind:Indivíduo) ={
    var soma = 0
    for(e <- 0 until ind.cromossomo.size){
      soma += ind.fapt.avaliaEventoInv(e, ind.cromossomo)
    }
     if(soma != 0) false
     else true
  }

  private def affectedRoomInTimeslotHcv(t:Int, ind:Indivíduo):Int={
    var roomHcv = 0
    val es = ind.cromossomo.eventosSimultâneos
    if(es.contains(t)){
      for(i<-0 until es(t).size){
        for(j<-i+1 until es(t).size){
          if (ind.cromossomo(es(t)(i)).sala == 
              ind.cromossomo(es(t)(j)).sala)
	    roomHcv += 1
        }
      }
    }
    roomHcv;
  }

  private def singleClassesScv(e: Int, ind: Indivíduo): Int = {
    ind.fapt.avaliaAfetadoPref(e, ind.cromossomo)
  }


  private def buscaLocal(ind:Indivíduo):Indivíduo = {  
  // perform local search with given time limit and probabilities
  // for each type of move

  // keep a list of events to go through
  // scramble the list of events to obtain a random order

 // println("Aptidão antes de realizar a busca local: " + ind.apt)
//  println("BL1")  
  val eventList = rng.shuffle((0 until pbm.nEventos).toList)
 
  var neighbourAffectedHcv = 0 
  // partial evaluation of neighbour solution hcv
  var neighbourScv = 0 
  // partial evaluation of neighbour solution scv
  var evCount = 0     // counter of events considered
  var melhor = ind
  var stepCount = 0  // set step counter to zero
  var foundbetter = false
  var t = 0

  val debug = false

  cronômetro.iniciar

  if(!computeFeasibility(melhor)){
    // if the timetable is not feasible try to solve hcv
    breakable {
        var i = 0
        while(evCount < pbm.nEventos){
          if (stepCount > maxSteps || 
              cronômetro.decorrido > limiteBL)
            break 
          var currentHcv = melhor.fapt.avaliaEventoInv(eventList(i),melhor.cromossomo)
          if(currentHcv == 0){
              // if the event on the list does not cause any hcv
              evCount += 1 // increase the counter
          } else {
          // otherwise if the event in consideration caused hcv
          var currentAffectedHcv = 0
          var t_start = rng.nextInt(pbm.nHorários)
          // try moves of type 1
          var t_orig = melhor.cromossomo(eventList(i)).horário
          breakable {
              var h = 0
              t = t_start
             // println("Iniciando Mov1")
              while(h < pbm.nHorários){
                    if(stepCount > maxSteps || 
                       cronômetro.decorrido > limiteBL)
                       break
                    if(rng.nextFloat < prob1){ 
                    // with given probability
                      stepCount += 1
                      if(debug) println("Fazendo mov1 para as invioláveis")
                      val neighbourSolution = 
                        Mov1.move(melhor, eventList(i),t)
                      neighbourAffectedHcv =
                        neighbourSolution.fapt.avaliaAfetadoInv(eventList(i), neighbourSolution.cromossomo) +  
                        affectedRoomInTimeslotHcv(t_orig,neighbourSolution)
                      currentAffectedHcv = 
                        melhor.fapt.avaliaAfetadoInv(eventList(i), melhor.cromossomo) +
                        affectedRoomInTimeslotHcv(t, melhor);

                      if(debug) println("vizinho = " + neighbourAffectedHcv + "   atual =  " + currentAffectedHcv)

                      if(neighbourAffectedHcv < currentAffectedHcv){
                        if(debug) println("Escolhi o VISINHO pois é mlhor q o atual")
                        melhor = neighbourSolution
                        evCount = 0
                        foundbetter = true
                        break
                      }
                    }
                    t= (t+1) % pbm.nHorários
                    h += 1
              } // fim for (int h = 0
          } // fim breakable        
          if(foundbetter){
              foundbetter = false;
          } else {

          if(prob2 != 0){
            breakable{
                var j= (i+1)%pbm.nEventos
                //println("Iniciando Mov2")                  
                while(j != i){ 
                  // try moves of type 2
                  if (stepCount > maxSteps || 
                      cronômetro.decorrido > limiteBL)
                    break
                  if(rng.nextFloat < prob2){
                    // with given probability
                    stepCount += 1
                    val neighbourSolution = 
                      Mov2.move(melhor, eventList(i),eventList(j))
                    neighbourAffectedHcv =
                      neighbourSolution.fapt.avaliaAfetadoInv(eventList(i), neighbourSolution.cromossomo) +  
                      neighbourSolution.fapt.avaliaAfetadoInv(eventList(j), neighbourSolution.cromossomo)
                    currentAffectedHcv = 
                      melhor.fapt.avaliaAfetadoInv(eventList(i), melhor.cromossomo) +
                      melhor.fapt.avaliaAfetadoInv(eventList(j), melhor.cromossomo)

                    if(debug) println("vizinho = " + neighbourAffectedHcv + "   atual =  " + currentAffectedHcv)

                    if(neighbourAffectedHcv < currentAffectedHcv){
                      if(debug) println("Escolhi o VISINHO pois é mlhor q o atual")
                      melhor = neighbourSolution
                      evCount = 0
                      foundbetter = true
                      break
                    }
                  }
                  j = (j+1)%pbm.nEventos
                }
            } // fim breakable
          } // fim if prob2 != 0
          if(foundbetter){
             foundbetter = false;
             // continue;
          } else {

          if(prob3 != 0){
            breakable {
                var j = (i+1) % pbm.nEventos
               // println("Iniciando Mov3")                  
                while(j != i){ 
                  // try moves of type 3
                  if(stepCount > maxSteps || 
                     cronômetro.decorrido > limiteBL)
                    break
                  breakable {
                      var k = (j+1)%pbm.nEventos
                      while(k != i){
                        if(stepCount > maxSteps|| 
                           cronômetro.decorrido > limiteBL)
                          break
                          
                        if(rng.nextFloat < prob3){
                          // with given probability
                          stepCount += 1

                          currentAffectedHcv = 
                            melhor.fapt.avaliaAfetadoInv(eventList(i), melhor.cromossomo) +
                            melhor.fapt.avaliaAfetadoInv(eventList(j), melhor.cromossomo) +
                            melhor.fapt.avaliaAfetadoInv(eventList(k), melhor.cromossomo)

                          val neighbourSolution =
                            Mov3.move(melhor, eventList(i), eventList(j), eventList(k))

                          neighbourAffectedHcv =
                            neighbourSolution.fapt.avaliaAfetadoInv(eventList(i), neighbourSolution.cromossomo) +  
                            neighbourSolution.fapt.avaliaAfetadoInv(eventList(j), neighbourSolution.cromossomo) +  
                            neighbourSolution.fapt.avaliaAfetadoInv(eventList(k), neighbourSolution.cromossomo)
                            
                          if(debug) println("vizinho = " + neighbourAffectedHcv + "   atual =  " + currentAffectedHcv)

                          if(neighbourAffectedHcv < 
                             currentAffectedHcv){
                            if(debug) println("Escolhi o VISINHO pois é mlhor q o atual")
                            melhor = neighbourSolution
                            evCount = 0;
                            foundbetter = true
                            break
                          } 
                        } // fim if (rng.nextFloat

                        if(stepCount > maxSteps|| 
                           cronômetro.decorrido > limiteBL)
                          break

                        if(rng.nextFloat < prob3){  
                          stepCount += 1

                          currentAffectedHcv = 
                            melhor.fapt.avaliaAfetadoInv(eventList(i), melhor.cromossomo) +
                            melhor.fapt.avaliaAfetadoInv(eventList(k), melhor.cromossomo) +
                            melhor.fapt.avaliaAfetadoInv(eventList(j), melhor.cromossomo)
                          
                          // try one of the to possible 3-cycle    
                          val neighbourSolution =
                            Mov3.move(melhor, eventList(i),eventList(k), eventList(j))
                            
                          neighbourAffectedHcv =
                            neighbourSolution.fapt.avaliaAfetadoInv(eventList(i), neighbourSolution.cromossomo) +  
                            neighbourSolution.fapt.avaliaAfetadoInv(eventList(k), neighbourSolution.cromossomo) +  
                            neighbourSolution.fapt.avaliaAfetadoInv(eventList(j), neighbourSolution.cromossomo)
                           
                          if(debug) println("vizinho = " + neighbourAffectedHcv + "   atual =  " + currentAffectedHcv)

                          if(neighbourAffectedHcv < 
                             currentAffectedHcv){
                            if(debug) println("Escolhi o VISINHO pois é mlhor q o atual")
                            melhor = neighbourSolution
                            evCount = 0;
                            foundbetter = true;
                            break
                          } 
                        }
                        k = (k+1) % pbm.nEventos
                      } // fim for k
                  } // fim breakable
                  
                  if(foundbetter)
                    break
                    
                  j = (j+1)%pbm.nEventos
                }  // fim for j

            } // fim breakable
          } // fim if prob3 != 0
          if(foundbetter){
            foundbetter = false;
          } else           
             evCount += 1
      } // fim else
      } // fim else
        } // fim else
   // println("Aptidão dentro LS1 Inv: " + melhor.apt + " evento " + i)
    i = (i+1) % pbm.nEventos
        } // for int i
    } // fim breakable
  } // fim if !feasible

// Fim Primeira Parte

// Início Segunda Parte
  
  if(computeFeasibility(melhor)){ // if the timetable is feasible
    evCount = 0;
    var neighbourHcv = 0
    breakable {
        var i = 0
    
        while(evCount < pbm.nEventos){ 
          // go through the events in the list
          if (stepCount > maxSteps|| 
              cronômetro.decorrido > limiteBL)
             break
            
          var currentScv = melhor.fapt.avaliaEventoPref(eventList(i), melhor.cromossomo)
          if(currentScv == 0){ // if there are no scv
            evCount += 1 // increase counter
          } else {
          // otherwise try all the possible moves
          var t_start = rng.nextInt(pbm.nHorários) // try moves of type 1
          breakable {
              var h= 0
              t = t_start
              //println("Iniciando Mov1 Pref")
              while(h < pbm.nHorários){
                if (stepCount > maxSteps || 
                    cronômetro.decorrido > limiteBL)
                   break
                if(rng.nextFloat < prob1){ // each with given propability
                  stepCount += 1
                  
                  val neighbourSolution = Mov1.move(melhor,eventList(i),t)

                  neighbourHcv = 
                    neighbourSolution.fapt.avaliaAfetadoInv(eventList(i), neighbourSolution.cromossomo); 

                  if(neighbourHcv == 0){
                    //consider the move only if no hcv are introduced
                    // respectively Scv involving event e 
                    neighbourScv = 
                      neighbourSolution.fapt.avaliaEventoPref(eventList(i),neighbourSolution.cromossomo ) + 
	              singleClassesScv(eventList(i),melhor)-
	              singleClassesScv(eventList(i),neighbourSolution)

                    if(debug) println("vizinho = " + neighbourScv + "   atual =  " + currentScv)
                    
                    if(neighbourScv < currentScv){
                      if(debug) println("Escolhi o VISINHO pois é mlhor q o atual")
                      melhor = neighbourSolution
                      evCount = 0
                      foundbetter = true
                      break
                    }
                  }
                }
                t = (t+1) % pbm.nHorários
                h += 1
              } // fim for
          } //    fim breakable
          if(foundbetter){
            foundbetter = false
          } else {
          if(prob2 != 0){
            breakable{
                var j= (i+1)%pbm.nEventos
                //println("Iniciando Mov2 Pref")                        
                while(j != i){
                  //try moves of type 2
                  if (stepCount > maxSteps || 
                      cronômetro.decorrido > limiteBL)
                     break
                  if(rng.nextFloat < prob2){
                  // with the given probability
                    stepCount += 1
                    
                    val neighbourSolution = Mov2.move(melhor,eventList(i),eventList(j))
                    neighbourHcv = 
                      neighbourSolution.fapt.avaliaAfetadoInv(eventList(i), neighbourSolution.cromossomo) + 
	              neighbourSolution.fapt.avaliaAfetadoInv(eventList(j), neighbourSolution.cromossomo) 

                    if(neighbourHcv == 0){
                      //only if no hcv are introduced by the move
                      //compute alterations on scv for neighbour solution

                      neighbourScv =
                       neighbourSolution.fapt.avaliaEventoPref(eventList(i), neighbourSolution.cromossomo) + 
	               singleClassesScv(eventList(i),melhor) - 
		       singleClassesScv(eventList(i),neighbourSolution) +
	               neighbourSolution.fapt.avaliaEventoPref(eventList(j),neighbourSolution.cromossomo) + 
		       singleClassesScv(eventList(j),melhor) - 
		       singleClassesScv(eventList(j),neighbourSolution) 

                      val compararATual = currentScv + melhor.fapt.avaliaEventoPref(eventList(j), melhor.cromossomo)
                      if(debug) println("vizinho = " + neighbourScv + "   atual =  " + compararATual)

                      if (neighbourScv < compararATual){
                        if(debug) println("Escolhi o VISINHO pois é mlhor q o atual")
                        melhor = neighbourSolution
                        evCount = 0;
                        foundbetter = true
                        break
                      }
                    }
                  }
                  j = (j+1) % pbm.nEventos
                }
            } // fim breakable
          }
        if(foundbetter){
          foundbetter = false
        } else {
          if(prob3 != 0){
            breakable {
                var j = (i+1) % pbm.nEventos
                //println("Iniciando Mov3 Pref")                        
                while(j != i){
                  // try moves of type 3
                  if (stepCount > maxSteps || 
                      cronômetro.decorrido > limiteBL)
                    break
                  breakable {
                      var k = (j+1) % pbm.nEventos
                      while(k != i){
                          
                        if(stepCount > maxSteps || 
                           cronômetro.decorrido > limiteBL)
                          break
                          
                        if(rng.nextFloat < prob3){
                          // with given probability try one of the 2 possibles 3-cycles
                          stepCount += 1
                          val neighbourSolution =
                            Mov3.move(melhor, eventList(i),eventList(j), eventList(k))
                          neighbourHcv = 
                            neighbourSolution.fapt.avaliaAfetadoInv(eventList(i), neighbourSolution.cromossomo) + 
	                    neighbourSolution.fapt.avaliaAfetadoInv(eventList(j), neighbourSolution.cromossomo) + 
	                    neighbourSolution.fapt.avaliaAfetadoInv(eventList(k), neighbourSolution.cromossomo) 

                          if(neighbourHcv == 0){
                            neighbourScv =
                              neighbourSolution.fapt.avaliaEventoPref(eventList(i), neighbourSolution.cromossomo) + 
	                      singleClassesScv(eventList(i),melhor) - 
		              singleClassesScv(eventList(i),neighbourSolution) +
	                      neighbourSolution.fapt.avaliaEventoPref(eventList(j),neighbourSolution.cromossomo) + 
		              singleClassesScv(eventList(j),melhor) - 
		              singleClassesScv(eventList(j),neighbourSolution) +
                              neighbourSolution.fapt.avaliaEventoPref(eventList(k),neighbourSolution.cromossomo) + 
		              singleClassesScv(eventList(k),melhor) - 
		              singleClassesScv(eventList(k),neighbourSolution) 

                            val compararATual = currentScv + 
                                                melhor.fapt.avaliaEventoPref(eventList(j), melhor.cromossomo) +
                                                melhor.fapt.avaliaEventoPref(eventList(k), melhor.cromossomo)
                            if(debug) println("vizinho = " + neighbourScv + "   atual =  " + compararATual)

                            if (neighbourScv < compararATual){
                              if(debug) println("Escolhi o VISINHO pois é mlhor q o atual")
                              melhor = neighbourSolution
                              evCount = 0
                              foundbetter = true
                              break
                            }
                          }
                        }
                        
                        if (stepCount > maxSteps || 
                            cronômetro.decorrido > limiteBL)
                          break

                        if(rng.nextFloat < prob3){ 
                              // with the same probability try the other 
                              //possible 3-cycle 
                              // for the same 3 events
                          stepCount += 1
                          
                          val neighbourSolution =
                            Mov3.move(melhor, eventList(i), eventList(k), eventList(j))

                          neighbourHcv = 
                            neighbourSolution.fapt.avaliaAfetadoInv(eventList(i), neighbourSolution.cromossomo) + 
	                    neighbourSolution.fapt.avaliaAfetadoInv(eventList(k), neighbourSolution.cromossomo) + 
	                    neighbourSolution.fapt.avaliaAfetadoInv(eventList(j), neighbourSolution.cromossomo) 

                          if(neighbourHcv == 0){
                            neighbourScv =
                              neighbourSolution.fapt.avaliaEventoPref(eventList(i), neighbourSolution.cromossomo) + 
	                      singleClassesScv(eventList(i),melhor) - 
	                      singleClassesScv(eventList(i),neighbourSolution) +
	                      neighbourSolution.fapt.avaliaEventoPref(eventList(k),neighbourSolution.cromossomo) + 
		              singleClassesScv(eventList(k),melhor) - 
		              singleClassesScv(eventList(k),neighbourSolution) +
                              neighbourSolution.fapt.avaliaEventoPref(eventList(j),neighbourSolution.cromossomo) + 
		              singleClassesScv(eventList(j),melhor) - 
		              singleClassesScv(eventList(j),neighbourSolution)
                            
                            val campararAtual  = currentScv + 
                                                 melhor.fapt.avaliaEventoPref(eventList(k), melhor.cromossomo) +
                                                 melhor.fapt.avaliaEventoPref(eventList(j), melhor.cromossomo)
                            if(debug) println("vizinho = " + neighbourScv + "   atual =  " + campararAtual)

                            if (neighbourScv < campararAtual){
                              if(debug) println("Escolhi o VISINHO pois é mlhor q o atual")
                              melhor = neighbourSolution
                              evCount = 0
                              foundbetter = true
                              break
                            }
                          }
                        }
                        k = (k+1)%pbm.nEventos   
                      }
                  } // fim breakable
                  if(foundbetter)
                    break
                  j = (j+1) % pbm.nEventos 
                }
            } // fim breakable
          }
        if(foundbetter){
          foundbetter = false;
          // continue;
        } else
          evCount += 1
      } // fim else
      } // fim else
      } // fim else
     // println("Aptidão dentro LS1 Pref: " + melhor.apt + " evento " + i)
      i = (i+1)% pbm.nEventos;
        } // for int i = 0
    } // fim breakable    
  } // fim if
 // println("Aptidão depois da busca local: " + melhor.apt)
  melhor
}
}

/*

Esquema de Busca Local 1 (LS1)
entrada: Indivíduo I da população

Para cada evento e_i em E faça
  Se evento e_i é infactível Então
     encontrouMelhor = FALSE
     enquanto existir ainda um movimento não tentado em N1 E
            não encontrouMelhor faça
        Crie um novo vizinho V com o movimento N1
        Aplique o algoritmo de emparelhamento aos horários afetados
        pelo movimento em V tal que suas salas sejam alocadas.
        Avalie parcialmente o resultado do movimento em V.
        Se o movimento em V reduz as violações das restrições invioláveis
        Então
            realize o movimento N1 definitivamente em e_i
            encontrouMelhor = TRUE
        fim-se
     fim-enquanto
   Se não encontrouMelhor então
      enquanto existir ainda um movimento não tentado em N2 E
             não encontrouMelhor faça
        Crie um novo vizinho V com o movimento N2
        Aplique o algoritmo de emparelhamento aos horários afetados
        pelo movimento em V tal que suas salas sejam alocadas.
        Avalie parcialmente o resultado do movimento em V.
        Se o movimento em V reduz as violações das restrições invioláveis
        Então
            realize o movimento N2 definitivamente em e_i
            encontrouMelhor = TRUE
        fim-se
      fim-enquanto
    Senão se não encontrouMelhor então
          enquanto existir ainda um movimento não tentado em N3 E
                   não encontrouMelhor faça
             Crie um novo vizinho V com o movimento N3
             Aplique o algoritmo de emparelhamento aos horários afetados
             pelo movimento em V tal que suas salas sejam alocadas.
             Avalie parcialmente o resultado do movimento em V.
             Se o movimento em V reduz as violações das restrições invioláveis
             Então
               realize o movimento N1 definitivamente em e_i
               encontrouMelhor = TRUE
             fim-se
          fim-enquanto
  fim-se
fim-para

Se não restarem restrições involáveis Então
   para cada evento e_i em E faça
      Se o evento e_i possui uma violação de restrição preferencial então
        Se existir ainda um movimento não tentado Então
           Calcule os movimentos: primeiro N1; se N1 falhar, tente N2;
           Se N1 e N2 falharem, tente N3.
           Aplique o algoritmo de emparelhamento aos horários afetados
           pelo movimento tal que suas salas sejam alocadas.
           Avalie parcialmente o resultado do movimento.
           Se o movimento reduz as violações das restrições invioláveis 
           Então realize os movimentos e vá para linha 4.
           fim-se
        fim-se
      fim-se
   fim-para
 fim-se

saída: um indivíduo I possivelmente melhorado. 
 
 */
