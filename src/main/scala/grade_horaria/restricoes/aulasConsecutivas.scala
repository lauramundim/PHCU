package grade_horaria

import util.control.Breaks._

import grade_horaria._
import ag.Restrição

//Restrição Preferencial das instâncias Tim.

class AulasConsecutivas(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número eventos que têm mais de duas aulas 
  //    consecutivas

  val nHD = prob.periodosPorDia
  var nHS = prob.nHorários

  def penalizaEvento(e:Int, cromo: Cromossomo) = {

    var t = cromo(e).horário
    var eCRP = 0
    var encontraSala = true
    val es = cromo.eventosSimultâneos
    //aluno nao deve ter mais do que duas aulas em seguida
    for(i <-0 until prob.nEstudantes){
      if(prob.estudanteEventos(i)(e) == 1){
        //verificar timeslots antes e depois do intervalo de 
        //tempo de eventos e
        if(t%nHD < nHD-1){
          encontraSala = false
          breakable{
            if(es.contains(t+1)){
              for(j <- 0 until es(t+1).length){
                if(prob.estudanteEventos(i)(es(t+1)(j)) == 1){
                  if(t%nHD < nHD-2){
                    breakable{ 
                      if(es.contains(t+2)){
                        for(k <- 0 until es(t+2).length){
                          if(prob.estudanteEventos(i)(es(t+2)(k)) == 1){
                            eCRP = eCRP + 1
                            encontraSala = true
                            break
                          }
                        }
                      }//fim if(es.contains(t+2))
                    }//fim breakable
                  }//fim if(t%nHD < nHD-2)
                  if(t%nHD > 0){
                    breakable{
                      if(es.contains(t-1)){
                        for(k <- 0 until es(t-1).length){
                          if(prob.estudanteEventos(i)(es(t-1)(k)) == 1){
                            eCRP = eCRP + 1
                            encontraSala = true
                            break
                          }
                        }//fim for(k <- 0 
                      }//fim if(es.contains(t-1))
                    }//fim breakable
                  }//fim  if(t%nHD > 0)
                }//fim if(pro.estudanteEventos
                if(encontraSala)
                  break
              }//fim for j<-0
            }//fim if(es.contains(t+1))
          }
        }
        if(t%nHD > 1){
          encontraSala = false
          breakable{
            if(es.contains(t-1)){
              for(j <- 0 until es(t-1).length){
                breakable{
                  if(es.contains(t-2)){
                    for(k <- 0 until es(t-2).length){
                      if(prob.estudanteEventos(i)(es(t-1)(j)) == 1 && 
                         prob.estudanteEventos(i)(es(t-2)(k)) == 1){
                        eCRP = eCRP + 1
                        encontraSala = true
                        break
                      }
                    }//fim for(k <- 0
                  }//fim if(es.contains(t-2))
                }//fim breakable
                if(encontraSala)
                  break
              }//fim for(j <- 0 
            }//fim if(es.contains(t-1))
          }//fim breakable
        }//fim if(t%nHD > 1)
      }//fim if(prob.estudanteEventos(i)(e) == 1)
    }// fim for(i <-0
    if(debug)
      println("Violações de aulas consecutivas do evento " + e + ": " + eCRP)
    eCRP  
 }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    0
  }
  
  def violadas(c:Cromossomo):Int = {
    var classesConsecutivas = 0
    var atendeHorario = true
    var crp = 0
    //os alunos nao devem ter mais do que duas aulas em seguida
    for(j <- 0 until prob.nEstudantes){
      classesConsecutivas = 0
      //contar classes consecutivas em um dia
      for(i <- 0 until nHS){
        // se chegar no primeiro horario do dia zera o contador
        if((i%nHD) == 0){
          classesConsecutivas = 0
        }
        atendeHorario = false
        breakable{
          if(c.eventosSimultâneos.contains(i)){  
            for(k <- 0 until c.eventosSimultâneos(i).length){
              // se o aluno tem aula em um horario
              if(prob.estudanteEventos(j)(c.eventosSimultâneos(i)(k)) == 1){
                // atualiza as variaveis
                atendeHorario = true
                classesConsecutivas = classesConsecutivas + 1
                // se entrar nesse if mais de uam vez soma a penalidade
                if(classesConsecutivas > 2) 
                  crp = crp + 1
                break
              }
            }//fim for(k <- 0
          }//fim if(c.eventosSimultâneos.contains(i))
        }//fim breakable
        //se nao ocorreu nada no horario seta a variavel
        if(!atendeHorario)
          classesConsecutivas = 0
      }
    }

    if(debug)
      println("Violações de aulas consecutivas: " + classesConsecutivas)
    
    classesConsecutivas
  }


/*
def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {

    cromo.indices foreach { e =>
      var t = cromo(e).horário
      var eCRP = 0
      var encontraSala = true
      //aluno nao deve ter mais do que duas aulas em seguida
      for(i <-0 until prob.nEstudantes){
        if(prob.estudanteEventos(i)(e) == 1){
          //verificar timeslots antes e depois do intervalo de 
          //tempo de eventos e
          if(t%nHD < nHD-1){
            encontraSala = false
            breakable{
              if(cromo.eventosSimultâneos.contains(t+1)){
                for(j <- 0 until cromo.eventosSimultâneos(t+1).length){
                  if(prob.estudanteEventos(i)(cromo.eventosSimultâneos(t+1)(j)) == 1){
                    if(t%nHD < nHD-2){
                      breakable{ 
                        if(cromo.eventosSimultâneos.contains(t+2)){
                          for(k <- 0 until cromo.eventosSimultâneos(t+2).length){
                            if(prob.estudanteEventos(i)(cromo.eventosSimultâneos(t+2)(k)) == 1){
                              eCRP = eCRP + 1
                              encontraSala = true
                              break
                            }
                          }
                        }//fim if(cromo.eventosSimultâneos.contains(t+2))
                      }//fim breakable
                    }//fim if(t%nHD < nHD-2)
                    if(t%nHD > 0){
                      breakable{
                        if(cromo.eventosSimultâneos.contains(t-1)){
                          for(k <- 0 until cromo.eventosSimultâneos(t-1).length){
                            if(prob.estudanteEventos(i)(cromo.eventosSimultâneos(t-1)(k)) == 1){
                              eCRP = eCRP + 1
                              encontraSala = true
                              break
                            }
                          }//fim for(k <- 0 
                        }//fim if(cromo.eventosSimultâneos.contains(t-1))
                      }//fim breakable
                    }//fim  if(t%nHD > 0)
                  }//fim if(pro.estudanteEventos
                  if(encontraSala)
                    break
                }//fim for j<-0
              }//fim if(cromo.eventosSimultâneos.contains(t+1))
            }
          }
          if(t%nHD > 1){
            encontraSala = false
            breakable{
              if(cromo.eventosSimultâneos.contains(t-1)){
                for(j <- 0 until cromo.eventosSimultâneos(t-1).length){
                  breakable{
                    if(cromo.eventosSimultâneos.contains(t-2)){
                      for(k <- 0 until cromo.eventosSimultâneos(t-2).length){
                        if(prob.estudanteEventos(i)(cromo.eventosSimultâneos(t-1)(j)) == 1 && 
                           prob.estudanteEventos(i)(cromo.eventosSimultâneos(t-2)(k)) == 1){
                          eCRP = eCRP + 1
                          encontraSala = true
                          break
                        }
                      }//fim for(k <- 0
                    }//fim if(cromo.eventosSimultâneos.contains(t-2))
                  }//fim breakable
                  if(encontraSala)
                    break
                }//fim for(j <- 0 
              }//fim if(cromo.eventosSimultâneos.contains(t-1))
            }//fim breakable
          }//fim if(t%nHD > 1)
        }//fim if(prob.estudanteEventos(i)(e) == 1)
      }// fim for(i <-0
      penalidades(e) += eCRP 
    }// fim foreach
  }
*/
}
