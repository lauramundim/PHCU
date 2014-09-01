package grade_horaria

import util.control.Breaks._

import grade_horaria._
import ag.Restrição
 
//Restrição Preferencial das instâncias Tim.

class ÚnicaAula(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número estudantes que têm apenas uma 
  //   aula no dia

  val nHD = prob.periodosPorDia

  def penalizaEvento(e:Int, cromo: Cromossomo) = {
    var eCRP = 0
    var t = cromo(e).horário
    //contar cada aluno, no caso de ter uma unica turma em um dia
    var unicasClasses = prob.estudanteNum(e)
    var outrasClasses = 0
  
    //aluno nao deve ter apenas uma aula no dia
    for(i <-0 until prob.nEstudantes){
      if(prob.estudanteEventos(i)(e) == 1){
        //definir outras classes no dia a ser zero para cada aluno
        outrasClasses = 0
        //os alunos nao devem ter uma unica aula em um dia
        breakable{
          for(s <- (t-(t%nHD)) until (t-(t%nHD)+nHD)){
            if(s != t){
              breakable{
                if(cromo.eventosSimultâneos.contains(s)){
                  for(j<-0 until cromo.eventosSimultâneos(s).length){
                   if(prob.estudanteEventos(i)(cromo.eventosSimultâneos(s)(j)) == 1){
                      outrasClasses = outrasClasses + 1
                      break
                    }
                  }
                }
              }
              //se o estudante tem outra aula no dia
              if(outrasClasses > 0){
                //nao contam isso do numero de estudante e de 
                //eventos tendo uma unica classe, nesse dia
                unicasClasses = unicasClasses - 1
                break
              }
            }
          }
        }
      }
    }
    if(debug)
      println("Violações de classes únicas do evento " + e + ": " + unicasClasses)
    unicasClasses   
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    var t = cromo(e).horário
    var classes = 0
    var unicaClasses = 0
    val es = cromo.eventosSimultâneos
    for(i <- 0 until prob.nEstudantes){
      if(prob.estudanteEventos(i)(e) == 1){
        classes = 0
        breakable{
          for(s <- (t-(t%nHD)) until (t-(t%nHD)+nHD)){
            if(classes > 1)
              break
            //estamos na regiao viavel de modo que nao sao 
            //eventos com mesmos estudantes no mesmo horario
            if(s!=t){
              breakable{
                if(es.contains(s)){
                  for(j <- 0 until es(s).length){
                    if(prob.estudanteEventos(i)(es(s)(j)) == 1){
                      classes = classes + 1
                      break
                    }
                  }
                }
              }
            }
          }
        }
        if(classes == 1)
          unicaClasses += 1
      }
    }
    unicaClasses
  }
  
  def violadas(c:Cromossomo):Int = {
    var crp = 0
    var classesDia = 0
    //os alunos nao devem ter uma unica classe em um dia
    for(j <- 0 until prob.nEstudantes){
      classesDia = 0
      // para cada dia
      for(d <- 0 until prob.diasPorSemana){
        classesDia = 0
        breakable{ 
          //para cada horário do dia
          for(t <- 0 until nHD){
            breakable{ 
              // se tiver aula
              if(c.eventosSimultâneos.contains(nHD*d+t)){
                for(k<-0 until c.eventosSimultâneos(nHD*d+t).length){
                  if(prob.estudanteEventos(j)(c.eventosSimultâneos(nHD*d+t)(k)) == 1){
                    // incrementa a variavel
                    classesDia = classesDia + 1
                    break
                  }
                }
              }
            }
            // se ate aqui ja tiver mais de uma aula no dia 
            //ja finaliza
            if(classesDia > 1) 
              break
          }
        }
        // se terminar o for e soh tiver uam aula no dia penaliza
        if(classesDia == 1)
          crp = crp + 1
      }
    }
    if(debug)
      println("Violações de aula unica: " + crp)
    crp
  }

/*
def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {
   cromo.indices foreach { e =>
      var eCRP = 0
      var t = cromo(e).horário
      //contar cada aluno, no caso de ter uma unica turma em um dia
      var unicasClasses = prob.estudanteNum(e)
      var outrasClasses = 0
  
      //aluno nao deve ter apenas uma aula no dia
      for(i <-0 until prob.nEstudantes){
        if(prob.estudanteEventos(i)(e) == 1){
          //definir outras classes no dia a ser zero para cada aluno
          outrasClasses = 0
          //os alunos nao devem ter uma unica aula em um dia
          breakable{
            for(s <- (t-(t%nHD)) until (t-(t%nHD)+nHD)){
              if(s != t){
                breakable{
                  if(cromo.eventosSimultâneos.contains(s)){
                    for(j<-0 until cromo.eventosSimultâneos(s).length){
                     if(prob.estudanteEventos(i)(cromo.eventosSimultâneos(s)(j)) == 1){
                        outrasClasses = outrasClasses + 1
                        break
                      }
                    }
                  }
                }
                //se o estudante tem outra aula no dia
                if(outrasClasses > 0){
                  //nao contam isso do numero de estudante e de 
                  //eventos tendo uma unica classe, nesse dia
                  unicasClasses = unicasClasses - 1
                  break
                }
              }
            }
          }
        }
      }
      penalidades(e) += unicasClasses
    }
  }
*/
}
