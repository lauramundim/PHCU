package grade_horaria.alocador

import ag.Cronômetro
import grade_horaria._
import util.control.Breaks._

class LS2(
  val pbm:Problema,
  Mov1: Movimento1,
  val maxSteps:Int ,
  val limiteBL2: Double
){

  var contadorPassos = 0
  var contaEv = 0
  var melhorou = false

  val cronômetro = new Cronômetro

  def aplica(pop:População, percentual:Double):População = {
    var n = 1
    //println("LS2: ")
    val novosInds = (for (ind <- pop.indivíduos) yield {
                       //print(".")
                       n += 1
                       buscaLocal(ind, percentual) })
    //println
    pop.replica(novosInds)
  }



  private def buscaLocal(ind:Indivíduo, percentual:Double): Indivíduo = {
    // seleciona aleatoriamente a dada percentagem de horários do
    // número total de horários.
    val eventosSimultâneos = ind.cromossomo.eventosSimultâneos
    val nSel = (percentual * pbm.nHorários).round.toInt
    val s = 
     (rng.shuffle((0 until pbm.nHorários).toList).take(nSel).toSet &
             eventosSimultâneos.keys.toSet).toArray
    val eventosS = s map { eventosSimultâneos(_) }
    val penalidades = eventosS map { le =>
      (le map { e => ind.fapt.avaliaEvento(e, ind.cromossomo)}).sum
    }
    val tMax = s(penalidades.indexOf(penalidades.max))
    val w = eventosSimultâneos(tMax)
      
    var evCount = 0     // counter of events considered
    var melhor = ind
    var stepCount = 0  // set step counter to zero
    var foundbetter = false
    var t = 0

    //println("Aptidão antes do BL2: " + ind.apt)
    cronômetro.iniciar
    breakable {
      var i = 0
      while(evCount < w.size){
        if (stepCount > maxSteps || 
            cronômetro.decorrido > limiteBL2)
          break

          // otherwise if the event in consideration caused hcv
          var t_start = rng.nextInt(pbm.nHorários)
          // try moves of type 1
          var t_orig = melhor.cromossomo(w(i)).horário
          breakable {
            var h = 0
            t = t_start
            while(h < pbm.nHorários){
              if(stepCount > maxSteps || 
                 cronômetro.decorrido > limiteBL2)
                break
              stepCount += 1

              val neighbourSolution = Mov1.move(melhor, w(i), t)
              if( neighbourSolution.apt < melhor.apt){
                melhor = neighbourSolution
                evCount = 0
                foundbetter = true
                break
              }                    
              t = (t+1) % pbm.nHorários
              h += 1
            } // fim while
          } // fim breakable        
          if(foundbetter){
            foundbetter = false;
          } else 
            evCount += 1
          i = (i+1) % w.size
        } // fim while evCount
    } // fim breakable
    //println("Aptidão depois do BL2: " + melhor.apt)
    melhor
  }
}

