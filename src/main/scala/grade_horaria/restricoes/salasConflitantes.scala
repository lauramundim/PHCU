package grade_horaria

import scala.collection.mutable.HashMap

import grade_horaria._
import ag.Restrição
 
//Restrição Inviolável das instâncias XML.

class SalasConflitantes(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número total de conflitos de salas
  //   em que aconteçam mais de um evento em um mesmo horário

  def penalizaEvento(e:Int, cromo:Cromossomo) = {
    var contsala = 0
    var t = cromo(e).horário
    val es = cromo.eventosSimultâneos
    for(i <- 0 until es(t).length){
      if (es(t)(i) != e){
        if (cromo(e).sala == cromo(es(t)(i)).sala){
          contsala = contsala + 1
        }
      }
    }
    if(debug)
      println("Violações de salas conflitantes do evento " + 
        e + ": " + contsala)
    contsala    
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    var contsala = 0
    var t = cromo(e).horário
    val es = cromo.eventosSimultâneos
    for(i <- 0 until es(t).length){
      for(j <- i+1 until es(t).length){
        if(cromo(es(t)(i)).sala == cromo(es(t)(j)).sala)
          contsala = contsala + 1
      }
    }
    contsala
  }

  def violadas(c:Cromossomo):Int = {
    var v_salas =0
    var evento_contado = Array[(String,Int, Int)]()
    //vSalas armazana como chave o horario e a sala, 
    //o array sao qnts vezes esse par de sala e horario foram usados
    var vSalas = new HashMap[(Int, Int), Array[Int]]
    //percorre os eventos
    for(i <- 0 until prob.nEventos){
      if(!evento_contado.contains((prob.eventos(i), c(i).sala, c(i).horário))){
        evento_contado = 
          evento_contado :+ (prob.eventos(i), c(i).sala, c(i).horário)
        if(!vSalas.contains((c(i).sala, c(i).horário)))
          vSalas += (c(i).sala, c(i).horário) -> Array(1)
        else
          vSalas((c(i).sala, c(i).horário)) = 
            vSalas((c(i).sala, c(i).horário)) :+ 1
      }
    }
    //contando qnts vezes cada sala e horario foram usados
    var chaves = vSalas.keys.toArray
    for(j <- 0 until chaves.length){
      if(vSalas(chaves(j)).length > 1){
        
      /*println(vSalas(chaves(j)).length + " lectures in room " + 
             prob.listaSalas(chaves(j)._1).id + " the periodo " +
             chaves(j)._2 + " [" + 
             (vSalas(chaves(j)).length-1) + "]")*/

        v_salas = v_salas + (vSalas(chaves(j)).length-1)
      }      
    }
    if (debug)
      println("SalasConflitantes: " + v_salas)
    v_salas

  } //fim violadas

/*
  def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {
   cromo.indices foreach { e =>
      var contsala = 0
      var t = cromo(e).horário
      for(i <- 0 until cromo.eventosSimultâneos(t).length){
        if (cromo.eventosSimultâneos(t)(i) != e){
          if (cromo(e).sala == cromo(cromo.eventosSimultâneos(t)(i)).sala){
            contsala = contsala + 1
          }
        }
      }
      penalidades(e) += contsala
    }
  }
*/
} //fim classe
