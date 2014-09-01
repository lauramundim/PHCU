package grade_horaria

import scala.collection.mutable.HashMap

import grade_horaria._
import ag.Restrição

//Restrição Preferencial das instâncias XML.

class AulasIsoladas(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número de aulas isoladas de eventos do mesmo
  //   curriculo, cada evento isolado é uma violação, mesmo que
  //   represente a mesma disciplina


  def penalizaEvento(e: Int, cromo:Cromossomo) = {
    //juntando as aulas desse evento + as aulas dos eventos 
    //correlacionados nao pode sobrar aulas isoadas desse evento
    //separa os horarios de todos eventos q correlaciona com e
    //    def aulaIsolada(e: Int): Int = {
 
    var discHorários = new HashMap[Int, Array[Int]]
    for(i <- 0 until prob.nDisciplinas) discHorários += i -> Array()
    var somaA = 0
    for(i <- 0 until prob.nDisciplinas){
      for(j <- somaA until somaA + prob.listaDisciplinas(i).aulas)
        discHorários(i) = discHorários(i) :+ cromo(j).horário
        somaA = somaA + prob.listaDisciplinas(i).aulas
    }

    val nHD = prob.periodosPorDia
    var t = cromo(e).horário
    var v_aulas_isoladas = 0
    var horariosEventosCorrelacionados = Array[Int]()
  
    for(j <- 0 until prob.nCurriculos){
      var todosHC = Array[Int]()
      // se e está no currículo ele será um elemento de esta, 
      //senao esta sera vazio
      var esta = prob.listaCurriculos(j).refs.find(a =>
                               a.ref == prob.eventos(e)).toArray
      if(!esta.isEmpty){
        for(k <- 0 until prob.listaCurriculos(j).refs.length){
          var aux1 = prob.listaDisciplinas.find(a => 
               a.id == prob.listaCurriculos(j).refs(k).ref).toArray
          var posDisc = prob.listaDisciplinas.indexOf(aux1(0))
          todosHC = todosHC ++ discHorários(posDisc)
        }
        todosHC = todosHC.sortBy(a => a)
        var hsCurriculo = todosHC.distinct
          
        if(hsCurriculo.length <= 1){
          println("!!!CURRICULO SOH COM 1 HORARIO OU 0!!!")
        }
        else{
          //ePos eh a posicao do horario de e no array
          var ePos = hsCurriculo.indexOf(t)
          //eProx eh o horario do proximo de e
          var eProx = -1
          //eAnt eh o horaio antes do e
          var eAnt = -1
            
          //se o e eh o primeiro evento a acontecer
          if(ePos == 0){
            eProx = hsCurriculo(ePos+1)
            //se o e nao eh seguido pelo o outro evento
            if(t + 1 != eProx){
              v_aulas_isoladas = v_aulas_isoladas + 1
              //println("2- violacao por ser o primeiro e nao " + 
              //        "ser seguido")
            }
            else{
              //se sao seguidos mas estao em dias diferentes
              if((t/nHD).toInt != (eProx/nHD).toInt){
                v_aulas_isoladas = v_aulas_isoladas + 1
                //println("3- violacao por ser o primeiro, " + 
                //        "eh seguido mas esta em dias diferentes")
              }
            }//fim else
          }//fim if(ePos == 0)
          //se ele for o ultimo
          if(ePos == hsCurriculo.length-1){
            eAnt = hsCurriculo(ePos-1)
            if(eAnt + 1 != t){
              v_aulas_isoladas = v_aulas_isoladas + 1
            }
            else{
            //se sao seguidos mas estao em dias diferentes
              if((t/nHD).toInt != (eAnt/nHD).toInt){
                v_aulas_isoladas = v_aulas_isoladas + 1
                //println("5 - violacao no ultimo esta seguido " + 
                //        " mas em dias diferentes")
              }
            }//fim else
          }
          else{
            if(ePos != 0){
              eProx = hsCurriculo(ePos+1)
              eAnt = hsCurriculo(ePos-1)
              // se for isolado do antes e do proximo
              if(t+1 != eProx && t-1 != eAnt ){
                v_aulas_isoladas = v_aulas_isoladas + 1
                //println("6- tem janela antes e depois")
              }
              //se for seguido do proximo mas sao em dias 
              //diferentes e o anterior nao eh seguido
              if(t+1 == eProx && 
                (t/nHD).toInt != (eProx/nHD).toInt && 
                t-1 != eAnt){
                v_aulas_isoladas = v_aulas_isoladas + 1
                //println("7- o proximo esta em dia diferente")
              }
              //se for seguido do anterior mas sao em dias 
              //diferentes e o proximo nao eh seguido
              if(t-1 == eAnt && 
                 (t/nHD).toInt != (eAnt/nHD).toInt && 
                 t+1 != eProx){
                v_aulas_isoladas = v_aulas_isoladas + 1
                //println("8- o anterior esta em dia diferente")
              }
            }//fim if(ePos != 0)
          }//fim else
        }//fim else
      }//fim if(!esta.isEmpty)
    }//fim or(j <- 0
    if(debug)
      println("Violações de aulas isoladas do evento " + e +
              ": " + v_aulas_isoladas)
    v_aulas_isoladas
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    0
  }


  def violadas(c:Cromossomo):Int = {
    //para calcular o número de janelas entre eventos 
    //correlacionados vamos usar uma estrura que chamamos de 
    //discHorários discHorários é um hashMap onde as chaves são 
    //as disciplinas em que para cada discplina tem um array de 
    //horários que acontece essa disciplina

    var discHorários = new HashMap[Int, Array[Int]]
    for(i <- 0 until prob.nDisciplinas) discHorários += i -> Array()
    var somaA = 0
    for(i <- 0 until prob.nDisciplinas){
      for(j <- somaA until somaA + prob.listaDisciplinas(i).aulas)
        discHorários(i) = discHorários(i) :+ c(j).horário
      somaA = somaA + prob.listaDisciplinas(i).aulas
    }
    val nHD = prob.periodosPorDia
    //começando a contar as violações
    var v_aulas_isoladas = 0
    //para cada curriculo
    for(j <- 0 until prob.nCurriculos){
      //armazena os horarios de eventos correlacionados, 
      //do mesmo curriculo.
      var todosHC = Array[Int]()
      //percorre a lista de refs do curriculo
      for(k <- 0 until prob.listaCurriculos(j).refs.length){  
        //aux1 eh a disciplina correspondente ao ref do curriculo
        var aux1 = prob.listaDisciplinas.find(a => 
             a.id == prob.listaCurriculos(j).refs(k).ref).toArray
        //pego o index dessa disciplina
        var posDisc = prob.listaDisciplinas.indexOf(aux1(0))
        //concatena tds os horarios das disciplinas desse curriculo
        todosHC = todosHC ++ discHorários(posDisc)
      }
      //ordeno os horarios
      todosHC = todosHC.sortBy(a => a)
      //elimina os horarios repitidos
      var hsCurriculo = todosHC.distinct  
      
      //se o curriculo soh tem um horario, ainda nao qr dizer 
      //q a aula eh isolada
      if(hsCurriculo.length == 1){
        //repeticoes conta qnts vezes esse horario tinha antes 
        //de eliminar os repetidos
        var repeticoes = 
          todosHC.count(x => x == hsCurriculo(0))
        //se for >1 ai sim qr dizer q violou, pois eh garantido
        // q tem mais de 1 aula
        if(repeticoes > 1){
          //println("1-violacao por repeticoes de um unico horario")
          //cada repeticao eh uma aula isolada
          v_aulas_isoladas = v_aulas_isoladas + (repeticoes-1)
        }
      }
      else{
        //se o primeiro horario +1 eh diferente do segundo
        //significa q o primeiro horario ja eh isolado
        if(hsCurriculo(0)+1 != hsCurriculo(1)){
          v_aulas_isoladas = v_aulas_isoladas + 1
          //println("2- violacao pq o primeiro horario e isolado "+ 
                 // hsCurriculo(0))
          var repeticoes =
            todosHC.count(x => x == hsCurriculo(0))
          if(repeticoes > 1){
            v_aulas_isoladas = v_aulas_isoladas + (repeticoes-1)
            //println("2- violacao por repeticoes")
          }
        }
        else{
          //se sao seguidos mas estao em dias diferentes
          if((hsCurriculo(0)/nHD).toInt != 
             (hsCurriculo(1)/nHD).toInt){
            v_aulas_isoladas = v_aulas_isoladas + 1
            //println("3- violacao pq o primeiro horario e" +
                    //" isolado " + hsCurriculo(0))
            var repeticoes =
              todosHC.count(x => x == hsCurriculo(0))
            if(repeticoes > 1){
              v_aulas_isoladas = v_aulas_isoladas + (repeticoes-1)
              //println("3- violacao por repeticoes")
            }
            //se tem mais de 2 horarios, estao em dias diferentes
            if(hsCurriculo.length>2){
              // e o segundo horario do outro dia nao o segue
              if(hsCurriculo(0)+2 != hsCurriculo(2)){
                v_aulas_isoladas = v_aulas_isoladas + 1
                //println("3,1- violacao pq eh isolado do depois" +
                //        hsCurriculo(1))
                var repeticoes = 
                  todosHC.count(x => x == hsCurriculo(1))
                if(repeticoes > 1){
                  v_aulas_isoladas = 
                    v_aulas_isoladas + (repeticoes-1)
                  //println("3,1- violacao por repeticoes")
                }
              }
            }
          }
        }
        //para os demais horarios
        for(k <- 1 until hsCurriculo.length){
        //se o horario atual +1 eh diferente do proximo
        //e o horaios atual -1 eh diferente do anterior
        //significa q eh uma aula isolada
          if(k+1 < hsCurriculo.length){
            //se nao tem seguido nem antes nem depois
            if(hsCurriculo(k) + 1 != hsCurriculo(k+1) &&
               hsCurriculo(k)-1 != hsCurriculo(k-1)){
              v_aulas_isoladas = v_aulas_isoladas + 1
              //println("4-violacao pq eh isolado antes e depois " +
              //         hsCurriculo(k))
              var repeticoes = 
                todosHC.count(x => x == hsCurriculo(k))
              if(repeticoes > 1){
                v_aulas_isoladas = v_aulas_isoladas + (repeticoes-1)
                //println("4- violacao por repeticoes")
              }
            }
            //caso os horarios sejam seguidos, mas seja dias
            // diferentes
            if(hsCurriculo(k) + 1 == hsCurriculo(k+1) &&
               (hsCurriculo(k)/nHD).toInt != (hsCurriculo(k+1)/nHD).toInt){
              if(hsCurriculo(k) -1 != hsCurriculo(k-1)){
                v_aulas_isoladas = v_aulas_isoladas + 1
                //println("5- violacao pq eh isolado do antes" + 
                //        hsCurriculo(k))
                var repeticoes = 
                  todosHC.count(x => x == hsCurriculo(k))
                if(repeticoes > 1){
                  v_aulas_isoladas = 
                    v_aulas_isoladas + (repeticoes-1)
                  //println("5- violacao por repeticoes")
                }
              }
              if(k+2 < hsCurriculo.length){
                if(hsCurriculo(k) + 2 != hsCurriculo(k+2)){
                  v_aulas_isoladas = v_aulas_isoladas + 1
                  //println("6- violacao pq eh isolado do depois" +
                  //         hsCurriculo(k+1))
                  var repeticoes = 
                    todosHC.count(x => x == hsCurriculo(k+1))
                  if(repeticoes > 1){
                    v_aulas_isoladas = 
                      v_aulas_isoladas + (repeticoes-1)
                    //println("6- violacao por repeticoes")
                  }
                }
              }
            }
          }
          //se o ultimo horario nao eh seguido do antepenultimo
          if(k == hsCurriculo.length -1){
            if(hsCurriculo(k) != hsCurriculo(k-1) + 1){
              v_aulas_isoladas = v_aulas_isoladas + 1
              //println("7- violacao pq o ultimo eh isolado " + 
              //         hsCurriculo(k))
              var repeticoes = 
                todosHC.count(x => x == hsCurriculo(k))
              if(repeticoes > 1){
                v_aulas_isoladas = v_aulas_isoladas + (repeticoes-1)
                //println("7- violacao por repeticoes")
              }
            }
            else{
              if((hsCurriculo(k)/nHD).toInt != (hsCurriculo(k-1)/nHD).toInt){
                v_aulas_isoladas = v_aulas_isoladas + 1
                //println("8- violacao pq o ultimo eh isolado em " +
                //        "dias diferentes " + hsCurriculo(k))
                var repeticoes = 
                  todosHC.count(x => x == hsCurriculo(k))
                if(repeticoes > 1){
                  v_aulas_isoladas = 
                    v_aulas_isoladas + (repeticoes-1)
                  //println("8- violacao por repeticoes")
                }
              }
            }
          }
        }
      }
    }
    if (debug)
      println("AulasIsoladas: " + (v_aulas_isoladas*2))
    v_aulas_isoladas*2
  }//fim violadas

/*
def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {
    //juntando as aulas desse evento + as aulas dos eventos correlacionados
    //nao pode sobrar aulas isoadas desse evento
    //separa os horarios de todos eventos q correlaciona com e
    //    def aulaIsolada(e: Int): Int = {

    var discHorários = new HashMap[Int, Array[Int]]
    for(i <- 0 until prob.nDisciplinas) discHorários += i -> Array()
    var somaA = 0
    for(i <- 0 until prob.nDisciplinas){
      for(j <- somaA until somaA + prob.listaDisciplinas(i).aulas)
        discHorários(i) = discHorários(i) :+ cromo(j).horário
        somaA = somaA + prob.listaDisciplinas(i).aulas
    }

    val nHD = prob.periodosPorDia

    cromo.indices foreach { e =>
      var t = cromo(e).horário
      var v_aulas_isoladas = 0
      var horariosEventosCorrelacionados = Array[Int]()
  
      for(j <- 0 until prob.nCurriculos){
        var todosHC = Array[Int]()
          // se e está no currículo ele será um elemento de esta, senao esta sera vazio
        var esta = prob.listaCurriculos(j).refs.find(a =>
                               a.ref == prob.eventos(e)).toArray
        if(!esta.isEmpty){
          for(k <- 0 until prob.listaCurriculos(j).refs.length){
            var aux1 = prob.listaDisciplinas.find(a => a.id == prob.listaCurriculos(j).refs(k).ref).toArray
            var posDisc = prob.listaDisciplinas.indexOf(aux1(0))
            todosHC = todosHC ++ discHorários(posDisc)
          }
          todosHC = todosHC.sortBy(a => a)
          var hsCurriculo = todosHC.distinct
          
          /*println("\n \n curriculo: " + j + " todos horarios: \n")
           for(x <- 0 until todosHC.length){
           print(todosHC(x) + ", ")
           }

           println("curriculo: " + j + " sem horarios: repitidos\n")

           for(x <- 0 until hsCurriculo.length){
           print(hsCurriculo(x) + ", ")

           }
           println("\n")*/
          if(hsCurriculo.length <= 1){
            println("!!!CURRICULO SOH COM 1 HORARIO OU 0!!!")
          }
          else{
            //ePos eh a posicao do horario de e no array
            var ePos = hsCurriculo.indexOf(t)
            //eProx eh o horario do proximo de e
            var eProx = -1
            //eAnt eh o horaio antes do e
            var eAnt = -1
              
            //se o e eh o primeiro evento a acontecer
            if(ePos == 0){
              eProx = hsCurriculo(ePos+1)
              //se o e nao eh seguido pelo o outro evento
              if(t + 1 != eProx){
                v_aulas_isoladas = v_aulas_isoladas + 1
                //println("2- violacao por ser o primeiro e nao ser seguido")
              }
              else{
                //se sao seguidos mas estao em dias diferentes
                if((t/nHD).toInt != (eProx/nHD).toInt){
                  v_aulas_isoladas = v_aulas_isoladas + 1
                    //println("3- violacao por ser o primeiro, eh seguido mas esta em dias diferentes")
                }
              }//fim else
            }//fim if(ePos == 0)
            //se ele for o ultimo
            if(ePos == hsCurriculo.length-1){
              eAnt = hsCurriculo(ePos-1)
              if(eAnt + 1 != t){
                v_aulas_isoladas = v_aulas_isoladas + 1

              }
              else{
                //se sao seguidos mas estao em dias diferentes
                if((t/nHD).toInt != (eAnt/nHD).toInt){
                  v_aulas_isoladas = v_aulas_isoladas + 1
                    //println("5 - violacao no ultimo esta seguido mas em dias diferentes")
                }
              }//fim else
            }
            else{
              if(ePos != 0){
                eProx = hsCurriculo(ePos+1)
                eAnt = hsCurriculo(ePos-1)
                // se for isolado do antes e do proximo
                if(t+1 != eProx && t-1 != eAnt ){
                  v_aulas_isoladas = v_aulas_isoladas + 1
                  //println("6- tem janela antes e depois")
                }
                //se for seguido do proximo mas sao em dias diferentes e o anterior nao eh seguido
                if(t+1 == eProx && (t/nHD).toInt != (eProx/nHD).toInt && t-1 != eAnt){
                  v_aulas_isoladas = v_aulas_isoladas + 1
                  //println("7- o proximo esta em dia diferente")
                }
                //se for seguido do anterior mas sao em dias diferentes e o proximo nao eh seguido
                if(t-1 == eAnt && (t/nHD).toInt != (eAnt/nHD).toInt && t+1 != eProx){
                  v_aulas_isoladas = v_aulas_isoladas + 1
                  //println("8- o anterior esta em dia diferente")
                }
              }//fim if(ePos != 0)
            }//fim else
          }//fim else
        }//fim if(!esta.isEmpty)
      }//fim or(j <- 0
      penalidades(e) += v_aulas_isoladas
    } // fim cromo foreach
  }

*/
}//fim classe
