package grade_horaria
import grade_horaria._

import java.io._

class Estatísticas{

  def gravaParam(arqEst: String, par:Parâmetros){
    var fw = new FileWriter(new File(arqEst),false)
    var bw = new BufferedWriter(fw)

    bw.write("Usando os parâmetros")

    bw.close()
    fw.close()
  }

  def gravaCtt(p: Ctt, nomeResult: String, c: Cromossomo) = {
    var fw = new FileWriter(new File(nomeResult),false)
    var bw = new BufferedWriter(fw)

    for (i <- 0 until c.size){
      bw.write(p.eventos(i)+ " " + p.siglasSalas(c(i).sala) + " " +
            (c(i).horário/p.periodosPorDia).toInt + " " +
             c(i).horário % p.periodosPorDia + "\n")
    }
    bw.close()
    fw.close()
  }

  def gravaTim(nomeResult: String, c: Cromossomo) = {
     var fw = new FileWriter(new File(nomeResult),false)
     var bw = new BufferedWriter(fw)

     for (i <- 0 until c.size){
       bw.write(c(i).horário + " " + c(i).sala + "\n")
     }
    bw.close()
    fw.close()
   }



}

//https://www.youtube.com/watch?v=l2BMAqiMfsM 
//https://www.youtube.com/watch?v=UZfnPkkLCA0
//video ensinando fazer grafos no matplotlib
