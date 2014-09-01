package grade_horaria
import grade_horaria._

import java.io._

class Solução{
  def gravaCtt(p: Problema, nomeResult: String, c: Cromossomo) = {
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

   def gravaConfigParam(nomeArq: String, par: Parâmetros, 
                        melhores: Array[Int], s: Long){
     var fw = new FileWriter(new File(nomeArq),true)
     var bw = new BufferedWriter(fw)
    
     //escrever resultados da calibragem dos parâmetros
     bw.write(par.alfa+ " & " + par.beta + " & " + par.gama + 
              " & " + par.tal.toInt)
     melhores.foreach(a => bw.write(" & " + a))
     bw.write(" & " + s + "\\\\ \\hline\n") 
     bw.close()
     fw.close()
   }

   def gravaTituloResults(nomeArq: String){
     var fw = new FileWriter(new File(nomeArq),true)
     var bw = new BufferedWriter(fw)
     bw.write("Instâncias\t&\tAlgoritmo\t&\tMelhor\t&\tMédia\t&\tDesvio\t&\tPior\t&\tSemente\n")
     bw.close()
     fw.close()
   }
   def gravaResults(nomeArq: String, nomeInst: String, melhor: Int,
                    média: Double, desvio: Double, pior: Int, 
                    ag: String, s: String){

     var fw = new FileWriter(new File(nomeArq),true)
     var bw = new BufferedWriter(fw)
    
     //escrever resultados do AG
     bw.write(nomeInst+ "\t&\t" + ag + "\t&\t" + melhor + "\t&\t" +
              média + "\t&\t" + desvio + "\t&\t" + pior + "\t&\t" +
              s + "\\\\ \\hline\n") 
     bw.close()
     fw.close()
   }

    def gravaGerações(nomeArq: String, melhor: Int, média: Double, 
                      desvio: Double, pior: Int, geração:Int){

     var fw = new FileWriter(new File(nomeArq),true)
     var bw = new BufferedWriter(fw)
    
     //escrever resultados do AG
     bw.write(geração + "\t&\t" + melhor + "\t&\t" +
              média + "\t&\t" + desvio + "\t&\t" + pior)
     bw.write("\\\\ \\hline\n") 
     bw.close()
     fw.close()
   }
   // método que grava os parâmetros
   def geraParCalibragem(alpha: List[Double], beta: List[Double], 
                      gama: List[Double], tau: List[Double], 
                      sementes: List[Long], nomeArq: String){
     var fw = new FileWriter(new File(nomeArq), false)
     var bw = new BufferedWriter(fw)
     bw.write("<p>\n")
     for(s<-sementes;a<-alpha; b<-beta; g<-gama; t<-tau){
       bw.write("<parametros tamPop=\"10\" tamSel=\"2\" " ++
                "pc=\"0.8\" pm=\"0.5\" semente=\"" + s +
                "\" nMaxGeração=\"40\" tamTorneio=\"2\" " ++ 
                "nFilhos=\"1\" divisor=\"2\" prob1=\"1.0\" " ++ 
                "prob2=\"1.0\" prob3=\"1.0\" maxSteps=\"100\" " ++ 
                "limiteBL=\"60\" maxStepsLS2=\"300\" " ++ 
                "percentualLS2=\"0.2\" alfa=\"" + a + "\" beta=\"" +
                b + "\" gama=\"" + g + "\" tal=\"" + t + 
                "\" limiteExec=\"60\" limiteBL2=\"60\" " ++ 
                "nExecuções=\"1\"/>\n")
     }
     bw.write("</p>\n")
     bw.close()
     fw.close()
   }
  
}
