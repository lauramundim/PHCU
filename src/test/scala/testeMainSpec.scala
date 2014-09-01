package grade_horaria
import grade_horaria._
import grade_horaria.basico._
import grade_horaria.basico.mem._
import grade_horaria.alocador._
import grade_horaria.alocador.mem._

import org.scalatest.FlatSpec

class MainSpec extends FlatSpec {

   //cria arquivo de solução para gravar as respostas
   val sol = new Solução()
   //ler o arquivo de parâmetros definidos
   val lerPar = new LerParâmetros("./src/test/scala/parametros.xml")
   //Diretório onde vai salvar os valores por geração
   val dirGer = "./src/test/scala/TESTESFINAIS/COMPARACAO/GERACOES/"
   //Diretório onde vai salvar o resultado final
   val dirResult = "./src/test/scala/TESTESFINAIS/COMPARACAO/"
   //Diretórios onde estão as instâncias
   val dirPATAT2002 = "./src/test/scala/Datasets/PATAT2002/"
   val dirTTC2002 = "./src/test/scala/Datasets/TTC2002/"
   val dirITC2007 = "./src/test/scala/Datasets/ITC2007/curriculum/"

   //listas com os nomes das instâncias
   val PATAT2002 = List()
  /*List("easy01.tim", "easy02.tim", "easy03.tim",
                        "easy04.tim", "easy05.tim", "medium01.tim",
                     "medium02.tim", "medium03.tim", "medium04.tim",
                       "medium05.tim", "hard01.tim", "hard02.tim")*/
   val TTC2002 = List()
   /*List("competition01.tim","competition02.tim","competition03.tim",
        "competition04.tim","competition05.tim","competition06.tim",
        "competition07.tim","competition08.tim","competition09.tim",
        "competition10.tim","competition11.tim","competition12.tim",
        "competition13.tim","competition14.tim","competition15.tim",
        "competition16.tim","competition17.tim","competition18.tim",
        "competition19.tim","competition20.tim")*/
   val ITC2007 = List("comp01.xml", "comp07.xml", "comp11.xml", "comp17.xml", "comp21.xml")
    /* List("comp01.xml", "comp02.xml", "comp03.xml", "comp04.xml",
          "comp05.xml", "comp06.xml", "comp07.xml", "comp08.xml",
          "comp09.xml", "comp10.xml", "comp11.xml", "comp12.xml",
          "comp13.xml", "comp14.xml", "comp15.xml", "comp16.xml",
          "comp17.xml", "comp18.xml", "comp19.xml", "comp20.xml",
          "comp21.xml")*/
   //lista das listas das instâncias
   val instâncias = List(PATAT2002, TTC2002, ITC2007)
  
  "Um algoritmo genético básico com Mem" should
    "evoluir uma solução para easy01.tim" ignore {
   //variáveis para controlar o nome do arquivo de acordo
   //   com a semente
   val nomeArq = "resultados"
   var nomeCompl = ""
   var sem = 0

   //percorre os parâmetros
   for(i<-0 until lerPar.par.length){
     println("----------------------")
     println("Parâmetros: " + i)
     println("----------------------")
     val par = lerPar.getPar(i)
     if(sem%5 == 0) {
        nomeCompl = 
          dirResult++nomeArq++par.semente.get.toString++".txt"
      }
     sol.gravaTituloResults(nomeCompl)
     //var melhor = 0
     //var pior = 0
     //var média = 0.0
     //var desvio = 0.0
     //var melhorExec = -1
     //Para cada lista de instâncias
     for(j<-0 until instâncias.length){ 
       println("----------------------")
       println("Grupo de Instâncias: " + instâncias(j))
       println("----------------------")
       //para cada instância da lista
       for(k<-0 until instâncias(j).length){
         println("----------------------")
         println("Instância: " + instâncias(j)(k))
         println("----------------------")

         //-------------se as intâncias são PATAT2002-------------
         if(j == 0){
           //Algoritmo Genético Básico (AGBasico)
           val agBasico0 = new AGBasicoTim(par,
                                    dirPATAT2002++instâncias(j)(k),
                        dirGer++"AGBasico/PATAT2002/AGBasico-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popBasico0 = agBasico0.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popBasico0.melhorAptidão, 
                            popBasico0.médiaAptidão,
                            popBasico0.desvioAptidão,
                            popBasico0.piorAptidão,
                            "AGBásico", par.semente.get.toString) 
           //Algoritmo Genético Básico com Memória (AGBasicoMEM)
           val agBasicoMEM0 = new AGBasicoMemTim(par,
                                    dirPATAT2002++instâncias(j)(k),
                      dirGer++"AGBasicoMEM/PATAT2002/AGBasicoMEM-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popBasicoMEM0 = agBasicoMEM0.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popBasicoMEM0.melhorAptidão, 
                            popBasicoMEM0.médiaAptidão,
                            popBasicoMEM0.desvioAptidão,
                            popBasicoMEM0.piorAptidão,
                            "AGBásicoMEM", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas 
           //  (AGAlocador)
           val agAlocador0 = new AGAlocadorTim(par,
                                    dirPATAT2002++instâncias(j)(k),
                      dirGer++"AGAlocador/PATAT2002/AGAlocador-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocador0 = agAlocador0.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocador0.melhorAptidão, 
                            popAlocador0.médiaAptidão,
                            popAlocador0.desvioAptidão,
                            popAlocador0.piorAptidão,
                            "AGAlocador", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas com Memória
           //  (AGAlocadorMem)
           val agAlocadorMEM0 = new AGAlocadorMemTim(par,
                                    dirPATAT2002++instâncias(j)(k),
                  dirGer++"AGALocadorMEM/PATAT2002/AGAlocadorMEM-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorMEM0 = agAlocadorMEM0.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorMEM0.melhorAptidão, 
                            popAlocadorMEM0.médiaAptidão,
                            popAlocadorMEM0.desvioAptidão,
                            popAlocadorMEM0.piorAptidão,
                          "AGAlocadorMEM", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas e uma Busca
           //  Local (AGAlocacorBL1)
           val agAlocadorBL10 = new AGLS1AlocadorTim(par,
                                    dirPATAT2002++instâncias(j)(k),
                  dirGer++"AGAlocadorBL1/PATAT2002/AGAlocadorBL1-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorBL10 = agAlocadorBL10.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorBL10.melhorAptidão, 
                            popAlocadorBL10.médiaAptidão,
                            popAlocadorBL10.desvioAptidão,
                            popAlocadorBL10.piorAptidão,
                          "AGAlocadorBL1", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas e duas 
           //  Buscas Locais (AGALocadorBL2)
           val agAlocadorBL20 = new AGLS2AlocadorTim(par,
                                    dirPATAT2002++instâncias(j)(k),
                  dirGer++"AGAlocadorBL2/PATAT2002/AGAlocadorBL2-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorBL20 = agAlocadorBL20.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorBL20.melhorAptidão, 
                            popAlocadorBL20.médiaAptidão,
                            popAlocadorBL20.desvioAptidão,
                            popAlocadorBL20.piorAptidão,
                          "AGAlocadorBL2", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas com 
           //  Memória e uma Busca Local (AGAlocacorMEMBL1)
           val agAlocadorMEMBL10 = new AGLS1AlocadorMemTim(par,
                                    dirPATAT2002++instâncias(j)(k),
            dirGer++"AGAlocadorMEMBL1/PATAT2002/AGAlocadorMEMBL1-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorMEMBL10 = agAlocadorMEMBL10.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorMEMBL10.melhorAptidão, 
                            popAlocadorMEMBL10.médiaAptidão,
                            popAlocadorMEMBL10.desvioAptidão,
                            popAlocadorMEMBL10.piorAptidão,
                       "AGAlocadorMEMBL1", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas com 
           //  Memória e e duas Buscas Locais (AGAlocadorMEMBL2)
           val agAlocadorMEMBL20 = new AGLS2AlocadorMemTim(par,
                                    dirPATAT2002++instâncias(j)(k),
            dirGer++"AGAlocadorMEMBL2/PATAT2002/AGAlocadorMEMBL2-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorMEMBL20 = agAlocadorMEMBL20.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorMEMBL20.melhorAptidão, 
                            popAlocadorMEMBL20.médiaAptidão,
                            popAlocadorMEMBL20.desvioAptidão,
                            popAlocadorMEMBL20.piorAptidão,
                       "AGAlocadorMEMBL2", par.semente.get.toString)
         }//fim if intâncias PATAT2002

         //-------------se as intâncias são TTC2002-------------
         if(j == 1){
           //Algoritmo Genético Básico (AGBasico)
           val agBasico1 = new AGBasicoTim(par,
                                    dirTTC2002++instâncias(j)(k),
                        dirGer++"AGBasico/TTC2002/AGBasico-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popBasico1 = agBasico1.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popBasico1.melhorAptidão, 
                            popBasico1.médiaAptidão,
                            popBasico1.desvioAptidão,
                            popBasico1.piorAptidão,
                            "AGBásico", par.semente.get.toString)
           //Algoritmo Genético Básico com Memória (AGBasicoMEM)
           val agBasicoMEM1 = new AGBasicoMemTim(par,
                                    dirTTC2002++instâncias(j)(k),
                      dirGer++"AGBasicoMEM/TTC2002/AGBasicoMEM-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popBasicoMEM1 = agBasicoMEM1.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popBasicoMEM1.melhorAptidão, 
                            popBasicoMEM1.médiaAptidão,
                            popBasicoMEM1.desvioAptidão,
                            popBasicoMEM1.piorAptidão,
                            "AGBásicoMEM", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas 
           //  (AGAlocador)
           val agAlocador1 = new AGAlocadorTim(par,
                                    dirTTC2002++instâncias(j)(k),
                      dirGer++"AGAlocador/TTC2002/AGAlocador-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocador1 = agAlocador1.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocador1.melhorAptidão, 
                            popAlocador1.médiaAptidão,
                            popAlocador1.desvioAptidão,
                            popAlocador1.piorAptidão,
                            "AGAlocador", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas com Memória
           //  (AGAlocadorMem)
           val agAlocadorMEM1 = new AGAlocadorMemTim(par,
                                    dirTTC2002++instâncias(j)(k),
                  dirGer++"AGALocadorMEM/TTC2002/AGAlocadorMEM-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorMEM1 = agAlocadorMEM1.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorMEM1.melhorAptidão, 
                            popAlocadorMEM1.médiaAptidão,
                            popAlocadorMEM1.desvioAptidão,
                            popAlocadorMEM1.piorAptidão,
                          "AGAlocadorMEM", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas e uma Busca
           //  Local (AGAlocacorBL1)
           val agAlocadorBL11 = new AGLS1AlocadorTim(par,
                                    dirTTC2002++instâncias(j)(k),
                  dirGer++"AGAlocadorBL1/TTC2002/AGAlocadorBL1-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorBL11 = agAlocadorBL11.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorBL11.melhorAptidão, 
                            popAlocadorBL11.médiaAptidão,
                            popAlocadorBL11.desvioAptidão,
                            popAlocadorBL11.piorAptidão,
                          "AGAlocadorBL1", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas e duas 
           //  Buscas Locais (AGALocadorBL2)
           val agAlocadorBL21 = new AGLS2AlocadorTim(par,
                                    dirTTC2002++instâncias(j)(k),
                  dirGer++"AGAlocadorBL2/TTC2002/AGAlocadorBL2-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorBL21 = agAlocadorBL21.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorBL21.melhorAptidão, 
                            popAlocadorBL21.médiaAptidão,
                            popAlocadorBL21.desvioAptidão,
                            popAlocadorBL21.piorAptidão,
                          "AGAlocadorBL2", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas com 
           //  Memória e uma Busca Local (AGAlocacorMEMBL1)
           val agAlocadorMEMBL11 = new AGLS1AlocadorMemTim(par,
                                    dirTTC2002++instâncias(j)(k),
            dirGer++"AGAlocadorMEMBL1/TTC2002/AGAlocadorMEMBL1-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorMEMBL11 = agAlocadorMEMBL11.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorMEMBL11.melhorAptidão, 
                            popAlocadorMEMBL11.médiaAptidão,
                            popAlocadorMEMBL11.desvioAptidão,
                            popAlocadorMEMBL11.piorAptidão,
                       "AGAlocadorMEMBL1", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas com 
           //  Memória e e duas Buscas Locais (AGAlocacorMEMBL2)
           val agAlocadorMEMBL21 = new AGLS2AlocadorMemTim(par,
                                    dirTTC2002++instâncias(j)(k),
            dirGer++"AGAlocadorMEMBL2/TTC2002/AGAlocadorMEMBL2-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorMEMBL21 = agAlocadorMEMBL21.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorMEMBL21.melhorAptidão, 
                            popAlocadorMEMBL21.médiaAptidão,
                            popAlocadorMEMBL21.desvioAptidão,
                            popAlocadorMEMBL21.piorAptidão,
                       "AGAlocadorMEMBL2", par.semente.get.toString)
         }//fim if intâncias TTC2002

         //-------------se as intâncias são ITC2007-------------
         if(j == 2){
           println("entrei no if")
           //Algoritmo Genético Básico (AGBasico)
           println(dirITC2007++instâncias(j)(k))
           val agBasico2 = new AGBasico(par,
                                    dirITC2007++instâncias(j)(k),
                        dirGer++"AGBasico/ITC2007/AGBasico-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popBasico2 = agBasico2.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popBasico2.melhorAptidão, 
                            popBasico2.médiaAptidão,
                            popBasico2.desvioAptidão,
                            popBasico2.piorAptidão,
                            "AGBásico", par.semente.get.toString)
           //Algoritmo Genético Básico com Memória (AGBasicoMEM)
           val agBasicoMEM2 = new AGBasicoMem(par,
                                    dirITC2007++instâncias(j)(k),
                      dirGer++"AGBasicoMEM/ITC2007/AGBasicoMEM-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popBasicoMEM2 = agBasicoMEM2.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popBasicoMEM2.melhorAptidão, 
                            popBasicoMEM2.médiaAptidão,
                            popBasicoMEM2.desvioAptidão,
                            popBasicoMEM2.piorAptidão,
                            "AGBásicoMEM", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas 
           //  (AGAlocador)
           val agAlocador2 = new AGAlocador(par,
                                    dirITC2007++instâncias(j)(k),
                      dirGer++"AGAlocador/ITC2007/AGAlocador-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocador2 = agAlocador2.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocador2.melhorAptidão, 
                            popAlocador2.médiaAptidão,
                            popAlocador2.desvioAptidão,
                            popAlocador2.piorAptidão,
                            "AGAlocador", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas com Memória
           //  (AGAlocadorMem)
           val agAlocadorMEM2 = new AGAlocadorMem(par,
                                    dirITC2007++instâncias(j)(k),
                  dirGer++"AGALocadorMEM/ITC2007/AGAlocadorMEM-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorMEM2 = agAlocadorMEM2.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorMEM2.melhorAptidão, 
                            popAlocadorMEM2.médiaAptidão,
                            popAlocadorMEM2.desvioAptidão,
                            popAlocadorMEM2.piorAptidão,
                          "AGAlocadorMEM", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas e uma Busca
           //  Local (AGAlocacorBL1)
           val agAlocadorBL12 = new AGLS1Alocador(par,
                                    dirITC2007++instâncias(j)(k),
                  dirGer++"AGAlocadorBL1/ITC2007/AGAlocadorBL1-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorBL12 = agAlocadorBL12.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorBL12.melhorAptidão, 
                            popAlocadorBL12.médiaAptidão,
                            popAlocadorBL12.desvioAptidão,
                            popAlocadorBL12.piorAptidão,
                          "AGAlocadorBL1", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas e duas 
           //  Buscas Locais (AGALocadoBL2)
           val agAlocadorBL22 = new AGLS2Alocador(par,
                                    dirITC2007++instâncias(j)(k),
                  dirGer++"AGAlocadorBL2/ITC2007/AGAlocadorBL2-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorBL22 = agAlocadorBL22.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorBL22.melhorAptidão, 
                            popAlocadorBL22.médiaAptidão,
                            popAlocadorBL22.desvioAptidão,
                            popAlocadorBL22.piorAptidão,
                          "AGAlocadorBL2", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas com 
           //  Memória e uma Busca Local (AGAlocacorMEMBL1)
           val agAlocadorMEMBL12 = new AGLS1AlocadorMem(par,
                                    dirITC2007++instâncias(j)(k),
            dirGer++"AGAlocadorMEMBL1/ITC2007/AGAlocadorMEMBL1-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorMEMBL12 = agAlocadorMEMBL12.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorMEMBL12.melhorAptidão, 
                            popAlocadorMEMBL12.médiaAptidão,
                            popAlocadorMEMBL12.desvioAptidão,
                            popAlocadorMEMBL12.piorAptidão,
                       "AGAlocadorMEMBL1", par.semente.get.toString)
           //Algoritmo Genético usando Alocação de Salas com 
           //  Memória e e duas Buscas Locais (AGAlocacorMEMBL2)
           val agAlocadorMEMBL22 = new AGLS2AlocadorMem(par,
                                    dirITC2007++instâncias(j)(k),
            dirGer++"AGAlocadorMEMBL2/ITC2007/AGAlocadorMEMBL2-"++
                              instâncias(j)(k).split('.')(0)++"("++
                                  par.semente.get.toString++").txt")
           val popAlocadorMEMBL22 = agAlocadorMEMBL22.run()
           sol.gravaResults(nomeCompl, 
                            instâncias(j)(k).split('.')(0), 
                            popAlocadorMEMBL22.melhorAptidão, 
                            popAlocadorMEMBL22.médiaAptidão,
                            popAlocadorMEMBL22.desvioAptidão,
                            popAlocadorMEMBL22.piorAptidão,
                       "AGAlocadorMEMBL2", par.semente.get.toString)
         }//fim if intâncias ITC2007
         sem+=1
       }//fim for k
     }//fim for j
   }//fim for i
  }
}
