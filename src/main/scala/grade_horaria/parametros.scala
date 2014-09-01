package grade_horaria

import scala.io.Source

case class Parâmetros (

  // número de indivíduos na população
  val tamPop:Int,

  // número de indivíduos selecionados da população                
  val tamSel:Int,
                    
  // probabilidade de cruzamento                                    
  val pc:Double,
  
  // probabilidade de mutação                  
  val pm:Double,
 
  // semente para o gerador de números aleatórios                 
  val semente:Option[Long],

  // número máximo de gerações
  val nMaxGeração:Int,

  // tamanho do torneio                
  val tamTorneio:Int,

  // quantidades de filhos a ser gerados no cruzamento              
  val nFilhos:Int,
 
  //número de vezes que irá imprimir as gerações na tela
  val divisor:Int,			

  // Probabilidade de realizar o movimento 1 em LS1
  val prob1:Double,

  // Probabilidade de realizar o movimento 2 em LS1           
  val prob2:Double,

  // Probabilidade de realizar o movimento 3 em LS1
  val prob3:Double,
 
  // Limite de passos de excução do LS1
  val maxSteps:Int,

  // Limite de tempo para executar o LS1
  val limiteBL:Double,

  //limite de passos de execução do LS2
  val maxStepsLS2:Int,

  //percentual de indivíduos que vai participar do LS2
  val percentualLS2:Double,
  
  //percentagem dos melhores indivíduos que serão 
  //selecionados para a construção da MEM
  val alfa:Double,
    
  //percentagem do total de eventos que serão usados 
  //no cruzamento com a MEM
  val beta:Double,

  //probabilidade do filho ser gerado com o cruzamento usando a MEM
  val gama:Double,

  //frequência de atualização da MEM 
  val tal:Double,

  //limite de tempo de execução do AG
  var limiteExec:Double,

  //limite de tempo de execução do LS2
  val limiteBL2:Double,

  //número de execuções do AG
  val nExecuções: Int

) extends ag.Parâmetros
  
