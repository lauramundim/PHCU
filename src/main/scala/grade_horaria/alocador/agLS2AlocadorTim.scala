package grade_horaria.alocador

import ag.Cronômetro
import grade_horaria.{AGBaseTim, Parâmetros, Solução}
import grade_horaria.alocador._

class AGLS2AlocadorTim(
  par:Parâmetros,
  arqProb: String,
  arqGer: String = " ", // para escrever os dados das gerações
  arqSol: String = " " //para escrever a sol. no padrão do validador
) extends AGBaseTim[Indivíduo, População](par, arqProb){
  val cronômetro = new Cronômetro
  val reparador = new Reparação(prob)
  val mov1 = new Movimento1(prob, reparador)
  val mov2 = new Movimento2(prob, reparador)
  val mov3 = new Movimento3(prob, reparador)    
  val movimentos = new MovimentoAleatório(prob, reparador)
  val mutação = new MutaçãoPop(par, movimentos)
  val torneio = new Torneio(par)
  val cruzamento = new Cruzamento(par, prob, reparador)
  val buscaLocal = new LS1(prob,mov1,mov2,mov3,par.prob1,par.prob2,
                            par.prob2,par.maxSteps, par.limiteBL)
  val buscaLocal2 = new LS2(prob,mov1,par.maxStepsLS2,par.limiteBL2)
  val atual = new AtualizaçãoPop
  val sol = new Solução

  def run():População = {
    cronômetro.iniciar
    var geração = 1
    var pop =  População(prob, fapt, reparador, par.tamPop)
    pop = buscaLocal.aplica(pop)
    pop = buscaLocal2.aplica(pop, par.percentualLS2)

    //Caso tenha arquivo de gerações, gravar nele as informações
    //  da primeira geração (População inicial)
    if(arqGer != " ")
      sol.gravaGerações(arqGer, pop.melhorAptidão, pop.médiaAptidão,
                        pop.desvioAptidão, pop.piorAptidão, geração)

    val divisor = (par.nMaxGeração / par.divisor).max(1)
    while (geração <= par.nMaxGeração && 
           pop.melhorIndivíduo.apt != 0 &&
           cronômetro.decorrido < par.limiteExec){
     /* if ((geração % divisor) == 0) {
        println("-----------------")
        println("Geração " + geração)
        println("Melhor apt = " + pop.melhorAptidão)
        println("Pior apt = "   + pop.piorAptidão)
        println("Média apt = "  + pop.médiaAptidão)
        println
      }*/
      val pais = torneio.seleciona(pop)
      val filhos = cruzamento.cruza(pais)
      val mutantes = mutação.opera(filhos)
      val melhorados = buscaLocal.aplica(mutantes)
      val melhorados2 = 
         buscaLocal2.aplica(melhorados,par.percentualLS2)

      pop = atual.atualiza(pop, melhorados2)
      geração += 1
      //Caso tenha arquivo de gerações, gravar nele as informações
      //  de todas as gerações do AG 
      if(arqGer != " ")
        sol.gravaGerações(arqGer,pop.melhorAptidão,pop.médiaAptidão,
                          pop.desvioAptidão, pop.piorAptidão,
                          geração)
    }
    /*println("Resultado final")
    println("Melhor apt = " + pop.melhorAptidão)
    println("Pior apt = "   + pop.piorAptidão)
    println("Média apt = "  + pop.médiaAptidão)
    println("Gastou um tempo de " + cronômetro.decorrido)
    println*/
    if(arqSol != " ")
      sol.gravaTim(arqSol, pop.melhorIndivíduo.cromossomo)
    pop
  }
}
  
  
