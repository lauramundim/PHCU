package grade_horaria.alocador

import ag.Cronômetro
import grade_horaria.{AGBaseTim, Parâmetros, Solução}
import grade_horaria.alocador._

class AGAlocadorTim(
  par:Parâmetros,
  arqProb: String,
  arqGer: String = " ", // para escrever os dados das gerações
  arqSol: String = " " //para escrever a sol. no padrão do validador
) extends AGBaseTim[Indivíduo, População](par, arqProb){
  val cronômetro = new Cronômetro
  val reparador = new Reparação(prob)
  val movimentos = new MovimentoAleatório(prob, reparador)
  val mutação = new MutaçãoPop(par, movimentos)
  val torneio = new Torneio(par)
  val cruzamento = new Cruzamento(par, prob, reparador)
  val atual = new AtualizaçãoPop
  val sol = new Solução

  def run():População = {
    cronômetro.iniciar
    var geração = 1
    var pop =  População(prob, fapt, reparador, par.tamPop)

    //Caso tenha arquivo de gerações, gravar nele as informações
    //  da primeira geração (População inicial)
    if(arqGer != " ")
      sol.gravaGerações(arqGer, pop.melhorAptidão, pop.médiaAptidão,
                        pop.desvioAptidão, pop.piorAptidão, geração)

    val divisor = (par.nMaxGeração / par.divisor).max(1)    
    while (geração <= par.nMaxGeração && 
           pop.melhorIndivíduo.apt != 0 &&
           cronômetro.decorrido < par.limiteExec){
     /*if ((geração % divisor) == 0) {
        println("-----------------")
        println("Geração " + geração)
        println("Melhor apt = " + pop.melhorAptidão)
        println("Pior apt = "   + pop.piorAptidão)
        println("Média apt = "  + pop.médiaAptidão)
      }*/
      val pais = torneio.seleciona(pop)
      val filhos = cruzamento.cruza(pais)
      val mutantes = mutação.opera(filhos)
      pop = atual.atualiza(pop,mutantes)
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
    println*/
    if(arqSol != " ")
      sol.gravaTim(arqSol, pop.melhorIndivíduo.cromossomo)
    pop
  }
}
  
  
