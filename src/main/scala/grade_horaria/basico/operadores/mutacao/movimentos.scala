package grade_horaria.basico
import grade_horaria._

import ag.Mutação
//import grade_horaria.basico.Indivíduo

class Movimento1(prob:Problema) extends Mutação[Cromossomo, Indivíduo] {
  // FUNÇÃO: move um evento para um horário qualquer
  def opera(ind:Indivíduo):Indivíduo = {
    val t = rng.nextInt(prob.nHorários)
    val e = rng.nextInt(prob.nEventos)
    if (debug)
       println("mov1: evento sorteado " + e + " no horário " + t)      
    val c = ind.cromossomo
    ind.replica(c updated (e, c(e).copy(horário = t)))
  }
}


class Movimento2(prob:Problema) extends Mutação[Cromossomo,Indivíduo] {
  // FUNÇÃO: permuta os horários de dois eventos quaisquer
  def opera(ind:Indivíduo):Indivíduo = {
    val e1 = rng.nextInt(prob.nEventos)
    var e2 = rng.nextInt(prob.nEventos)
    while (e1 == e2) e2 = rng.nextInt(prob.nEventos)
    var c = ind.cromossomo
    val h1 = c(e1).horário
    if (debug)
       println("mov2: eventos sorteados " + e1 + " e " + e2)
    c = c updated (e1, c(e1).copy(horário = c(e2).horário))
    c = c updated (e2, c(e2).copy(horário = h1))
    ind.replica(c)
  }
}


class Movimento3(prob:Problema) extends Mutação[Cromossomo,Indivíduo] {
  // FUNÇÃO: permuta os horários de três eventos quaisquer
  def opera(ind:Indivíduo):Indivíduo = {
    val e1 = rng.nextInt(prob.nEventos)
    var e2 = rng.nextInt(prob.nEventos)
    while (e1 == e2) e2 = rng.nextInt(prob.nEventos)
    var e3 = rng.nextInt(prob.nEventos)
    while (e3 == e1 || e3 == e2) e3 = rng.nextInt(prob.nEventos)

    var c = ind.cromossomo
    val h1 = c(e1).horário
    if (debug)
       println("mov3: eventos sorteados " + e1 + ", " + e2 + " e " + e3)
    c = c updated (e1, c(e1).copy(horário = c(e2).horário))
    c = c updated (e2, c(e2).copy(horário = c(e3).horário))
    c = c updated (e3, c(e3).copy(horário = h1))
    ind.replica(c)
  }
}

class MovimentoAleatório(prob:Problema) extends Mutação[Cromossomo,Indivíduo] {
  // FUNÇÃO: escolhe um dos três tipos de movimentos e o executa
  //   sobre o cromossomo passado.
  val mov1 = new Movimento1(prob)
  val mov2 = new Movimento2(prob)
  val mov3 = new Movimento3(prob)
  val movimentos = List(mov1,mov2,mov3)

  def opera(ind:Indivíduo):Indivíduo = 
    rng.shuffle(movimentos).head.opera(ind)
  
}

