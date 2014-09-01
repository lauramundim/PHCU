package grade_horaria.alocador

import grade_horaria._

import ag.Mutação

class Movimento1(
  prob: Problema,
  rep:  Reparação
) extends Mutação[Cromossomo, Indivíduo] {
  // FUNÇÃO: move um evento e para um horário t qualquer

  def move(ind:Indivíduo, e:Int, t:Int) = {
    if (debug)
       println("mov1: evento sorteado " + e + " no horário " + t)      

    var c = ind.cromossomo
    val oldt = c(e).horário

    c = c updated (e, c(e).copy(horário = t))

    // Realoca as salas dos eventos ocorrendo no horário t
    c = rep.realocaSalas(c, t)
    // Realoca as salas dos eventos ocorrendo no horário antigo
    // do evento e, se existir algum evento.
    if (c.eventosSimultâneos contains oldt)
       c = rep.realocaSalas(c,oldt)
    ind.replica(c)
  }

  def opera(ind:Indivíduo):Indivíduo = {
    val t = rng.nextInt(prob.nHorários)
    val e = rng.nextInt(prob.nEventos)

    move(ind, e, t)
  }
}


class Movimento2(
  prob:Problema,
  rep: Reparação
) extends Mutação[Cromossomo,Indivíduo] {
  // FUNÇÃO: permuta os horários de dois eventos e1 e e2 quaisquer

  def move(ind:Indivíduo, e1: Int, e2: Int) = {
    var c = ind.cromossomo
    val h1 = c(e1).horário
    val h2 = c(e2).horário
    if (debug)
       println("mov2: eventos sorteados " + e1 + " e " + e2)
    c = c updated (e1, c(e1).copy(horário = h2))
    c = c updated (e2, c(e2).copy(horário = h1))

    // Realoca as salas dos eventos ocorrendo em h1
    c = rep.realocaSalas(c, h1)
   // Realoca as salas dos eventos ocorrendo em h2         
    c = rep.realocaSalas(c, h2)
    ind.replica(c)
  }

  def opera(ind:Indivíduo):Indivíduo = {
    val e1 = rng.nextInt(prob.nEventos)
    var e2 = rng.nextInt(prob.nEventos)
    while (e1 == e2) e2 = rng.nextInt(prob.nEventos)
    move(ind, e1, e2)
  }
}


class Movimento3(
  prob:Problema,
  rep: Reparação
) extends Mutação[Cromossomo,Indivíduo] {
  // FUNÇÃO: permuta os horários de três eventos quaisquer

  def move(ind:Indivíduo, e1:Int, e2:Int, e3:Int) = {
    var c = ind.cromossomo
    val h1 = c(e1).horário
    val h2 = c(e2).horário
    val h3 = c(e3).horário

    if (debug)
       println("mov3: eventos sorteados " + e1 + ", " + e2 + " e " + e3)
    c = c updated (e1, c(e1).copy(horário = h2))
    c = c updated (e2, c(e2).copy(horário = h3))
    c = c updated (e3, c(e3).copy(horário = h1))

    // Realoca as salas dos eventos ocorrendo em h1
    c = rep.realocaSalas(c, h1)
   // Realoca as salas dos eventos ocorrendo em h2         
    c = rep.realocaSalas(c, h2)
   // Realoca as salas dos eventos ocorrendo em h2         
    c = rep.realocaSalas(c, h3)                     
    ind.replica(c)
  }

  def opera(ind:Indivíduo):Indivíduo = {
    val e1 = rng.nextInt(prob.nEventos)
    var e2 = rng.nextInt(prob.nEventos)
    while (e1 == e2) e2 = rng.nextInt(prob.nEventos)
    var e3 = rng.nextInt(prob.nEventos)
    while (e3 == e1 || e3 == e2) e3 = rng.nextInt(prob.nEventos)

    move(ind, e1, e2, e3)
  }

}

class MovimentoAleatório(
  prob:Problema,
  rep: Reparação
) extends Mutação[Cromossomo,Indivíduo] {
  // FUNÇÃO: escolhe um dos três tipos de movimentos e o executa
  //   sobre o cromossomo passado.
  val mov1 = new Movimento1(prob, rep)
  val mov2 = new Movimento2(prob, rep)
  val mov3 = new Movimento3(prob, rep)
  val movimentos = List(mov1,mov2,mov3)


  def opera(ind:Indivíduo):Indivíduo = 
    rng.shuffle(movimentos).head.opera(ind)
  
}

