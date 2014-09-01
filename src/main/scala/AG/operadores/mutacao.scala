package ag

abstract class Mutação[Cromossomo,
                       Ind <: Indivíduo[Cromossomo,Ind]]{
  def opera(c:Ind):Ind

}

abstract class MutaçãoPop[Cromossomo,
                          Ind <: Indivíduo[Cromossomo,Ind],
                          P <: População[Cromossomo, Ind, P]]{
  def opera(pop:P):P 
}
