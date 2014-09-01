package ag

abstract class AG[Cromossomo,
                  S <: Indivíduo[Cromossomo, S],
                  P <: População[Cromossomo, S, P]]{
  def run():P
}
