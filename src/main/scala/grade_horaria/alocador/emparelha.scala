package grade_horaria.alocador

import scala.collection.mutable.{Map => MMap, Queue => MQueue}

import grade_horaria._

class GrafoBipartido[Vertice](vert1:List[Vertice], vert2:List[Vertice]) {
  type Aresta = (Vertice, Vertice)
  type MapaIncidência = Map[Vertice,Aresta]
  type MapaAdjacência = Map[Vertice, MapaIncidência]
  type Emparelhamento = Map[Vertice,Vertice]  
  object Feito extends Exception {}

  private var mAdj:MapaAdjacência = Map()

  vert1 foreach {v => insereVertice(v) }
  vert2 foreach {v => insereVertice(v) }

  /** Insere um novo vértice no grafo. */
  def insereVertice(v:Vertice) = {
    mAdj += (v -> Map[Vertice,Aresta]())
  }  

  /** Insere e retorna uma nova aresta de u para v  */
  def insereAresta(u:Vertice, v:Vertice) = {
    val a = (u,v)
    mAdj += ( u -> (mAdj(u) + (v -> a)),
              v -> (mAdj(v) + (u -> a)) )
    a
  }

  /** Retorna o conjunto de todos os vértices vizinhos ao vértice
      dado */
  def vizinhos(v:Vertice) = mAdj(v).keys

  /** Retorna um iterador para todos os vértices do grafo */
  def vertices = mAdj.keys

def emparelha:Emparelhamento = {
  // Realiza o emparelhamento do grafo bipartido usando o método de
  //  Hopcroft e Karp
    var pair_G1 = MMap[Vertice,Option[Vertice]]()
    var pair_G2 = MMap[Vertice,Option[Vertice]]()
    var dist = MMap[Option[Vertice],Int]()
    var q = MQueue[Option[Vertice]]()

    /* Realiza uma busca em largura */
    def bfs():Boolean = {      
      rng.shuffle(vert1) foreach { v =>
        pair_G1(v) match {
          case None => {
             dist += (Some(v) -> 0)
             q.enqueue(Some(v))
          }
          case _ => dist += (Some(v) -> Int.MaxValue)
        }      
      } // foreach v
      dist += (None -> Int.MaxValue)
      while ( q.nonEmpty ) {
        val v = q.dequeue
        v match {
          case Some(vertice) => {
            if (dist(v)  < dist(None))
              rng.shuffle(vizinhos(vertice).toList) foreach { u =>
                if (dist(pair_G2(u)) == Int.MaxValue) {
                   dist += (pair_G2(u) -> (dist(v) + 1))
                   q.enqueue(pair_G2(u))
                }
              } // foreach u
          }
          case None => ()
        }
      } // while
      return (dist(None) !=  Int.MaxValue)
    } // fim bfs


    /* Realiza uma busca em profundidade */
    def dfs(v:Option[Vertice]):Boolean = {
      try {
        v match {
          case None => throw Feito
          case Some(vertice) =>  {
            rng.shuffle(vizinhos(vertice).toList) foreach { u =>
              if (dist(pair_G2(u)) == dist(v) + 1)
                if ( dfs(pair_G2(u)) ) {
                  pair_G2 += (u -> v)
                  pair_G1 += (vertice -> Some(u))
                  throw Feito
                }
            } // foreach u
              dist += (v -> Int.MaxValue)
              false
          }
        } // match
      } catch {
        case Feito => true
      }
    } // fim dfs

    var matching = 0
    vertices foreach { v =>
       pair_G1 += (v -> None)
       pair_G2 += (v -> None)
    }
    while (bfs())
      rng.shuffle(vert1) foreach { v =>
        if (pair_G1(v).isEmpty)
          if (dfs(Some(v)))
             matching += 1
      }
    Map() ++ pair_G1.filter( e => (e._2).isDefined ).map( e =>
      (e._1,
       e._2 match {
            case None => throw Feito
            case Some(x) => x
        }))

  } // final HopcroftKarp

}
