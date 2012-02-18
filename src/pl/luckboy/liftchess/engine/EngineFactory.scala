package pl.luckboy.liftchess.engine

/** Fabryka silnika.
 * 
 * @author Łukasz Szpakowski
 */
trait EngineFactory[TSearchInput, TSearcher <: Searcher[TSearchInput]]
{
  def apply(searcher: Searcher[TSearchInput]): TSearcher
}