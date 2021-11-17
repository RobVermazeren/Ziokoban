package ziokoban.core.fieldmap

import ziokoban.core._
import scala.util.{Failure, Success, Try}
import scala.annotation.tailrec

case class FieldMap(fields: Map[Coord, Tile.Field]) { 
  def crateLocations: Set[Coord] = fieldsByOccupant(Occupant.Crate)

  def pusherLocation: Try[Coord] =
    fieldsByOccupant(Occupant.Pusher).toList match {
      case h :: Nil => Success(h)
      case _        => Failure(new Exception("Level should contain exactly one pusher."))
    }

  def targetLocations: Set[Coord] =
    fields.filter { case (_, f) => f.isTarget }.keySet

  private def fieldsByOccupant(o: Occupant): Set[Coord] =
    fields.filter { case (_, f) => f.occupant == o }.keySet

  def reachableFields(pusher: Coord): Set[Coord] =
    reachableFields(fields.keySet, pusher)

  private def reachableFields(fields: Set[Coord], start: Coord): Set[Coord] = {
    // Return all neighboring fields for c.
    def neighboringFields(c: Coord): Set[Coord] = {
      val candidateCoords = Set(Coord(c.x + 1, c.y), Coord(c.x - 1, c.y), Coord(c.x, c.y + 1), Coord(c.x, c.y - 1))
      candidateCoords.filter(fields.contains)
    }

    @tailrec
    def go(reached: Set[Coord], todo: List[Coord]): Set[Coord] =
      todo match {
        case Nil => reached

        case c :: cs =>
          val newReached = reached + c
          // Add the neighboring fields that have not already been processed.
          val newTodos = neighboringFields(c) diff newReached
          go(newReached, (cs.toSet ++ newTodos).toList)
      }

    go(Set.empty, List(start))
  }

  def normalize: FieldMap =
    if fields.isEmpty then this
    else 
      val coords = fields.keySet
      val minX = coords.map(_.x).min
      val minY = coords.map(_.y).min
      if (minX == 0) && (minY == 0) then this
      else FieldMap(
        fields.map { case (c,t) => Coord(c.x - minX, c.y - minY) -> t }
      ) 
}

object FieldMap:

  def fromLevelMap(levelMap: LevelMap): FieldMap =
    val fields = levelMap.collect { case (c, f @ Tile.Field(_, _)) =>
      c -> f
    }
    FieldMap(fields)
