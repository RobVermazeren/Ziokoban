package ziokoban.core

import scala.util.Try

case class Coord(x: Int, y: Int)

/** Type of `Occupant` of a Sokoban field. */
enum Occupant:
  case Pusher, Crate, None

/** Type of `Tile` in Sokoban. */  
enum Tile:
  case Wall extends Tile
  case Void extends Tile
  case Field(occupant: Occupant, isTarget: Boolean) extends Tile

/** A map of `Coord` => `Tile` representing a level. */
type LevelMap = Map[Coord, Tile]

/**
 * Represents a Sokoban level for given `sourceMap`. Fields from the `sourceMap` that are not reachable for the `pusher`
 * are ignored.
 * @param sourceMap
 *   The source level map
 * @param fields
 *   All fields on this map (reachable for pusher)
 * @param walls
 *   All walls on this map
 * @param crates
 *   All crates on this map
 * @param targets
 *   All targets on this map
 */
final case class Level private (
  sourceMap: LevelMap,
  fields: Set[Coord],
  pusher: Coord,
  walls: Set[Coord],
  crates: Set[Coord],
  targets: Set[Coord]
) {
  val height: Int = sourceMap.keySet.map(_.y).max + 1
  val width: Int  = sourceMap.keySet.map(_.x).max + 1

  def isWall(c: Coord): Boolean   = walls.contains(c)
  def hasCrate(c: Coord): Boolean = crates.contains(c)
  def isTarget(c: Coord): Boolean = targets.contains(c)
}

object Level {
  /**
   * Create a `Level` for a `LevelMap`.
   *
   * Fails if level is not valid. Level is valid when:
   *   - exactly one `pusher`.
   *   - number of `targets` is equal to number of `crates`.
   *   - all `targets` are reachable for `pusher`.
   *   - all `crates` are reachable for `pusher`.
   */
  def fromLevelMap(levelMap: LevelMap): Try[Level] = {
    val fieldMap = fieldmap.FieldMap.fromLevelMap(levelMap)

    val walls = levelMap.filter {
      case (_, Tile.Wall) => true
      case _              => false
    }.keySet

    for {
      pusher <- fieldMap.pusherLocation
      crates  = fieldMap.crateLocations
      targets = fieldMap.targetLocations
      if (crates.size == targets.size)
      reachableFields = fieldMap.reachableFields(pusher)
      if (crates subsetOf reachableFields)
      if (targets subsetOf reachableFields)
    } yield Level(levelMap, reachableFields, pusher, walls, crates, targets)
  }

}
