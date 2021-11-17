package ziokoban.core.fieldmap

import zio.test._
import ziokoban.core._
import zio._
import scala.annotation.meta.field

object FieldMapSpec extends DefaultRunnableSpec {

  object Generators {

    def coord(minX: Int = 0, maxX: Int = 40, minY: Int = 0, maxY: Int = 40): Gen[Has[Random] with Has[Sized], Coord] =
      for {
        x <- Gen.int(minX, maxX)
        y <- Gen.int(minY, maxY)
      } yield Coord(x, y)

    def emptyFieldMap(maxWidth: Int, maxHeight: Int): Gen[Has[Random] with Has[Sized], FieldMap] =
      for {
        width  <- Gen.int(1, maxWidth)
        height <- Gen.int(1, maxHeight)
      } yield {
        val fields: Seq[(Coord, Tile.Field)] =
          for {
            x <- 1 to width
            y <- 1 to height
          } yield Coord(x, y) -> Tile.Field(Occupant.None, false)
        FieldMap(fields.toMap)
      }

    /** Generate field map, with one pusher */
    def fieldMap(maxWidth: Int, maxHeight: Int, maxCrates: Int): Gen[Has[Random] with Has[Sized], FieldMap] =
      for {
        emptyMap  <- emptyFieldMap(maxWidth, maxHeight)
        map1      <- addPusher(emptyMap)
        numCrates <- if (maxCrates == 0) Gen.const(0) else Gen.int(1, maxCrates).map(_ min freeFields(map1))
        map2      <- addCrates(map1, numCrates)
        map3      <- addTargets(map2, numCrates)
      } yield map3

    def freeFields(fm: FieldMap): Int = fm.fields.filter { case (_, field) => field.occupant == Occupant.None }.size

    def emptyTileFromFieldMap(fm: FieldMap): Gen[Has[Random] with Has[Sized], (Coord, Tile.Field)] =
      tileFromFieldMap(fm)(f => f.occupant == Occupant.None)

    def nonTargetTileFromFieldMap(fm: FieldMap): Gen[Has[Random] with Has[Sized], (Coord, Tile.Field)] =
      tileFromFieldMap(fm)(f => !f.isTarget)

    def tileFromFieldMap(
      fm: FieldMap
    )(select: Tile.Field => Boolean): Gen[Has[Random] with Has[Sized], (Coord, Tile.Field)] = {
      val selectedFields = fm.fields.toList.filter { case (_, f) => select(f) }
      if (selectedFields.isEmpty)
        Gen.fromZIO(UIO.die(new IllegalArgumentException("no fields comply with selection criteria")))
      else
        for {
          i <- Gen.int(1, selectedFields.size)
        } yield selectedFields.drop(i - 1).head // RVNOTE: is there a way to do this without using head?
    }

    def addOccupant(fm: FieldMap, occupant: Occupant): Gen[Has[Random] with Has[Sized], FieldMap] =
      for {
        emptyTile <- emptyTileFromFieldMap(fm)
      } yield FieldMap(fm.fields + (emptyTile._1 -> emptyTile._2.copy(occupant = occupant)))

    def addPusher(fm: FieldMap): Gen[Has[Random] with Has[Sized], FieldMap] =
      addOccupant(fm, Occupant.Pusher)

    def addCrate(fm: FieldMap): Gen[Has[Random] with Has[Sized], FieldMap] =
      addOccupant(fm, Occupant.Crate)

    def addCrates(fm: FieldMap, crateNumber: Int): Gen[Has[Random] with Has[Sized], FieldMap] =
      if (crateNumber < 1) Gen.const(fm)
      else
        for {
          map <- addCrate(fm)
          remaining = crateNumber - 1
          map1 <-
            if (remaining > 0) addCrates(map, remaining)
            else Gen.const(map)
        } yield map1

    def addTarget(fm: FieldMap): Gen[Has[Random] with Has[Sized], FieldMap] =
      for {
        nonTargetTile <- nonTargetTileFromFieldMap(fm)
      } yield FieldMap(fm.fields + (nonTargetTile._1 -> nonTargetTile._2.copy(isTarget = true)))

    def addTargets(fm: FieldMap, targetNumber: Int): Gen[Has[Random] with Has[Sized], FieldMap] =
      if (targetNumber < 1) Gen.const(fm)
      else
        for {
          map <- addTarget(fm)
          remaining = targetNumber - 1
          map1 <-
            if (remaining > 0) addTargets(map, remaining)
            else Gen.const(map)
        } yield map1

    def twoPusherFieldMap(
      maxWidth: Int,
      maxHeight: Int,
      maxCrates: Int = 0
    ): Gen[Has[Random] with Has[Sized], FieldMap] =
      for {
        mapWithPusher <- fieldMap(maxWidth, maxHeight, maxCrates)
        if mapWithPusher.fields.size > 2 // Ensure room enough for 2 pushers.
        map <- addPusher(mapWithPusher)
      } yield map

    def growEmptyFieldMap(current: FieldMap, remaining: Int): Gen[Has[Random] with Has[Sized], FieldMap] =
      if (remaining <= 0) Gen.const(current)
      else
        for {
          r <- Gen.int(1, 2)
          m <- {
            if (r == 1) incrementFieldMapHorizontal(current)
            else incrementFieldMapVertical(current)
          }
          m1 <- growEmptyFieldMap(m, remaining - 1)
        } yield m1

    def incrementFieldMapHorizontal(fm: FieldMap): Gen[Has[Random] with Has[Sized], FieldMap] = {
      val availableYCoords = fm.fields.keySet.map(_.y).toList
      for {
        yCoord <- Gen.elements(availableYCoords: _*)
        xCoord <- Gen.elements(emptyHorizontalNeigbours(fm, yCoord): _*)
      } yield FieldMap(fm.fields + (Coord(xCoord, yCoord) -> Tile.Field(Occupant.None, false)))
    }

    def incrementFieldMapVertical(fm: FieldMap): Gen[Has[Random] with Has[Sized], FieldMap] = {
      val availableXCoords = fm.fields.keySet.map(_.x).toList
      for {
        xCoord <- Gen.elements(availableXCoords: _*)
        yCoord <- Gen.elements(emptyVerticalNeigbours(fm, xCoord): _*)
      } yield FieldMap(fm.fields + (Coord(xCoord, yCoord) -> Tile.Field(Occupant.None, false)))
    }

    def emptyHorizontalNeigbours(fm: FieldMap, y: Int): List[Int] = {
      val existingXsForY         = fm.fields.keySet.filter(_.y == y).map(_.x)
      val allHorizontalNeigbours = existingXsForY.flatMap(x => Set(x - 1, x + 1))
      (allHorizontalNeigbours diff existingXsForY).toList
    }

    def emptyVerticalNeigbours(fm: FieldMap, x: Int): List[Int] = {
      val existingYsForX       = fm.fields.keySet.filter(_.x == x).map(_.y)
      val allVerticalNeigbours = existingYsForX.flatMap(y => Set(y - 1, y + 1))
      (allVerticalNeigbours diff existingYsForX).toList
    }

  }

  import Assertion._

  def spec =
    getPusherLocationSpec +
      getCrateLocationsSpec +
      getTargetLocationsSpec +
      reachableFieldsSpec +
      normalizeSpec

  def getPusherLocationSpec =
    suite(".getPusherLocation")(
      test("empty LevelMap") {
        val result = FieldMap(Map.empty).pusherLocation
        assert(result)(isFailure)
      },
      test("non-empty LevelMap > no pusher") {
        check(Generators.emptyFieldMap(20, 20)) { fieldMap =>
          val result = fieldMap.pusherLocation
          assert(result)(isFailure)
        }
      },
      test("non-empty LevelMap > one pusher") { // RVTODO; fix
        check(Generators.fieldMap(20, 20, 40)) { fieldMap =>
          val result = fieldMap.pusherLocation
          assert(result)(isSuccess(occupiedByPusher(fieldMap)))
        }
      },
      test("non-empty LevelMap > multiple pushers") {
        check(Generators.twoPusherFieldMap(20, 20)) { fieldMap =>
          val result = fieldMap.pusherLocation
          assert(result)(isFailure)
        }
      },
    )

  def occupiedByPusher(fieldMap: FieldMap): Assertion[Coord] =
    Assertion.assertion("occupiedByPusher")()(actual =>
      fieldMap.fields.get(actual).map(_.occupant).contains(Occupant.Pusher)
    )

  def getCrateLocationsSpec =
    suite(".getCrateLocations")(
      test("empty LevelMap") {
        val result = FieldMap(Map.empty).crateLocations
        assert(result)(isEmpty)
      },
      test("non-empty LevelMap") {
        check(Generators.fieldMap(maxWidth = 20, maxHeight = 20, maxCrates = 40)) { fieldMap =>
          val resultCoords = fieldMap.crateLocations
          val otherCoords  = fieldMap.fields.keySet diff resultCoords

          val resultFields = resultCoords.toList.flatMap(fieldMap.fields.get) // So only fields found for given coords.
          assert(resultFields.size)(equalTo(resultCoords.size)) // Ensure all resultCoords had corresponding fields.
          assert(resultFields.map(_.occupant))(
            forall(equalTo(Occupant.Crate))
          ) // Ensure all resultFields contain crates.

          val otherFields = otherCoords.toList.flatMap(fieldMap.fields.get)
          assert(otherFields.size)(equalTo(otherCoords.size))
          assert(otherFields.map(_.occupant))(
            forall(not(equalTo(Occupant.Crate)))
          ) // Ensure all other fields don't contain crates.
        }
      }
    )

  def getTargetLocationsSpec =
    suite(".getTargetLocations")(
      test("empty LevelMap") {
        val result = FieldMap(Map.empty).targetLocations
        assert(result)(isEmpty)
      },
      test("non-empty LevelMap") {
        check(Generators.fieldMap(maxWidth = 20, maxHeight = 20, maxCrates = 40)) { fieldMap =>
          val resultCoords = fieldMap.targetLocations
          val otherCoords  = fieldMap.fields.keySet diff resultCoords

          val resultFields = resultCoords.toList.flatMap(fieldMap.fields.get) // So only fields found for given coords.
          assert(resultFields.size)(equalTo(resultCoords.size)) // Ensure all resultCoords had corresponding fields.
          assert(resultFields.map(_.isTarget))(forall(isTrue))  // Ensure all resultFields contain crates.

          val otherFields = otherCoords.toList.flatMap(fieldMap.fields.get)
          assert(otherFields.size)(equalTo(otherCoords.size))
          assert(otherFields.map(_.isTarget))(forall(isFalse)) // Ensure all other fields don't contain crates.
        }
      }
    )

  def reachableFieldsSpec =
    suite(".reachableFields")(
      test("non-empty LevelMap (growing algorithm)") {
        val start          = Coord(0, 0)
        val singleFieldMap = FieldMap(Map(start -> Tile.Field(Occupant.None, false)))
        check(Generators.growEmptyFieldMap(singleFieldMap, 100)) { fieldMap =>
          val result = fieldMap.reachableFields(start)
          assert(result)(equalTo(fieldMap.fields.keySet))
        }
      }
    )

  def normalizeSpec =
    suite(".normalize")(
      test("empty LevelMap") {
        val result = FieldMap(Map.empty).normalize
        assert(result.fields)(isEmpty)
      },
      test("one tile LevelMap") {
        check(Generators.coord(-40, 40, -40, 40)) { coord =>
          val map: Map[Coord, Tile.Field] = Map(coord -> Tile.Field(Occupant.None, false))
          val result                      = FieldMap(map).normalize
          assert(result.fields.size)(equalTo(1))
          val minX = result.fields.keySet.map(_.x).min
          val minY = result.fields.keySet.map(_.y).min
          assert(minX)(equalTo(0))
          assert(minY)(equalTo(0))
        }
      },
      test("non-empty LevelMap (growing algorithm)") {
        val start          = Coord(0, 0)
        val singleFieldMap = FieldMap(Map(start -> Tile.Field(Occupant.None, false)))
        check(Generators.growEmptyFieldMap(singleFieldMap, 100)) { fieldMap =>
          val result = fieldMap.normalize
          assert(result.fields.size)(equalTo(101))
          val minX = result.fields.keySet.map(_.x).min
          val minY = result.fields.keySet.map(_.y).min
          assert(minX)(equalTo(0))
          assert(minY)(equalTo(0))
        }
      }
    )

}
