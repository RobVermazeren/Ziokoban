package nl.itvanced.ziokoban.config

import nl.itvanced.ziokoban.gameoutput.ansiconsole.{Config => GameOutputConfig, CharConfig => GameOutputCharConfig}
import nl.itvanced.ziokoban.levelcollectionprovider.{Config => LevelsConfig}
import zio.{Task, ZIO}
import zio.config._, ConfigDescriptor._, ConfigSource._
import zio.config.typesafe.TypesafeConfigSource._
import zio.config.typesafe.TypesafeConfig

case class GameConfig(
  gameOutput: GameOutputConfig,
  levels: LevelsConfig,
)

object GameConfig {

  // Manual creation of config descriptor, in order to get better understanding of what is going on.
  val gameOutputCharsConfigDescr =
    (string("pusherChar") |@| string("crateChar"))(GameOutputCharConfig.apply, GameOutputCharConfig.unapply)

  val gameOutputConfigDescr =
    (nested("chars") { gameOutputCharsConfigDescr })(GameOutputConfig.apply, GameOutputConfig.unapply)

  val levelsConfigDescr = 
    (string("directory") |@| string("current").optional)(LevelsConfig.apply, LevelsConfig.unapply)  
  
  val gameConfigDescr =
    (nested("gameOutput") { gameOutputConfigDescr } |@| nested("levels") { levelsConfigDescr })(GameConfig.apply, GameConfig.unapply)

  /** Provide Config Layer. 
   *  Order of looking up settings:
   *    1. ziokoban.conf in project root directory
   *    2. reference.conf resource
   */  
  def asLayer =
    TypesafeConfig.fromHoconFile(new java.io.File("./ziokoban.conf"), gameConfigDescr) <> // = orElse
      TypesafeConfig.fromDefaultLoader(gameConfigDescr)

}
