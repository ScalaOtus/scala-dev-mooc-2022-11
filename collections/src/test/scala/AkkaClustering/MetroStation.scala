package AkkaClustering

import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings, ShardRegion}
import com.typesafe.config.ConfigFactory
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import akka.cluster.sharding.external.ExternalShardAllocationStrategy.ShardRegion

import java.util.{Date, UUID}
import scala.collection.immutable
import scala.util.Random

case class TroykaCard(id: String, isAllowed: Boolean)
case class EntryAttempt(troykaCard: TroykaCard, date: Date)
case class EntryRejected(reason: String)
case object EntryAccepted

class Turnstile(validator: ActorRef) extends Actor with ActorLogging{
  override def receive: Receive ={
    case o: TroykaCard =>
      log.info("validator")
      validator ! EntryAttempt(o, new Date)
    case EntryAccepted => log.info("Green")
    case EntryRejected(reason) => log.info(s"Red $reason")
  }
}

class TroykaCovidPassValidator extends Actor with ActorLogging{

  override def preStart(): Unit = {
    super.preStart()
    log.info("start checking")
  }

  override def receive: Receive = {
    case EntryAttempt(card @ TroykaCard(_, isAllowed), _) =>
      log.info(s"validating $card")
      if (isAllowed) sender() ! EntryAccepted
      else sender() ! EntryRejected(s"not your day, sorry")
  }
}

object TurnstileSettings {
  val numberOfShards = 3
  val numberOfEntries = 30

  val extractEntityId: ShardRegion.ExtractEntityId ={
    case attempt @ EntryAttempt(TroykaCard(cardId, _), _) =>
      val entryId = cardId.hashCode % numberOfEntries
      println(s"!!!!! extract entry id for card # ${attempt.troykaCard.id} to entry ID ${entryId}")
      (entryId.toString, attempt)
  }

  val extractShardId: ShardRegion.ExtractShardId={
    case EntryAttempt(TroykaCard(cardId,_),_)=>
      val shardId = cardId.hashCode % numberOfShards
      println(s"!!!!!!!!extract shard id for card # $cardId to entity ID $shardId")
      shardId.toString
  }
}

class MetroStation(port: Int, amountOfTurnstiles: Int) extends  App{
  val config = ConfigFactory.parseString(
    s"""
    akka.remote.artery.canonical.port = $port
    """.stripMargin).withFallback(
    ConfigFactory.load("clusterShardingExample.conf"))

  val system = ActorSystem("DemoCluster", config)

  val validatorShardRegionRef: ActorRef =
    ClusterSharding(system).start(
      typeName = "TroykaCovidPassValidator",
      entityProps = Props[TroykaCovidPassValidator],
      settings = ClusterShardingSettings(system),
      extractEntityId = TurnstileSettings.extractEntityId,
      extractShardId = TurnstileSettings.extractShardId)

  val turnstitles: Seq[ActorRef] = (1 to amountOfTurnstiles)
    .map{
      x=>
        println(s"Before starting actor of turnstitles # $x")
        system.actorOf(Props(new Turnstile(validatorShardRegionRef)))
    }

  Thread.sleep(30000)
  for (_<-1 to 1000){
    val randomTurnstitleIndex = Random.nextInt(amountOfTurnstiles)
    val randomTurnstitle = turnstitles(randomTurnstitleIndex)

    randomTurnstitle ! TroykaCard(UUID.randomUUID().toString, Random.nextBoolean())
    Thread.sleep(200)
  }
}

object ChistyePrudy extends  MetroStation(2551,10)
object Lubanka extends MetroStation(2561, 5)
object OkhotniRad extends MetroStation(2571, 15)
