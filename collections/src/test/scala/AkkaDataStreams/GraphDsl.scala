import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, RunnableGraph, Sink, Source, Zip}

object  AkkaStreamGraph {
  implicit val system = ActorSystem("Graph")
  implicit val materializer = ActorMaterializer()


  val graph =
    GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      //1
      import GraphDSL.Implicits._

      //2
      val input = builder.add(Source(1 to 1000))
      val increment = builder.add(Flow[Int].map(x => x + 1))
      val multiplier = builder.add(Flow[Int].map(x => x * 10))
      val output = builder.add(Sink.foreach[(Int, Int)](println))

      val broadcats = builder.add(Broadcast[Int](2))
      val zip = builder.add(Zip[Int, Int])

      //3
      input ~> broadcats

      broadcats.out(0) ~> increment ~> zip.in0
      broadcats.out(1) ~> multiplier ~> zip.in1

      zip.out ~> output

      //4
      ClosedShape
    }

  def main(args: Array[String]): Unit = {
    RunnableGraph.fromGraph(graph).run()

  }
}