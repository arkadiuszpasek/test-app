import scala.io.StdIn
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import scala.util.{ Failure, Success }
import spray.json.DefaultJsonProtocol._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

object Main {
  def main(args: Array[String]) {
    case class Request(expression: String)
    case class Response(result: String)
    implicit val requestFormat = jsonFormat1(Request)
    implicit val responseFromat = jsonFormat1(Response)

    implicit val system = ActorSystem("calc-server")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val route =
      path("evaluate") {
        post {
          extractRequest { req =>
              onComplete(Unmarshal(req).to[Request]){
                case Success(request) => {
                  val result = Calculator.calculate(request.expression)
                  result match {
                    case Some(result) => complete(Response(result.toString))
                    case None =>  complete(HttpResponse(StatusCodes.BadRequest)) 
                  }
                }
                case Failure(_) => complete(HttpResponse(StatusCodes.BadRequest))
              }
            }
        }
      }
    val bindingFuture = Http().bindAndHandle(route,"localhost", 5555)

    println("Listening at http://localhost:5555/evaluate\nRETURN to stop")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}