import play.api.GlobalSettings
import play.api.mvc._

object Global extends GlobalSettings {
  override def onRouteRequest(request: RequestHeader): Option[Handler] = {
    val sb = new StringBuilder("==================\n")
    val method = request.method
    val uri = request.uri
    sb ++= s"$method $uri\n"
    request.headers.toMap.foreach{
      keyvalues => {
        val key = keyvalues._1
        sb ++= s"$key: "
        keyvalues._2.foreach(value => sb ++= s"$value\n")
      }
    }
    print(sb)
    super.onRouteRequest(request)
  }
}
