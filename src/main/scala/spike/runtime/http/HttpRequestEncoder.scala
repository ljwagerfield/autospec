package spike.runtime.http

import org.http4s.Request
import spike.runtime.{EndpointRequest, EndpointRequestResponse}
import spike.schema.ApplicationSchema

class HttpRequestEncoder {
  def apply[F[_]](schema: ApplicationSchema, history: List[EndpointRequestResponse], request: EndpointRequest): Request[F] = {
    // Do this 1st.
  }
}

