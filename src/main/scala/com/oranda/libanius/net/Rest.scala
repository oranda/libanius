/*
 * Libanius
 * Copyright (C) 2012-2019 James McCabe <james@oranda.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.oranda.libanius.net

import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet
import com.oranda.libanius.dependencies.AppDependencyAccess
import scala.io.Source


object Rest extends AppDependencyAccess {

  protected[net] def query(url: String): String = {
    val httpClient = new DefaultHttpClient()
    val httpResponse = httpClient.execute(new HttpGet(url))
    val entity = httpResponse.getEntity
    var restContent = ""
    if (entity != null) {
      val is = entity.getContent
      restContent = Source.fromInputStream(is).getLines.mkString
      is.close
    }
    httpClient.getConnectionManager.shutdown()

    // Watch out for redirection
    val statusCode = httpResponse.getStatusLine.getStatusCode
    if (restContent.contains("<html") || statusCode != 200)
      throw new RestResponseException(restContent, statusCode)
    restContent
  }
}

class RestResponseException(content: String, httpStatusCode: Int) extends RuntimeException
