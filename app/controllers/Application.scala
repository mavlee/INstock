package controllers

import java.lang._
import play.api._
import play.api.mvc._
import org.scribe.oauth.OAuthService
import org.scribe.builder.ServiceBuilder
import org.scribe.builder.api.LinkedInApi
import org.scribe.builder.api.Api
import org.scribe.model.Verifier
import org.scribe.model.Token
import org.scribe.model.Response
import org.scribe.model.OAuthRequest
import org.scribe.model.Verb
import com.google.gson.Gson
import json.Profile
import json.Connections
import json.Company
import json.Positions

object Application extends Controller {

  //get the auth token out and get data otherwise redirect them
  def index = Action { implicit request =>
    val accToken = session.get("acc_token")
    val accSecret = session.get("acc_secret")
    accToken match {
      case Some(k:String) => {
        val oauthService = getOauthService
        val accessToken = new Token(k,accSecret.get)
        //get the data
        val profileData = getProfileData(oauthService,accessToken).getBody()
        val connectionData = getConnectionData(oauthService,accessToken).getBody()
        val gson = new Gson() //created to make the objects
        val connections = gson.fromJson(connectionData,classOf[Connections])
        //sort
        connections.values = connections.values.sortWith((x,y) => x.firstName < y.firstName)
        val myProfile = gson.fromJson(profileData,classOf[Profile])
        println("Positions: " + myProfile.positions.getClass)
        import scala.collection.JavaConversions._
        var myPositions = myProfile.positions.asInstanceOf[java.util.LinkedHashMap[String, Any]].get("values").asInstanceOf[java.util.ArrayList[java.util.HashMap[String, java.util.HashMap[String, Any]]]].toList
        myPositions = myPositions.filter(_.get("company").containsKey("ticker")).filter(_.containsKey("startDate")).filter(_.containsKey("endDate"))
        println("Positions: " + myPositions)
        myPositions.foreach{ p =>
          val ticker = p.get("company").get("ticker")
          val startDate = p.get("startDate")
          val endDate = p.get("endDate")
          println("ticker: %s\nstartDate: %s\nendDate: %s".format(ticker, startDate, endDate))
        
        }
        Ok(views.html.index.render(myProfile, connections))
      }
      case _ =>{
        Logger.info("Redirecting to auth page")
        Redirect(routes.Application.auth())
      }
    }
  }

  //the auth action step
  def auth = Action {
    val serv = getOauthService
    val reqT = serv.getRequestToken()
    val authURL = serv.getAuthorizationUrl(reqT)
    //redirect and store request in cookie
    Redirect(authURL).withSession(
        "req_token" -> reqT.getToken(),
        "req_secret" -> reqT.getSecret()
    )
  }

  //the callback route for oauth
  def callback(oauth_token: String, oauth_verifier: String) = Action { implicit request =>
    val reqToken = session.get("req_token")
    val reqSecret = session.get("req_secret")
    reqToken match {
      case Some(k:String) if k==oauth_token => {
        Logger.error("Tokens DID match")
        val serv = getOauthService
        val verifier = new Verifier(oauth_verifier)
        val reqT = new Token(k, request.session.get("req_secret").get)
        val accessToken = serv.getAccessToken(reqT,verifier)
        Redirect(routes.Application.index()).withSession(
              "acc_token" -> accessToken.getToken(),
              "acc_secret" -> accessToken.getSecret()
              )
      }
      case _ => {
        Logger.error("Tokens didn't match")
        Redirect(routes.Application.auth())
      }
    }
  }

  //the following functions should be in some helper class
  def getOauthService:OAuthService = {
    //could extract this to be checked on startup
    val key = Play.current.configuration.getString("linkedin.apiKey") match {
      case Some(k:String) => k
      case _ => throw new Exception("no linkedin api key set in configuration")
    }
    val secret = Play.current.configuration.getString("linkedin.apiSecret") match {
      case Some(k:String) => k
      case _ => throw new Exception("no linkedin api secret set in configuration")
    }
    val callback = Play.current.configuration.getString("linkedin.callback") match {
      case Some(k:String) => k
      case _ => throw new Exception("no linkedin api callback url set in configuration")
    }
    //get the oauth service
    val sb:ServiceBuilder = new ServiceBuilder()
        .provider(classOf[LinkedInApi])
        .apiKey(key)
        .apiSecret(secret)
        .callback(callback)
    sb.build
  }

  def getProfileData(oauthService:OAuthService,accessToken:Token):Response = {
    val fields = "(id,first-name,last-name,summary,industry,headline,picture-url,positions:(company:(name,ticker),start-date,end-date))"
    val requestURL = "http://api.linkedin.com/v1/people/~:"+fields+"?format=json"
    val req = new OAuthRequest(Verb.GET, requestURL);
    val oauthService = getOauthService
    oauthService.signRequest(accessToken, req);
    req.send();
  }

  def getConnectionData(oauthService:OAuthService,accessToken:Token):Response = {
    val fields = "(id,first-name,last-name,summary,industry,headline,picture-url)"
    val requestURL = "http://api.linkedin.com/v1/people/~/connections:"+fields+"?format=json"
    val req = new OAuthRequest(Verb.GET, requestURL);
    oauthService.signRequest(accessToken, req);
    req.send();
  }
}

