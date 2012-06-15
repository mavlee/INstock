package controllers

import io.Source
import java.net.URL
import java.lang._
import java.io.FileNotFoundException
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

        var totalChange = 1.0

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
        //myPositions = myPositions.filter(_.get("company").containsKey("ticker")).filter(_.containsKey("startDate"))
        myPositions = myPositions.filter(_.get("company").containsKey("name")).filter(_.containsKey("startDate"))
        println("Positions: " + myPositions)
        val stocks = myPositions.map{ p =>
          //val ticker = p.get("company").get("ticker").toString
          val ticker = if (p.get("company").containsKey("ticker")) p.get("company").get("ticker").toString else p.get("company").get("name").toString
          val startDate = p.get("startDate").asInstanceOf[java.util.HashMap[String, Double]]
          val startMonth = startDate.get("month").toInt
          val startYear = startDate.get("year").toInt

          val endDate = if (p.containsKey("endDate")) p.get("endDate").asInstanceOf[java.util.HashMap[String, Double]] else new java.util.HashMap[String, Double]()
          val today = new java.util.Date
          val endMonth = if (endDate.contains("month")) endDate.get("month").toInt else (1+today.getMonth)
          val endYear = if (endDate.contains("year")) endDate.get("year").toInt else (1900+today.getYear)
          println("ticker: %s\nstartDate: %s\nendDate: %s".format(ticker, startDate, endDate))
          val stockInfo = getStockData(ticker, startMonth, startYear, endMonth, endYear)
          println("startPrice: %s\nendPrice: %s\nchange: %s\n".format(stockInfo._1, stockInfo._2, stockInfo._3))
          totalChange *= (stockInfo._3.toDouble + 1)
          (ticker.toString, stockInfo._1.toDouble, stockInfo._2.toDouble, stockInfo._3.toDouble)
        }
        Ok(views.html.index.render(myProfile, stocks, totalChange-1.0))
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

  def getStockData(ticker:String,sM:Int,sY:Int,eM:Int,eY:Int):(Double, Double, Double) = {
    try {
      val startPrice = getStockPrice(ticker, sM, sY)
      val endPrice = getStockPrice(ticker, eM, eY)
      val change = (endPrice-startPrice)/startPrice
      (startPrice, endPrice, change)
    } catch {
      case e:FileNotFoundException => {
        println("%s %d %d %d %d".format(ticker, sM, sY, eM, eY))
        (0, 0, 0)
      }
    }
  }

  def getStockPrice(stock:String, month:Int, year:Int):Double = {
    try {
      val url = "http://ichart.yahoo.com/table.csv?s=%s&a=%d&b=1&c=%d&d=%d&e=31&f=%d&g=w&ignore=.csv".format(stock, (month-1), year, (month-1), year)
      val connection = new URL(url).openConnection
      var lines = Source.fromInputStream(connection.getInputStream).getLines.drop(1).toList
      lines = lines.filter{x:String => (x.split(",").head.split("-").head.toInt == year)}
      if (lines.size == 0) 1
      else {
        val res = lines.map(_.split(",").drop(1).dropRight(2).map(_.toDouble).fold(0.0)(_+_)/4).fold(0.0)(_+_)/lines.size
        println("%s %d %d %s".format(stock, month, year, res))
        res
      }
    } catch {
      case _ => 1
    }
  }
}
