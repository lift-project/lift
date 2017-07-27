package nn.mysql

import java.sql.{Connection, DriverManager, Statement}

/**
  * Created by s1569687 on 7/26/17.
  */
object Connector {
  var connection: Connection = _
  var statement: Statement = _

  /* Connect to the MySQL database */
  val url: String = "jdbc:mysql://phantom"
  val username: String = System.getenv("LIFT_MYSQL_USERNAME")
  val db: String = username
  val password: String = System.getenv("LIFT_MYSQL_PASSWORD")
  try {
    Class.forName("com.mysql.jdbc.Driver").newInstance
    connection = DriverManager.getConnection(url + "/" + db, username, password)
    println("Successfully connected to the MySQL database.")
    statement = connection.createStatement
  } catch {
    case e: Exception => e.printStackTrace()
  }

  def close(): Unit = {
    connection.close()
  }

}
