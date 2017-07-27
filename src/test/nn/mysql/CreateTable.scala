package nn.mysql

import java.sql.{Connection, DriverManager}

/**
  * Created by s1569687 on 7/26/17.
  */
object CreateTable extends App {
  var connection: Connection = _

  val url = "jdbc:mysql://phantom"
  val username = System.getenv("LIFT_MYSQL_USERNAME")
  val db = username
  val password = System.getenv("LIFT_MYSQL_PASSWORD")
  val sql_script = System.getProperty("user.dir") + "/src/test/nn/mysql/" + "create_table.sql"
  try {
    Class.forName("com.mysql.jdbc.Driver").newInstance
    connection = DriverManager.getConnection(url + "/" + db, username, password)
    val statement = connection.createStatement

    statement.execute(scala.io.Source.fromFile(sql_script).getLines.mkString(""))

    println("Table creation script executed successfully.")
  } catch {
    case e: Exception => e.printStackTrace()
  }
  connection.close()

}
