package nn.cnn.mysql

/**
  * Created by s1569687 on 7/26/17.
  */
object CreateTable extends App {
  def apply() {
    nn.mysql.Connector.statement.execute(scala.io.Source.fromFile(
      System.getProperty("user.dir") + "/../../src/test/nn/cnn/mysql/" + "create_table.sql").getLines.mkString(""))
  }
}