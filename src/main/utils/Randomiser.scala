package utils

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import com.typesafe.scalalogging.Logger
import java.io.FileWriter
import java.io.FileReader

object Randomiser {
  private val logger = Logger(this.getClass)

  def backup(randomiser: scala.util.Random,
             serialFilePath: String,
             descriptionPostfix: String): Unit = {
    val path = serialFilePath + descriptionPostfix
    val backupDirPath = new File(path).getParentFile.getPath
    val backupDir = new File(backupDirPath)
    if (!backupDir.exists()) backupDir.mkdir

    val oos = new ObjectOutputStream(new FileOutputStream(path))
    oos.writeObject(randomiser)
    oos.close()

    logger.info(f"Backed up a randomiser to $path")
  }


  def restore(serialFilePath: String): Option[scala.util.Random] = {
    try {
      val ois = new ObjectInputStream(new FileInputStream(serialFilePath))
      val random = ois.readObject.asInstanceOf[scala.util.Random]
      ois.close()
      logger.info(f"Restored a randomiser from $serialFilePath")
      Some(random)
    } catch {
      case e: java.io.FileNotFoundException =>
        logger.info(f"Could not restore a randomiser from $serialFilePath")
        None
    }
  }

  def init(useBackedupRandomisers: Boolean,
           seed: Int,
           randomiserBackupPath: Option[String] = None): scala.util.Random = {
    if (useBackedupRandomisers) restore(randomiserBackupPath.getOrElse("")) match {
      case Some(random) => random
      case None => new scala.util.Random(seed)
    }
    else new scala.util.Random(seed)
  }
}


object RandomSeed {
  private val logger = Logger(this.getClass)

  def backup(seed: Int,
             filePath: String): Unit = {
    val backupDirPath = new File(filePath).getParentFile.getPath
    val backupDir = new File(backupDirPath)
    if (!backupDir.exists()) backupDir.mkdir

    val wr = new FileWriter(filePath)
    wr.write(seed.toString)
    wr.close()

    logger.info(f"Backed up a random seed to $filePath")
  }

  def restore(filePath: String): Option[Int] = {
    try {
      val fileReader = new FileReader(filePath)
      val seed: Int = fileReader.read()
      fileReader.close()
      logger.info(f"Restored a seed from $filePath: $seed")
      Some(seed)
    } catch {
      case e: java.io.FileNotFoundException =>
        logger.info(f"Could not restore a seed from $filePath")
        None
    }
  }

}
