import org.gk.BooleanCron

/**
  * Created by zhangxu on 2016/9/2.
  */
object test {
  def main(args: Array[String]): Unit = {
    while (true) {
      println(BooleanCron.cronMatch("1-59 * * * * 5"))
      Thread.sleep(1000)
    }
  }
}
