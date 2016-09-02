import org.gk.BooleanCron

/**
  * Created by zhangxu on 2016/9/2.
  */
object test {
  def main(args: Array[String]): Unit = {
    while (true) {
      println(BooleanCron.cronMatch("0-11,20-50 * * * * *"))
      Thread.sleep(1000)
    }
  }
}
