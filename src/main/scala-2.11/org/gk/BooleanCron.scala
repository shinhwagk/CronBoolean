package org.gk

import java.time.{LocalDate, LocalTime}

/**
  * Created by zhangxu on 2016/8/31.
  */
object BooleanCron {

  private case class CurrTime(sec: Int, min: Int, hour: Int, day: Int, mon: Int, week: Int, daysOfMonth: Int)

  private case class CronExpression(sec: Option[String], min: Option[String], hour: Option[String], day: Option[String], mon: Option[String], week: Option[String])

  private val rangeRegex = """^(\d+)-(\d+)$""".r

  private val intervalRegex = """^(\d+)/(\d+)$""".r

  private def cronUnitOptionValue(e: String): Option[String] = if (e == "*") None else Some(e)

  private def getCurrTimes: CurrTime = {
    val date = LocalDate.now()
    val time = LocalTime.now()
    val cSec = time.getSecond
    val cMin = time.getMinute
    val cHour = time.getHour
    val cDay = date.getDayOfMonth
    val cMon = date.getMonthValue
    val cWeek = date.getDayOfWeek.getValue
    val cDays = date.lengthOfMonth()
    CurrTime(cSec, cMin, cHour, cDay, cMon, cWeek, cDays)
  }

  private def parseCron(cron: String): Array[String] = {
    val cronUnit = cron.split("\\s")
    if (cronUnit.length >= 6) cronUnit else throw new Exception("format length error.")
  }

  private def generateCronExpression(timer: String): CronExpression = {
    val times = parseCron(timer)
    val secOpt = cronUnitOptionValue(times(0))
    val minOpt = cronUnitOptionValue(times(1))
    val hourOpt = cronUnitOptionValue(times(2))
    val dayOpt = cronUnitOptionValue(times(3))
    val monOpt = cronUnitOptionValue(times(4))
    val weekOpt = cronUnitOptionValue(times(5))
    CronExpression(secOpt, minOpt, hourOpt, dayOpt, monOpt, weekOpt)
  }

  def cronMatch(cronExpressionString: String): Boolean = {
    val cue = CronUnitEnume

    generateCronExpression(cronExpressionString) match {
      case CronExpression(Some(sec), Some(min), Some(hour), Some(day), Some(mon), Some(week)) =>
        cronExpressionUnitsMatch((cue.SEC, sec), (cue.MIN, min), (cue.HOUR, hour), (cue.DAY, day), (cue.MON, mon), (cue.WEEK, week))
      case CronExpression(Some(sec), Some(min), Some(hour), Some(day), _, Some(week)) =>
        cronExpressionUnitsMatch((cue.SEC, sec), (cue.MIN, min), (cue.HOUR, hour), (cue.DAY, day), (cue.WEEK, week))
      case CronExpression(Some(sec), Some(min), Some(hour), _, _, Some(week)) =>
        cronExpressionUnitsMatch((cue.SEC, sec), (cue.MIN, min), (cue.HOUR, hour), (cue.WEEK, week))
      case CronExpression(Some(sec), Some(min), _, _, _, Some(week)) =>
        cronExpressionUnitsMatch((cue.SEC, sec), (cue.MIN, min), (cue.WEEK, week))
      case CronExpression(Some(sec), _, _, _, _, Some(week)) =>
        cronExpressionUnitsMatch((cue.SEC, sec), (cue.WEEK, week))
      case CronExpression(Some(sec), Some(min), Some(hour), Some(day), Some(mon), _) =>
        cronExpressionUnitsMatch((cue.SEC, sec), (cue.MIN, min), (cue.HOUR, hour), (cue.DAY, day), (cue.MON, mon))
      case CronExpression(Some(sec), Some(min), Some(hour), Some(day), _, _) =>
        cronExpressionUnitsMatch((cue.SEC, sec), (cue.MIN, min), (cue.HOUR, hour), (cue.DAY, day))
      case CronExpression(Some(sec), Some(min), Some(hour), _, _, _) =>
        cronExpressionUnitsMatch((cue.SEC, sec), (cue.MIN, min), (cue.HOUR, hour))
      case CronExpression(Some(sec), Some(min), _, _, _, _) =>
        cronExpressionUnitsMatch((cue.SEC, sec), (cue.MIN, min))
      case CronExpression(Some(sec), _, _, _, _, _) =>
        cronExpressionUnitsMatch((cue.SEC, sec))
      case _ => false
    }
  }

  private def cronExpressionUnitsMatch(ces: (CronUnitEnume.Value, String)*) = {
    val unitMatchBycTime = unitMatch(getCurrTimes) _
    ces.map(unitMatchBycTime).reduce(_ && _)
  }

  private def unitMatchCurrTime(unitRange: List[Option[List[Int]]], curr: Int): Boolean = {
    if (unitRange.find(_.isEmpty).nonEmpty) false
    else unitRange.map(_.get).flatMap(u => u).find(_ == curr).nonEmpty
  }

  private def unitMatch(ctime: CurrTime)(cronUnit: (CronUnitEnume.Value, String)) = {
    val CurrTime(cSec, cMin, cHour, cDay, cMon, cWeek, cDays) = ctime
    cronUnit match {
      case (CronUnitEnume.SEC, unit) => secondMatch(unit)(cSec)
      case (CronUnitEnume.MIN, unit) => minuteMatch(unit)(cMin)
      case (CronUnitEnume.HOUR, unit) => hourMatch(unit)(cHour)
      case (CronUnitEnume.DAY, unit) => dayMatch(unit, cDays)(cDay)
      case (CronUnitEnume.MON, unit) => monthMatch(unit)(cMon)
      case (CronUnitEnume.WEEK, unit) => weekMatch(unit)(cWeek)
    }
  }

  private def unitUnfoldToValues(unit: String, minLimit: Int, maxLimit: Int): List[Option[List[Int]]] = {
    unit.split(",").map(_ match {
      case intervalRegex(s, i) => intervalToValues(s.toInt, i.toInt, maxLimit)
      case rangeRegex(l, r) => rangeToValues(l.toInt, r.toInt, minLimit, maxLimit)
      case u: String => Some(List(u.toInt))
    }).toList
  }

  private def secondMatch(unit: String)(implicit curr: Int): Boolean = {
    unitMatchCurrTime(unitUnfoldToValues(unit, 0, 59), curr)
  }

  private def minuteMatch(unit: String)(implicit curr: Int): Boolean = {
    unitMatchCurrTime(unitUnfoldToValues(unit, 0, 59), curr)
  }

  private def hourMatch(unit: String)(implicit curr: Int): Boolean = {
    unitMatchCurrTime(unitUnfoldToValues(unit, 0, 23), curr)
  }

  private def dayMatch(unit: String, daysOfMonth: Int)(implicit curr: Int): Boolean = {
    unitMatchCurrTime(unitUnfoldToValues(unit, 1, daysOfMonth), curr)
  }

  private def monthMatch(unit: String)(implicit curr: Int): Boolean = {
    unitMatchCurrTime(unitUnfoldToValues(unit, 1, 12), curr)
  }

  private def weekMatch(unit: String)(implicit curr: Int): Boolean = {
    unitMatchCurrTime(unitUnfoldToValues(unit, 1, 7), curr)
  }

  private def intervalToValues(start: Int, interval: Int, maxLimit: Int): Option[List[Int]] = {
    if (start < maxLimit && interval < maxLimit)
      Some((start to maxLimit by interval).toList)
    else None
  }

  private def rangeToValues(left: Int, right: Int, minLimit: Int, maxLimit: Int): Option[List[Int]] = {
    if (left < maxLimit && right < maxLimit && left < right)
      Some((left to right).toList)
    else if (left < maxLimit && right < maxLimit)
      Some(((left until maxLimit) ++ (minLimit to right)).toList)
    else None
  }

  object CronUnitEnume extends Enumeration {
    val SEC, MIN, HOUR, DAY, MON, WEEK = Value
  }
}