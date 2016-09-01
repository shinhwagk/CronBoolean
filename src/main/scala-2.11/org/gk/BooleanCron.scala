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

  private def optionCronValue(e: String): Option[String] = if (e == "*") None else Some(e)

  private def getCurrTimes: CurrTime = {
    val date = LocalDate.now()
    val time = LocalTime.now()
    val cSec = time.getSecond
    val cMin = time.getMinute
    val cHour = time.getHour
    val cDay = date.getDayOfMonth
    val cMon = date.getMonthValue
    val cWeek = date.getDayOfWeek.getValue
    CurrTime(cSec, cMin, cHour, cDay, cMon, cWeek, date.lengthOfMonth())
  }

  private def parseCron(cron: String): Array[String] = {
    val cronUnit = cron.split("\\s")
    if (cronUnit.length >= 6) cronUnit else throw new Exception("format length error.")
  }

  private def generateCronExpression(timer: String): CronExpression = {
    val times = parseCron(timer)
    val secOpt = optionCronValue(times(0))
    val minOpt = optionCronValue(times(1))
    val hourOpt = optionCronValue(times(2))
    val dayOpt = optionCronValue(times(3))
    val monOpt = optionCronValue(times(4))
    val weekOpt = optionCronValue(times(5))
    CronExpression(secOpt, minOpt, hourOpt, dayOpt, monOpt, weekOpt)
  }

  def cronMatch(cron: String): Boolean = {
    val CurrTime(cSec, cMin, cHour, cDay, cMon, cWeek, cDays) = getCurrTimes

    generateCronExpression(cron) match {
      case CronExpression(Some(sec), Some(min), Some(hour), Some(day), Some(mon), Some(week)) =>
        allTrueMatch(secondMatch(sec)(cSec), minuteMatch(min)(cMin), hourMatch(hour)(cHour), dayMatch(day, cDays)(cDay), monthMatch(mon)(cMon), weekMatch(week)(cWeek))
      case CronExpression(Some(sec), Some(min), Some(hour), Some(day), None, Some(week)) =>
        allTrueMatch(secondMatch(sec)(cSec), minuteMatch(min)(cMin), hourMatch(hour)(cHour), dayMatch(day, cDays)(cDay), weekMatch(week)(cWeek))
      case CronExpression(Some(sec), Some(min), Some(hour), None, None, Some(week)) =>
        allTrueMatch(secondMatch(sec)(cSec), minuteMatch(min)(cMin), hourMatch(hour)(cHour), weekMatch(week)(cWeek))
      case CronExpression(Some(sec), Some(min), None, None, None, Some(week)) =>
        allTrueMatch(secondMatch(sec)(cSec), minuteMatch(min)(cMin), weekMatch(week)(cWeek))
      case CronExpression(Some(sec), None, None, None, None, Some(week)) =>
        allTrueMatch(secondMatch(sec)(cSec), weekMatch(week)(cWeek))
      case CronExpression(Some(sec), Some(min), Some(hour), Some(day), Some(mon), None) =>
        allTrueMatch(secondMatch(sec)(cSec), minuteMatch(min)(cMin), hourMatch(hour)(cHour), dayMatch(day, cDays)(cDay), monthMatch(mon)(cMon))
      case CronExpression(Some(sec), Some(min), Some(hour), Some(day), None, None) =>
        allTrueMatch(secondMatch(sec)(cSec), minuteMatch(min)(cMin), hourMatch(hour)(cHour), dayMatch(day, cDays)(cDay))
      case CronExpression(Some(sec), Some(min), Some(hour), None, None, None) =>
        allTrueMatch(secondMatch(sec)(cSec), minuteMatch(min)(cMin), hourMatch(hour)(cHour))
      case CronExpression(Some(sec), Some(min), None, None, None, None) =>
        allTrueMatch(secondMatch(sec)(cSec), minuteMatch(min)(cMin))
      case CronExpression(Some(sec), None, None, None, None, None) =>
        allTrueMatch(secondMatch(sec)(cSec))
      case _ =>
        false
    }
  }

  private def unitMatch(unitRange: List[Option[List[Int]]], curr: Int): Boolean = {
    if (unitRange.find(_.isEmpty).nonEmpty) false
    else unitRange.map(_.get).flatMap(u => u).find(_ == curr).nonEmpty
  }

  private def secondMatch(unit: String)(implicit curr: Int): Boolean = {
    val c: List[Option[List[Int]]] = unitUnfold(unit, 0, 60)
    unitMatch(c, curr)
  }

  private def unitUnfold(unit: String, minLimit: Int, maxLimit: Int): List[Option[List[Int]]] = {
    unit.split(",").map(_ match {
      case intervalRegex(s, i) => intervalToValues(s.toInt, i.toInt, maxLimit)
      case rangeRegex(l, h) => rangeToValues(l.toInt, h.toInt, minLimit, maxLimit)
      case u: String => Some(List(u.toInt))
    }).toList
  }

  private def minuteMatch(unit: String)(implicit curr: Int): Boolean = {
    val c: List[Option[List[Int]]] = unitUnfold(unit, 0, 60)
    unitMatch(c, curr)
  }

  private def hourMatch(unit: String)(implicit curr: Int): Boolean = {
    val c: List[Option[List[Int]]] = unitUnfold(unit, 0, 24)
    unitMatch(c, curr)
  }

  private def dayMatch(unit: String, monthLength: Int)(implicit curr: Int): Boolean = {
    val c: List[Option[List[Int]]] = unitUnfold(unit, 1, monthLength)
    unitMatch(c, curr)
  }

  private def monthMatch(unit: String)(implicit curr: Int): Boolean = {
    val c: List[Option[List[Int]]] = unitUnfold(unit, 1, 12)
    unitMatch(c, curr)
  }

  private def weekMatch(unit: String)(implicit curr: Int): Boolean = {
    val c: List[Option[List[Int]]] = unitUnfold(unit, 1, 7)
    unitMatch(c, curr)
  }

  private def intervalToValues(start: Int, interval: Int, maxLimit: Int): Option[List[Int]] = {
    if (start < maxLimit && interval < maxLimit)
      Some((start.toInt to maxLimit by interval.toInt).toList)
    else None
  }

  private def rangeToValues(low: Int, high: Int, minLimit: Int, maxLimit: Int): Option[List[Int]] = {
    if (low < maxLimit && high.toInt < maxLimit && low.toInt < high.toInt)
      Some((low to high).toList)
    else if (low.toInt < maxLimit && high.toInt < maxLimit)
      Some((low.toInt until maxLimit).toList ::: (minLimit to high.toInt).toList)
    else None
  }

  private def allTrueMatch(t: Boolean*): Boolean = {
    t.reduce(_ && _)
  }
}