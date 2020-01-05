package common

import java.util.Calendar

object CalendarOps extends EsoObj{
  def getCal(t: Long): Calendar = {
    val cal = Calendar.getInstance
    cal.setTimeInMillis(t)
    cal}
}
