package org.goldenport.sexpr.eval

import scalaz._, Scalaz._
import org.goldenport.context.Showable
import org.goldenport.incident.{Incident => LibIncident}

/*
 * @since   Apr. 24, 2022
 * @version Apr. 24, 2022
 * @author  ASAMI, Tomoharu
 */
case class IncidentSequence( // See org.goldenport.context.Incidents
  incidents: Vector[LibIncident] = Vector.empty
) extends Showable {
  def print = incidents.toList match {
    case Nil => "N/A"
    case x :: Nil => x.print
    case xs => xs.map(_.print).mkString("(", " ", ")")
  }

  def +(rhs: IncidentSequence): IncidentSequence = copy(incidents ++ rhs.incidents)
  def +(ps: Seq[LibIncident]): IncidentSequence = copy(incidents ++ ps)
  def +(p: Option[LibIncident]): IncidentSequence = copy(incidents ++ p.toVector)
}

object IncidentSequence {
  val empty = IncidentSequence()

  implicit object IncidentSequenceMonoid extends Monoid[IncidentSequence] {
    def zero = empty
    def append(lhs: IncidentSequence, rhs: => IncidentSequence) = lhs + rhs
  }

  def apply(ps: Seq[LibIncident]): IncidentSequence = IncidentSequence(ps.toVector)
  def apply(p: Option[LibIncident]): IncidentSequence = IncidentSequence(p.toVector)
  def apply(p: LibIncident): IncidentSequence = IncidentSequence(Vector(p))
}
