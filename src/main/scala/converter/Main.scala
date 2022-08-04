package converter

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.DataOutputStream
import java.io.File
import java.io.DataInputStream
import java.io.EOFException
import scala.util.Try
import java.util.stream.IntStream
import scala.collection.mutable.Stack

object Converter {

  abstract class TraceEvent
  case class MemoryRead(address: Int) extends TraceEvent
  case class MemoryWrite(address: Int) extends TraceEvent
  case class PhaseStart(phase: TracePhase) extends TraceEvent
  case class PhaseEnd(phase: TracePhase) extends TraceEvent

  enum TracePhase:
    case CodeLoad extends TracePhase
    case CodeExecute extends TracePhase
    case GarbageMark extends TracePhase
    case GarbageSweep extends TracePhase

  def main(args: Array[String]): Unit = {
    if (args.length != 3) {
      throw Error("The input file and the two output files must be provided")
    }

    val input = args(0)
    val indexOutput = args(1)
    val phaseOutput = args(2)
    val bis = new DataInputStream(new BufferedInputStream(new FileInputStream(input)));

    val timeIndex = new BufferedOutputStream(new DataOutputStream(new FileOutputStream(new File(indexOutput))))
    val phaseIndex = new BufferedOutputStream(new DataOutputStream(new FileOutputStream(new File(phaseOutput))))

    val phases = Stack[(TracePhase, Int)]()
    
    var time = 0
    Stream.continually(Try(Seq(bis.readUnsignedByte(), bis.readUnsignedByte(), bis.readUnsignedByte()))).takeWhile(_.isSuccess).map(_.get).zipWithIndex.foreach { (v, i) =>
      val (event, newClock) = extractEvent(v.map(_.toShort))

      if newClock then {
        timeIndex.write((i & 0xFF000000) >>> 24)
        timeIndex.write((i & 0xFF0000) >>> 16)
        timeIndex.write((i & 0xFF00) >>> 8)
        timeIndex.write((i & 0xFF))
        time += 1
      }

      event match {
        case PhaseStart(phase) => phases.push((phase, time))
        case PhaseEnd(phase) => phases.pop() match {
          case (`phase`, t0) => {
            println(f"phase <$phase> ($t0, $time)")

            // type of phase
            phaseIndex.write(phase.ordinal)

            // beginning of phase
            phaseIndex.write((t0 & 0xFF000000) >>> 24)
            phaseIndex.write((t0 & 0xFF0000) >>> 16)
            phaseIndex.write((t0 & 0xFF00) >>> 8)
            phaseIndex.write((t0 & 0xFF))

            // end of phase
            phaseIndex.write((time & 0xFF000000) >>> 24)
            phaseIndex.write((time & 0xFF0000) >>> 16)
            phaseIndex.write((time & 0xFF00) >>> 8)
            phaseIndex.write((time & 0xFF))
          }
          case _ => throw Error("wrongly nested phases")
        }
        case _ =>
      }
    }

    if phases.size > 0 then throw Error(f"non empty phase stack ${phases.toString()}")

    timeIndex.close()
    bis.close()
    phaseIndex.close()

  }

  def extractEvent(s: Seq[Short]) = {
    val clockBit = ((s(0) & 0x80) >>> 7) == 1
    val argument = (s(1) << 8) | s(2)

    val event = s(0) & 0x7F match {
      case 0 => MemoryRead(argument)
      case 1 => MemoryWrite(argument)
      case 2 => PhaseStart(TracePhase.values(argument))
      case 3 => PhaseEnd(TracePhase.values(argument))
    }

    (event, clockBit)
  }
}
